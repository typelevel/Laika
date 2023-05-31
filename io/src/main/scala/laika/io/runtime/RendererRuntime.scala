/*
 * Copyright 2012-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package laika.io.runtime

import cats.effect.{ Async, Sync }
import cats.implicits._
import fs2.io.file.Files
import laika.api.Renderer
import laika.ast.Path.Root
import laika.ast._
import laika.config.Config.ConfigResult
import laika.config.{ ConfigError, ConfigException, LaikaKeys, ValidationError }
import laika.io.api.{ BinaryTreeRenderer, TreeRenderer }
import laika.io.model._
import laika.io.runtime.TreeResultBuilder.{ ParserResult, StyleResult, TemplateResult }
import laika.parse.markup.DocumentParser.InvalidDocuments
import laika.rewrite.nav._
import laika.rewrite.{ DefaultTemplatePath, OutputContext, Versions }

/** Internal runtime for renderer operations, for text and binary output as well
  * as parallel and sequential execution.
  *
  *  @author Jens Halm
  */
object RendererRuntime {

  private[laika] case class RenderConfig()

  /** Process the specified render operation for an entire input tree and a character output format.
    */
  def run[F[_]: Async: Batch](op: TreeRenderer.Op[F]): F[RenderedTreeRoot[F]] =
    run(op, op.theme.inputs, OutputContext(op.renderer.format))

  private def run[F[_]: Async: Batch](
      op: TreeRenderer.Op[F],
      themeInputs: InputTree[F],
      context: OutputContext
  ): F[RenderedTreeRoot[F]] = {

    def validatePaths(staticDocs: Seq[BinaryInput[F]]): F[Unit] = {
      val paths      = op.input.allDocuments.map(_.path) ++ staticDocs.map(_.path)
      val duplicates = paths.groupBy(identity).values.collect {
        case p if p.size > 1 => DuplicatePath(p.head)
      }
      if (duplicates.isEmpty) Sync[F].unit
      else Sync[F].raiseError(RendererErrors(duplicates.toSeq.sortBy(_.path.toString)))
    }

    val fileSuffix = op.renderer.format.fileSuffix

    type RenderResult = Either[BinaryInput[F], RenderedDocument]
    case class RenderOps(mkDirOps: Seq[F[Unit]], renderOps: Seq[F[RenderResult]])

    def file(rootDir: FilePath, path: Path): FilePath = rootDir / path.relative

    def filterStaticDocuments(
        staticDocs: Seq[BinaryInput[F]],
        root: DocumentTreeRoot,
        pathTranslator: PathTranslator,
        versions: Option[Versions]
    ): F[Seq[BinaryInput[F]]] = {

      /* This method needs to use the original root before the templates were applied as that step removes subtrees
         where the target format does not match, which also removes the tree config which is needed in this impl. */

      Sync[F].fromEither(RootCursor(root).map { cursor =>
        val renderUnversioned = versions.fold(true)(_.renderUnversioned)

        staticDocs.filter { doc =>
          val treeConfig = cursor.treeConfig(doc.path.parent)
          doc.formats.contains(context.formatSelector) &&
          treeConfig
            .get[TargetFormats]
            .getOrElse(TargetFormats.All)
            .contains(context.formatSelector) &&
          (renderUnversioned || pathTranslator.getAttributes(doc.path).exists(
            _.isVersioned
          )) // TODO - extract
        }
      }.leftMap(e => RendererErrors(Seq(ConfigException(e)))))
    }

    def renderDocuments(
        finalRoot: DocumentTreeRoot,
        pathTranslator: PathTranslator,
        versions: Option[Versions],
        styles: StyleDeclarationSet
    )(output: Path => TextOutput[F]): Seq[F[RenderResult]] = {
      val renderUnversioned = versions.fold(true)(_.renderUnversioned)
      finalRoot.allDocuments
        .filter(doc =>
          doc.targetFormats.contains(context.formatSelector) &&
          (renderUnversioned || pathTranslator.getAttributes(doc.path).exists(_.isVersioned))
        ) // TODO - extract
        .map { document =>
          val renderer          = Renderer.of(op.renderer.format).withConfig(op.config).build
          val docPathTranslator = pathTranslator.forReferencePath(document.path)
          val outputPath        = docPathTranslator.translate(document.path)
          for {
            renderResult <- Async[F].fromEither(
              renderer.render(document.content, outputPath, docPathTranslator, styles)
            )
            _            <- output(outputPath).writer(renderResult)
          } yield {
            val result = RenderedDocument(
              outputPath,
              document.title,
              document.sections,
              renderResult,
              document.config
            )
            Right(result): RenderResult
          }
        }
    }

    def copyDocuments(
        docs: Seq[BinaryInput[F]],
        dir: Option[FilePath],
        pathTranslator: Path => Path
    ): Seq[F[RenderResult]] = docs.map { doc =>
      val translatedDoc        = doc.copy(path = pathTranslator(doc.path))
      val result: RenderResult = Left(translatedDoc)
      dir.map(file(_, translatedDoc.path)) match {
        case Some(outFile) if !doc.sourceFile.contains(outFile) =>
          val out = Files.forAsync[F].writeAll(outFile.toFS2Path)
          doc.input.through(out).compile.drain.as(result)
        case _                                                  =>
          Sync[F].pure(result)
      }
    }

    def renderOps(
        finalRoot: DocumentTreeRoot,
        pathTranslator: PathTranslator,
        versions: Option[Versions],
        staticDocs: Seq[BinaryInput[F]]
    ): RenderOps = {

      val styles = finalRoot.styles(fileSuffix) ++ getThemeStyles(themeInputs.parsedResults)
      val pathTranslatorF = pathTranslator.translate(_: Path)

      def createDirectory(file: FilePath): F[Unit] =
        Files.forAsync[F].createDirectories(file.toFS2Path)

      op.output match {
        case StringTreeOutput            =>
          val renderOps =
            renderDocuments(finalRoot, pathTranslator, versions, styles)(p => TextOutput.noOp(p))
          val copyOps   = copyDocuments(staticDocs, None, pathTranslatorF)
          RenderOps(Nil, renderOps ++ copyOps)
        case DirectoryOutput(dir, codec) =>
          val renderOps = renderDocuments(finalRoot, pathTranslator, versions, styles)(p =>
            TextOutput.forFile(file(dir, p), p)(Async[F], codec)
          )
          val copyOps   = copyDocuments(staticDocs, Some(dir), pathTranslatorF)
          val mkDirOps  = (finalRoot.allDocuments.map(_.path) ++ staticDocs.map(_.path))
            .map(pathTranslatorF(_).parent)
            .distinct
            .map(p => createDirectory(file(dir, p)))
          RenderOps(mkDirOps, renderOps ++ copyOps)
      }
    }

    def processBatch(
        finalRoot: DocumentTreeRoot,
        ops: Seq[F[RenderResult]],
        pathTranslator: PathTranslator
    ): F[RenderedTreeRoot[F]] =
      Batch[F].execute(ops.toVector).map { results =>
        val titleName                  = TitleDocumentConfig.outputName(finalRoot.config)
        val (staticDocs, renderedDocs) = results.separate
        val coverDoc                   = renderedDocs.collectFirst {
          case doc if doc.path.parent == Root && doc.path.basename == "cover" => doc
        }

        def buildNode(path: Path, content: Seq[RenderContent]): RenderedTree = {
          val title    = finalRoot.tree.selectSubtree(path.relative).flatMap(_.title)
          val titleDoc = content.collectFirst {
            case doc: RenderedDocument if titleName.contains(doc.path.basename) => doc
          }
          RenderedTree(
            path,
            title,
            content.filterNot(doc => titleDoc.exists(_.path == doc.path)),
            titleDoc
          )
        }

        val resultRoot = TreeBuilder.build(
          renderedDocs.filterNot(res => coverDoc.exists(_.path == res.path)),
          buildNode
        )
        val template   =
          finalRoot.tree.getDefaultTemplate(fileSuffix).fold(TemplateRoot.fallback)(_.content)

        RenderedTreeRoot[F](
          resultRoot,
          template,
          finalRoot.config,
          context,
          pathTranslator,
          finalRoot.styles(fileSuffix),
          coverDoc,
          staticDocs
        )
      }

    def applyTemplate(root: DocumentTreeRoot): Either[Throwable, DocumentTreeRoot] = {

      val treeWithTpl: DocumentTree =
        if (root.tree.getDefaultTemplate(context.fileSuffix).isEmpty)
          root.tree.withDefaultTemplate(
            getDefaultTemplate(themeInputs, context.fileSuffix),
            context.fileSuffix
          )
        else
          root.tree
      val rootWithTpl               = root.copy(tree = treeWithTpl)
      val rules = op.config.rewriteRulesFor(rootWithTpl, RewritePhase.Render(context))
      mapError(rootWithTpl.applyTemplates(rules, context))
        .flatMap(root => InvalidDocuments.from(root, op.config.failOnMessages).toLeft(root))
    }

    def getThemeStyles(themeInputs: Seq[ParserResult]): StyleDeclarationSet = themeInputs.collect {
      case StyleResult(doc, format, _) if format == op.renderer.format.fileSuffix => doc
    }.reduceLeftOption(_ ++ _).getOrElse(StyleDeclarationSet.empty)

    def generateVersionInfo(
        finalRoot: DocumentTreeRoot,
        pathTranslator: PathTranslator,
        versions: Option[Versions],
        staticDocs: Seq[BinaryInput[F]]
    ): F[Option[BinaryInput[F]]] = {
      (versions, context.formatSelector) match {
        case (Some(versions), "html") if versions.renderUnversioned =>
          VersionedLinkTargets
            .gatherTargets[F](versions, staticDocs)
            .map { existing =>
              val versionedDocuments    = finalRoot.allDocuments.filter(
                _.config.get[Boolean](LaikaKeys.versioned).getOrElse(false)
              ).map(_.path)
              val unversionedTranslator = PathTranslator.ignoreVersions(pathTranslator)
              val targets               = VersionedLinkTargets.groupLinkTargets(
                versions,
                versionedDocuments.map(unversionedTranslator.translate),
                existing
              )
              val versionInfoString     = VersionInfoGenerator.generate(versions, targets)
              Some(
                BinaryInput.fromString[F](
                  versionInfoString,
                  VersionInfoGenerator.path,
                  TargetFormats.Selected("html")
                )
              )
            }
        case _                                                      =>
          Sync[F].pure(None)
      }
    }

    def replaceVersionInfo(vInfo: Option[BinaryInput[F]])(
        staticDocs: Seq[BinaryInput[F]]
    ): Seq[BinaryInput[F]] =
      staticDocs.filterNot(_.path == VersionInfoGenerator.path) ++ vInfo.toSeq

    def mapError[A](result: Either[ConfigError, A]): Either[RendererErrors, A] =
      result.leftMap(e => RendererErrors(Seq(ConfigException(e))))

    val staticPaths = op.staticDocuments.map(_.path).toSet
    val staticDocs  =
      op.staticDocuments ++ themeInputs.binaryInputs.filterNot(i => staticPaths.contains(i.path))
    val tree        = ParsedTree(op.input, staticDocs)

    for {
      mappedTree  <- op.theme.treeProcessor(op.renderer.format).run(tree)
      finalRoot   <- Sync[F].fromEither(applyTemplate(mappedTree.root))
      versions    <- Sync[F].fromEither(mapError(finalRoot.config.getOpt[Versions]))
      pTranslator <- Sync[F].fromEither(mapError(op.config.pathTranslatorFor(finalRoot, context)))
      vInfo  <- generateVersionInfo(finalRoot, pTranslator, versions, mappedTree.staticDocuments)
      static <- filterStaticDocuments(
        mappedTree.staticDocuments,
        mappedTree.root,
        pTranslator,
        versions
      ).map(replaceVersionInfo(vInfo))
      _      <- validatePaths(static)
      ops = renderOps(finalRoot, pTranslator, versions, static)
      _   <- ops.mkDirOps.toVector.sequence
      res <- processBatch(finalRoot, ops.renderOps, pTranslator)
    } yield res
  }

  private def getDefaultTemplate[F[_]: Sync](
      themeInputs: InputTree[F],
      suffix: String
  ): TemplateRoot =
    themeInputs.parsedResults.collectFirst {
      case TemplateResult(doc, _) if doc.path == DefaultTemplatePath.forSuffix(suffix) =>
        doc.content
    }.getOrElse(TemplateRoot.fallback)

  /** Process the specified render operation for an entire input tree and a binary output format.
    */
  def run[F[_]: Async: Batch](op: BinaryTreeRenderer.Op[F]): F[Unit] = {
    val context  = OutputContext(
      op.renderer.interimRenderer.format.fileSuffix,
      op.renderer.description.toLowerCase
    )
    val template = op.input.tree.getDefaultTemplate(context.fileSuffix)
      .fold(getDefaultTemplate(op.theme.inputs, context.fileSuffix))(_.content)
    for {
      preparedTree <- Async[F].fromEither(op.renderer.prepareTree(op.input))
      renderedTree <- run(
        TreeRenderer.Op[F](
          op.renderer.interimRenderer,
          op.theme,
          preparedTree,
          StringTreeOutput,
          op.staticDocuments
        ),
        op.theme.inputs,
        context
      )
      finalTree = renderedTree.copy[F](defaultTemplate = template)
      _ <- op.renderer.postProcessor.process(finalTree, op.output, op.config)
    } yield ()
  }

  // TODO - unify with ParserErrors (as TransformationErrors)
  case class DuplicatePath(path: Path, filePaths: Set[String] = Set.empty) extends RuntimeException(
        s"Duplicate path: $path ${filePathMessage(filePaths)}"
      )

  case class RendererErrors(errors: Seq[Throwable]) extends RuntimeException(
        s"Multiple errors during rendering: ${errors.map(_.getMessage).mkString(", ")}"
      )

  private def filePathMessage(filePaths: Set[String]): String =
    if (filePaths.isEmpty) "(no matching file paths)"
    else s"with matching file paths: ${filePaths.mkString(", ")}"

}
