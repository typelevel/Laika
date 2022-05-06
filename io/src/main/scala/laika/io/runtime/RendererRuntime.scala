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

import java.io.{File, IOException}
import cats.effect.{Async, Sync}
import cats.implicits._
import fs2.io.file.Files
import laika.api.Renderer
import laika.ast.Path.Root
import laika.ast._
import laika.config.{ConfigError, ConfigException}
import laika.io.api.{BinaryTreeRenderer, TreeRenderer}
import laika.io.model._
import laika.io.runtime.TreeResultBuilder.{ParserResult, StyleResult, TemplateResult}
import laika.parse.markup.DocumentParser.InvalidDocuments
import laika.rewrite.nav._
import laika.rewrite.{DefaultTemplatePath, TemplateContext, TemplateRewriter}

/** Internal runtime for renderer operations, for text and binary output as well
  * as parallel and sequential execution. 
  *
  *  @author Jens Halm
  */
object RendererRuntime {

  private[laika] case class RenderConfig ()

  /** Process the specified render operation for an entire input tree and a character output format.
    */
  def run[F[_]: Async: Batch] (op: TreeRenderer.Op[F]): F[RenderedTreeRoot[F]] = 
    run(op, op.theme.inputs, TemplateContext(op.renderer.format.fileSuffix, op.renderer.format.description.toLowerCase))

  private def run[F[_]: Async: Batch] (op: TreeRenderer.Op[F], themeInputs: InputTree[F], context: TemplateContext): F[RenderedTreeRoot[F]] = {  
    
    def validatePaths (staticDocs: Seq[BinaryInput[F]]): F[Unit] = {
      val paths = op.input.allDocuments.map(_.path) ++ staticDocs.map(_.path)
      val duplicates = paths.groupBy(identity).values.collect {
        case p if p.size > 1 => DuplicatePath(p.head)
      }
      if (duplicates.isEmpty) Sync[F].unit
      else Sync[F].raiseError(RendererErrors(duplicates.toSeq.sortBy(_.path.toString)))
    }

    val fileSuffix = op.renderer.format.fileSuffix

    type RenderResult = Either[BinaryInput[F], RenderedDocument]
    case class RenderOps (mkDirOps: Seq[F[Unit]], renderOps: Seq[F[RenderResult]])
    
    def file (rootDir: File, path: Path): File = new File(rootDir, path.toString.drop(1))

    def filterStaticDocuments (staticDocs: Seq[BinaryInput[F]],
                               root: DocumentTreeRoot,
                               lookup: TargetLookup,
                               config: TranslatorConfig): F[Seq[BinaryInput[F]]] = {

      /* This method needs to use the original root before the templates were applied as that step removes subtrees
         where the target format does not match, which also removes the tree config which is needed in this impl. */
      
      Sync[F].fromEither(RootCursor(root).map { cursor =>
        val renderUnversioned = config.versions.fold(true)(_.renderUnversioned)
        
        staticDocs.filter { doc =>
          val treeConfig = cursor.treeConfig(doc.path.parent)
          doc.formats.contains(context.finalFormat) &&
            treeConfig
              .get[TargetFormats]
              .getOrElse(TargetFormats.All)
              .contains(context.finalFormat) &&
            (renderUnversioned || lookup.isVersioned(treeConfig))
        }
      }.leftMap(e => RendererErrors(Seq(ConfigException(e)))))
    }
    
    def createPathTranslator (config: TranslatorConfig, refPath: Path, lookup: Path => Option[TranslatorSpec]): PathTranslator =
      ConfigurablePathTranslator(config, fileSuffix, context.finalFormat, refPath, lookup)
    
    def renderDocuments(finalRoot: DocumentTreeRoot, 
                        styles: StyleDeclarationSet, 
                        lookup: TargetLookup,
                        translatorConfig: TranslatorConfig)(output: Path => TextOutput[F]): Seq[F[RenderResult]] = {
      val renderUnversioned = translatorConfig.versions.fold(true)(_.renderUnversioned)
      finalRoot.allDocuments
        .filter(doc => doc.targetFormats.contains(context.finalFormat) &&
          (renderUnversioned || lookup.isVersioned(doc.config)))
        .map { document =>
          val renderer = Renderer.of(op.renderer.format).withConfig(op.config).build
          val pathTranslator = createPathTranslator(translatorConfig, document.path, lookup)
          val outputPath = pathTranslator.translate(document.path)
          val renderResult = renderer.render(document.content, outputPath, pathTranslator, styles)
          output(outputPath).writer(renderResult).as {
            val result = RenderedDocument(outputPath, document.title, document.sections, renderResult, document.config)
            Right(result): RenderResult
          }
        }
    }
    
    def copyDocuments (docs: Seq[BinaryInput[F]], dir: Option[File], pathTranslator: Path => Path): Seq[F[RenderResult]] = docs.map { doc =>
      val translatedDoc = doc.copy(path = pathTranslator(doc.path))
      val result: RenderResult = Left(translatedDoc)
      dir.map(file(_, translatedDoc.path)) match {
        case Some(outFile) if !doc.sourceFile.contains(outFile) =>
          val out = Files[F].writeAll(fs2.io.file.Path.fromNioPath(outFile.toPath))
          doc.input.through(out).compile.drain.as(result)
        case _ =>
          Sync[F].pure(result)
      }
    }
    
    def renderOps (finalRoot: DocumentTreeRoot, 
                   lookup: TargetLookup,
                   translatorConfig: TranslatorConfig,
                   staticDocs: Seq[BinaryInput[F]]): RenderOps = {
      
      val styles =  finalRoot.styles(fileSuffix) ++ getThemeStyles(themeInputs.parsedResults)
      val pathTranslator = createPathTranslator(translatorConfig, Root / "dummy", lookup).translate(_:Path)

      def createDirectory (file: File): F[Unit] = Files[F].createDirectories(fs2.io.file.Path.fromNioPath(file.toPath))
      
      op.output match {
        case StringTreeOutput => 
          val renderOps = renderDocuments(finalRoot, styles, lookup, translatorConfig)(p => TextOutput.noOp(p))
          val copyOps = copyDocuments(staticDocs, None, pathTranslator)
          RenderOps(Nil, renderOps ++ copyOps)
        case DirectoryOutput(dir, codec) =>
          val renderOps = renderDocuments(finalRoot, styles, lookup, translatorConfig)(p => TextOutput.forFile(p, file(dir, p), codec))
          val copyOps = copyDocuments(staticDocs, Some(dir), pathTranslator)
          val mkDirOps = (finalRoot.allDocuments.map(_.path) ++ staticDocs.map(_.path))
            .map(pathTranslator(_).parent)
            .distinct
            .map(p => createDirectory(file(dir, p)))
          RenderOps(mkDirOps, renderOps ++ copyOps)
      }
    }
    
    def processBatch (finalRoot: DocumentTreeRoot, ops: Seq[F[RenderResult]]): F[RenderedTreeRoot[F]] =

      Batch[F].execute(ops.toVector).map { results =>

        val titleName = TitleDocumentConfig.outputName(finalRoot.config)
        val (staticDocs, renderedDocs) = results.separate
        val coverDoc = renderedDocs.collectFirst {
          case doc if doc.path.parent == Root && doc.path.basename == "cover" => doc
        }

        def buildNode (path: Path, content: Seq[RenderContent]): RenderedTree = {
          val title = finalRoot.tree.selectSubtree(path.relative).flatMap(_.title)
          val titleDoc = content.collectFirst {
            case doc: RenderedDocument if titleName.contains(doc.path.basename) => doc
          }
          RenderedTree(path, title, content.filterNot(doc => titleDoc.exists(_.path == doc.path)), titleDoc)
        }

        val resultRoot = TreeBuilder.build(renderedDocs.filterNot(res => coverDoc.exists(_.path == res.path)), buildNode)
        val template = finalRoot.tree.getDefaultTemplate(fileSuffix).fold(TemplateRoot.fallback)(_.content)
  
        RenderedTreeRoot[F](resultRoot, template, finalRoot.config, finalRoot.styles(fileSuffix), coverDoc, staticDocs)
      }

    def applyTemplate (root: DocumentTreeRoot): Either[Throwable, DocumentTreeRoot] = {

      val treeWithTpl: DocumentTree = 
        if (root.tree.getDefaultTemplate(context.templateSuffix).isEmpty)
          root.tree.withDefaultTemplate(getDefaultTemplate(themeInputs, context.templateSuffix), context.templateSuffix)
        else 
          root.tree
      
      mapError(TemplateRewriter.applyTemplates(root.copy(tree = treeWithTpl), context))
        .flatMap(root => InvalidDocuments.from(root, op.config.failOnMessages).toLeft(root))
    }
    
    def getThemeStyles(themeInputs: Seq[ParserResult]): StyleDeclarationSet = themeInputs.collect {
      case StyleResult (doc, format, _) if format == op.renderer.format.fileSuffix => doc
    }.reduceLeftOption(_ ++ _).getOrElse(StyleDeclarationSet.empty)
    
    def generateVersionInfo (lookup: TargetLookup, config: TranslatorConfig, staticDocs: Seq[BinaryInput[F]]): F[Option[BinaryInput[F]]] = {
      (config.versions, context.finalFormat) match {
        case (Some(versions), "html") if versions.renderUnversioned =>
          VersionedLinkTargets
            .gatherTargets[F](versions, staticDocs)
            .map { existing =>
              val pathTranslator = createPathTranslator(config.copy(versions = None), Root / "dummy", lookup)
              val targets = VersionedLinkTargets.groupLinkTargets(versions, lookup.versionedDocuments.map(pathTranslator.translate), existing)
              Some(BinaryInput.fromString[F](VersionInfoGenerator.path, VersionInfoGenerator.generate(versions, targets), TargetFormats.Selected("html")))
            }
        case _ =>
          Sync[F].pure(None)
      }
    }
    
    def replaceVersionInfo (vInfo: Option[BinaryInput[F]])(staticDocs: Seq[BinaryInput[F]]): Seq[BinaryInput[F]] =
      staticDocs.filterNot(_.path == VersionInfoGenerator.path) ++ vInfo.toSeq
    
    def mapError[A] (result: Either[ConfigError, A]): Either[RendererErrors, A] =
      result.leftMap(e => RendererErrors(Seq(ConfigException(e))))
    
    val staticPaths = op.staticDocuments.map(_.path).toSet
    val staticDocs = op.staticDocuments ++ themeInputs.binaryInputs.filterNot(i => staticPaths.contains(i.path))
    val tree = ParsedTree(op.input, staticDocs)
    
    for {
      mappedTree <- op.theme.treeProcessor(op.renderer.format).run(tree)
      finalRoot  <- Sync[F].fromEither(applyTemplate(mappedTree.root))
      tConfig    <- Sync[F].fromEither(mapError(TranslatorConfig.readFrom(finalRoot.config)))
      lookup     <- Sync[F].fromEither(mapError(RootCursor(finalRoot, Some(context.finalFormat)))).map(new TargetLookup(_))
      vInfo      <- generateVersionInfo(lookup, tConfig, mappedTree.staticDocuments)
      static     <- filterStaticDocuments(mappedTree.staticDocuments, mappedTree.root, lookup, tConfig).map(replaceVersionInfo(vInfo))
      _          <- validatePaths(static)
      ops        =  renderOps(finalRoot, lookup, tConfig, static)
      _          <- ops.mkDirOps.toVector.sequence
      res        <- processBatch(finalRoot, ops.renderOps)
    } yield res
  }

  private def getDefaultTemplate[F[_]: Sync] (themeInputs: InputTree[F], suffix: String): TemplateRoot = 
    themeInputs.parsedResults.collectFirst {
      case TemplateResult(doc, _) if doc.path == DefaultTemplatePath.forSuffix(suffix) => doc.content
    }.getOrElse(TemplateRoot.fallback)

  /** Process the specified render operation for an entire input tree and a binary output format.
    */
  def run[F[_]: Async: Batch] (op: BinaryTreeRenderer.Op[F]): F[Unit] = {
    val context = TemplateContext(op.renderer.interimRenderer.format.fileSuffix, op.renderer.description.toLowerCase)
    val template = op.input.tree.getDefaultTemplate(context.templateSuffix)
                     .fold(getDefaultTemplate(op.theme.inputs, context.templateSuffix))(_.content)
    for {
      preparedTree <- Async[F].fromEither(op.renderer.prepareTree(op.input))
      renderedTree <- run(TreeRenderer.Op[F](op.renderer.interimRenderer, op.theme, preparedTree, StringTreeOutput, op.staticDocuments), op.theme.inputs, context)
      finalTree    =  renderedTree.copy[F](defaultTemplate = template)
      _            <- op.renderer.postProcessor.process(finalTree, op.output, op.config)
    } yield ()
  }

  // TODO - unify with ParserErrors (as TransformationErrors)
  case class DuplicatePath (path: Path, filePaths: Set[String] = Set.empty) extends
    RuntimeException(s"Duplicate path: $path ${filePathMessage(filePaths)}")

  case class RendererErrors (errors: Seq[Throwable]) extends
    RuntimeException(s"Multiple errors during rendering: ${errors.map(_.getMessage).mkString(", ")}")

  private def filePathMessage (filePaths: Set[String]): String =
    if (filePaths.isEmpty) "(no matching file paths)"
    else s"with matching file paths: ${filePaths.mkString(", ")}"

}
