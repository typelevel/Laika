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

import java.io.File

import cats.effect.Async
import cats.implicits._
import laika.api.Renderer
import laika.ast.Path.Root
import laika.ast._
import laika.config.{ConfigError, ConfigException}
import laika.io.binary
import laika.io.text.ParallelRenderer
import laika.io.text.SequentialRenderer
import laika.io.model._
import laika.io.runtime.TreeResultBuilder.{ParserResult, StyleResult, TemplateResult}
import laika.io.theme.Theme
import laika.parse.markup.DocumentParser.InvalidDocuments
import laika.rewrite.TemplateRewriter
import laika.rewrite.nav.{ConfigurablePathTranslator, PathTranslator, TitleDocumentConfig}

/** Internal runtime for renderer operations, for text and binary output as well
  * as parallel and sequential execution. 
  *
  *  @author Jens Halm
  */
object RendererRuntime {

  private case class CopiedDocument (path: Path)
  private type RenderResult = Either[CopiedDocument, RenderedDocument]

  /** Process the specified render operation for a single input document and 
    * a character output format.
    */
  def run[F[_]: Async: Runtime] (op: SequentialRenderer.Op[F]): F[String] = {
    val renderResult = op.renderer.render(op.input, op.path)
    OutputRuntime.write(renderResult, op.output).as(renderResult)
  }

  /** Process the specified render operation for a single input document and 
    * a character output format, using the specified path translator and styles.
    */
  def run[F[_]: Async: Runtime] (op: SequentialRenderer.Op[F],
                                 pathTranslator: PathTranslator,
                                 styles: StyleDeclarationSet): F[String] = {

    val renderResult = op.renderer.render(op.input, op.path, pathTranslator, styles)
    OutputRuntime.write(renderResult, op.output).as(renderResult)
  }

  /** Process the specified render operation for an entire input tree and 
    * a character output format.
    */
  def run[F[_]: Async: Runtime] (op: ParallelRenderer.Op[F]): F[RenderedTreeRoot[F]] = {
    
    def validatePaths (staticDocs: Seq[BinaryInput[F]]): F[Unit] = {
      val paths = op.input.allDocuments.map(_.path) ++ staticDocs.map(_.path)
      val duplicates = paths.groupBy(identity).values.collect {
        case p if p.size > 1 => DuplicatePath(p.head)
      }
      if (duplicates.isEmpty) Async[F].unit
      else Async[F].raiseError(RendererErrors(duplicates.toSeq.sortBy(_.path.toString)))
    }

    val fileSuffix = op.renderer.format.fileSuffix
    
    case class RenderOps (mkDirOps: Seq[F[Unit]], renderOps: Seq[F[RenderResult]])
    
    def file (rootDir: File, path: Path): File = new File(rootDir, path.toString.drop(1))

    def renderDocuments (finalRoot: DocumentTreeRoot, styles: StyleDeclarationSet)(output: Path => TextOutput[F]): Seq[F[RenderResult]] = finalRoot.allDocuments.map { document =>
      val pathTranslator = ConfigurablePathTranslator(finalRoot.config, fileSuffix)
      val renderer = Renderer.of(op.renderer.format).withConfig(op.config).build
      val outputPath = pathTranslator.translate(document.path)
      val textOp = SequentialRenderer.Op(renderer, document.content, outputPath, output(outputPath))
      run(textOp, pathTranslator, styles).map { res =>
        Right(RenderedDocument(outputPath, document.title, document.sections, res)): RenderResult
      }
    }
    
    def copyDocuments (docs: Seq[BinaryInput[F]], dir: File): Seq[F[RenderResult]] = docs.flatMap { doc =>
      val outFile = file(dir, doc.path)
      if (doc.sourceFile.contains(outFile)) None
      else {
        val out = OutputRuntime.binaryFileResource(outFile)
        Some(CopyRuntime.copy(doc.input, out).as(Left(CopiedDocument(doc.path)): RenderResult))
      }
    }
    
    def renderOps (finalRoot: DocumentTreeRoot, styles: StyleDeclarationSet, staticDocs: Seq[BinaryInput[F]]): RenderOps = op.output match {
      case StringTreeOutput => RenderOps(Nil, renderDocuments(finalRoot, styles)(p => TextOutput.forString(p)))
      case DirectoryOutput(dir, codec) => 
        val renderOps = renderDocuments(finalRoot, styles)(p => TextOutput.forFile(p, file(dir, p), codec))
        val toCopy = filterOutput(staticDocs, dir.getAbsolutePath)
        val copyOps = copyDocuments(toCopy, dir)
        val directories = (finalRoot.allDocuments.map(_.path.parent) ++ toCopy.map(_.path.parent)).distinct
          .map(p => OutputRuntime.createDirectory(file(dir, p)))
        RenderOps(directories, renderOps ++ copyOps)
    }
    
    def filterOutput (staticDocs: Seq[BinaryInput[F]], outPath: String): Seq[BinaryInput[F]] = {
      op.input.sourcePaths.collectFirst { 
        case inPath if outPath.startsWith(inPath) =>
          Root / RelativePath.parse(outPath.drop(inPath.length).stripPrefix("/")) 
      }.fold(staticDocs) { nestedOut => staticDocs.filterNot(_.path.isSubPath(nestedOut)) }
    }
    
    def processBatch (finalRoot: DocumentTreeRoot, ops: Seq[F[RenderResult]], staticDocs: Seq[BinaryInput[F]]): F[RenderedTreeRoot[F]] =

      Runtime[F].runParallel(ops.toVector).map { results =>

        val titleName = TitleDocumentConfig.outputName(finalRoot.config)
        val renderedDocs = results.collect { case Right(doc) => doc }
        val coverDoc = renderedDocs.collectFirst {
          case doc if doc.path.parent == Root && doc.path.basename == "cover" => doc
        }

        def buildNode (path: Path, content: Seq[RenderContent]): RenderedTree = {
          val title = finalRoot.tree.selectSubtree(path.relative).flatMap(_.title)
          val titleDoc = content.collectFirst {
            case doc: RenderedDocument if doc.path.basename == titleName => doc
          }
          RenderedTree(path, title, content.filterNot(doc => titleDoc.exists(_.path == doc.path)), titleDoc)
        }

        val resultRoot = TreeBuilder.build(renderedDocs.filterNot(res => coverDoc.exists(_.path == res.path)), buildNode)
        val template = finalRoot.tree.getDefaultTemplate(fileSuffix).fold(TemplateRoot.fallback)(_.content)
  
        RenderedTreeRoot[F](resultRoot, template, finalRoot.config, coverDoc, staticDocs, finalRoot.sourcePaths)
      }

    def applyTemplate (root: DocumentTreeRoot, themeInputs: Seq[ParserResult]): Either[ConfigError, DocumentTreeRoot] = {
      val suffix = op.renderer.format.fileSuffix

      val treeWithTpl: DocumentTree = root.tree.getDefaultTemplate(suffix).fold(
        root.tree.withDefaultTemplate(themeInputs.collectFirst { 
          case TemplateResult(doc, _) if doc.path == Root / s"default.template.$suffix" => doc.content 
        }.getOrElse(TemplateRoot.fallback), suffix)
      )(_ => root.tree)
      
      TemplateRewriter.applyTemplates(root.copy(tree = treeWithTpl), suffix)
    }
    
    def getThemeStyles(themeInputs: Seq[ParserResult]): StyleDeclarationSet = themeInputs.collect {
      case StyleResult (doc, format, _) if format == op.renderer.format.fileSuffix => doc
    }.reduceLeftOption(_ ++ _).getOrElse(StyleDeclarationSet.empty)
     
    for {
      themeInputs <- op.theme.inputs
      mappedTree  <- op.theme.treeTransformer.run(ParsedTree(op.input, op.staticDocuments ++ themeInputs.binaryInputs))
      finalRoot   <- Async[F].fromEither(applyTemplate(mappedTree.root, themeInputs.parsedResults)
                       .leftMap(e => RendererErrors(Seq(ConfigException(e))))
                      .flatMap(root => InvalidDocuments.from(root, op.config.failOnMessages).toLeft(root)))
      styles    = finalRoot.styles(fileSuffix) ++ getThemeStyles(themeInputs.parsedResults)
      static    = mappedTree.staticDocuments
      _         <- validatePaths(static)
      ops       =  renderOps(finalRoot, styles, static)
      _         <- ops.mkDirOps.toVector.sequence
      res       <- processBatch(finalRoot, ops.renderOps, static)
    } yield res
  }

  /** Process the specified render operation for a single input document and a binary output format.
    */
  def run[F[_]: Async: Runtime] (op: binary.SequentialRenderer.Op[F]): F[Unit] = {
    val root = DocumentTreeRoot(DocumentTree(Root, Seq(Document(Root / "input", RootElement(SpanSequence(TemplateElement(op.input)))))))
    val parOp = binary.ParallelRenderer.Op(op.renderer, Theme.default, root, op.output)
    run(parOp)
  }

  /** Process the specified render operation for an entire input tree and a binary output format.
    */
  def run[F[_]: Async: Runtime] (op: binary.ParallelRenderer.Op[F]): F[Unit] = {
    val template = op.input.tree
      .getDefaultTemplate(op.renderer.interimRenderer.format.fileSuffix)
      .fold(TemplateRoot.fallback)(_.content)
    for {
      preparedTree <- Async[F].fromEither(op.renderer.prepareTree(op.input))
      renderedTree <- run(ParallelRenderer.Op[F](op.renderer.interimRenderer, op.theme, preparedTree, StringTreeOutput))
      _            <- op.renderer.postProcessor.process(renderedTree.copy[F](defaultTemplate = template, staticDocuments = op.staticDocuments), op.output)
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
