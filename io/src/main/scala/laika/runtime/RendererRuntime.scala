package laika.runtime

import java.io.File

import cats.effect.Async
import cats.implicits._
import laika.ast.Path.Root
import laika.ast._
import laika.io.Parallel.ParallelRenderer
import laika.io.Sequential.SequentialRenderer
import laika.io.{TextOutput, _}

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
  def run[F[_]: Async: Runtime] (op: SequentialRenderer.Op[F], styles: Option[StyleDeclarationSet] = None): F[String] = {

    val renderResult = styles.fold(op.renderer.render(op.input, op.path)){ st => 
      op.renderer.render(op.input, op.path, st) 
    }
    
    for {
      output <- op.output
      _      <- OutputRuntime.write(renderResult, output)
    } yield renderResult
  }

  /** Process the specified render operation for an entire input tree and 
    * a character output format.
    */
  def run[F[_]: Async: Runtime] (op: ParallelRenderer.Op[F]): F[RenderedTreeRoot] = {

    val fileSuffix = op.renderer.format.fileSuffix
    val finalRoot = op.renderer.applyTheme(op.input)
    val styles = finalRoot.styles(fileSuffix)
    
    case class RenderOps (mkDirOps: Seq[F[Unit]], renderOps: Seq[F[RenderResult]])
    
    def file (rootDir: File, path: Path): File = new File(rootDir, path.toString.drop(1))

    def renderDocuments (output: Path => TextOutput): Seq[F[RenderResult]] = finalRoot.allDocuments.map { document =>
      val outputPath = document.path.withSuffix(fileSuffix)
      val textOp = SequentialRenderer.Op(op.renderer, document.content, outputPath, Async[F].pure(output(outputPath)))
      run(textOp, Some(styles)).map { res =>
        Right(RenderedDocument(outputPath, document.title, document.sections, res)): RenderResult
      }
    }
    
    def copyDocuments (docs: Seq[BinaryInput], dir: File): Seq[F[RenderResult]] = docs.map { in =>
      val out = BinaryFileOutput(file(dir, in.path), in.path)
      CopyRuntime.copy(in, out).as(Left(CopiedDocument(in.path)): RenderResult)
    }
    
    def renderOps (staticDocs: Seq[BinaryInput]): F[RenderOps] = op.output.map {
      case StringTreeOutput => RenderOps(Nil, renderDocuments(p => StringOutput(p)))
      case DirectoryOutput(dir, codec) => 
        val renderOps = renderDocuments(p => TextFileOutput(file(dir, p), p, codec))
        val toCopy = filterOutput(staticDocs, dir.getAbsolutePath)
        val copyOps = copyDocuments(toCopy, dir)
        val directories = (finalRoot.allDocuments.map(_.path.parent) ++ toCopy.map(_.path.parent)).distinct
          .map(p => OutputRuntime.createDirectory(file(dir, p)))
        RenderOps(directories, renderOps ++ copyOps)
    }
    
    def filterOutput (staticDocs: Seq[BinaryInput], outPath: String): Seq[BinaryInput] = {
      op.input.sourcePaths.collectFirst { 
        case inPath if outPath.startsWith(inPath) => Path(outPath.drop(inPath.length)) 
      }.fold(staticDocs) { nestedOut => staticDocs.filterNot(_.path.components.startsWith(nestedOut.components)) }
    }
    
    def processBatch (ops: Seq[F[RenderResult]], staticDocs: Seq[BinaryInput]): F[RenderedTreeRoot] =

      implicitly[Runtime[F]].runParallel(ops.toVector).map { results =>
        val renderedDocs = results.collect { case Right(doc) => doc }
        val coverDoc = renderedDocs.collectFirst {
          case doc if doc.path.parent == Root && doc.path.basename == "cover" => doc
        }

        def buildNode (path: Path, content: Seq[RenderContent]): RenderedTree = {
          val title = finalRoot.tree.selectSubtree(path.relativeTo(Root)).fold(Seq.empty[Span])(_.title)
          val titleDoc = content.collectFirst {
            case doc: RenderedDocument if doc.path.basename == "title" => doc
          }
          RenderedTree(path, title, content.filterNot(doc => titleDoc.exists(_.path == doc.path)), titleDoc)
        }

        val resultRoot = TreeBuilder.build(renderedDocs.filterNot(res => coverDoc.exists(_.path == res.path)), buildNode)
        val template = finalRoot.tree.getDefaultTemplate(fileSuffix).fold(TemplateRoot.fallback)(_.content)
  
        RenderedTreeRoot(resultRoot, template, finalRoot.config, coverDoc, staticDocs, finalRoot.sourcePaths)
      }

    for {
      static <- op.staticDocuments
      ops    <- renderOps(static)
      _      <- ops.mkDirOps.toVector.sequence
      res    <- processBatch(ops.renderOps, static)
    } yield res
  }

  /** Process the specified render operation for a single input document and 
    * a binary output format.
    */
  def run[F[_]: Async: Runtime] (op: binary.SequentialRenderer.Op[F]): F[Unit] = {
    val root = DocumentTreeRoot(DocumentTree(Root, Seq(Document(Root / "input", RootElement(Seq(SpanSequence(Seq(TemplateElement(op.input)))))))))
    val parOp = binary.ParallelRenderer.Op(op.renderer, root, op.output, Async[F].pure[Seq[BinaryInput]](Nil))
    run(parOp)
  }

  /** Process the specified render operation for an entire input tree and 
    * a binary output format.
    */
  def run[F[_]: Async: Runtime] (op: binary.ParallelRenderer.Op[F]): F[Unit] = {
    val template = op.renderer.interimRenderer.templateFor(op.input)
    val preparedTree = op.renderer.prepareTree(op.input)
    for {
      out          <- op.output
      renderedTree <- run(ParallelRenderer.Op[F](op.renderer.interimRenderer, preparedTree, Async[F].pure(StringTreeOutput), Async[F].pure(Nil)))
      _            <- op.renderer.postProcessor.process(renderedTree.copy(defaultTemplate = template), out)
    } yield ()
      
  }

}
