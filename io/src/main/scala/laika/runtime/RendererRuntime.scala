package laika.runtime

import java.io.File

import cats.effect.Async
import cats.implicits._
import laika.ast.Path.Root
import laika.ast._
import laika.io.Parallel.ParallelRenderer
import laika.io.Sequential.SequentialRenderer
import laika.io._

import scala.collection.mutable

/**
  *  @author Jens Halm
  */
object RendererRuntime {

  def run[F[_]: Async] (op: SequentialRenderer.Op[F], styles: Option[StyleDeclarationSet] = None): F[String] = {

    val renderResult = op.renderer.render(op.input, op.path)
    
    for {
      output <- op.output
      _      <- OutputRuntime.write(renderResult, output)
    } yield renderResult
  }
  
  def run[F[_]: Async] (op: ParallelRenderer.Op[F]): F[RenderedTreeRoot] = {

    val fileSuffix = op.renderer.format.fileSuffix
    val finalRoot = op.renderer.applyTheme(op.input)
    val styles = finalRoot.styles(fileSuffix)
    
    def outputPath (path: Path): Path = path.withSuffix(fileSuffix)
    
    def textOutputFor (path: Path): F[TextOutput] = op.output map {
      case StringTreeOutput => StringOutput(new mutable.StringBuilder, outputPath(path)) // TODO - 0.12 - temporary solution
      case DirectoryOutput(dir, codec) => TextFileOutput(new File(dir, outputPath(path).toString.drop(1)), outputPath(path), codec)
    }
//    def binaryOutputFor (path: Path): Seq[BinaryOutput] = op.output match {
//      case StringTreeOutput => Nil
//      case DirectoryOutput(dir, codec) => Seq(BinaryFileOutput(new File(dir, path.toString.drop(1)), path))
//    }

    def renderDocument (document: Document): F[RenderContent] = {
      val textOp = SequentialRenderer.Op(op.renderer, document.content, document.path, textOutputFor(document.path))
      run(textOp, Some(styles)).map { res =>
        RenderedDocument(outputPath(document.path), document.title, document.sections, res)
      }
    }

//    def copy (document: BinaryInput): Seq[Operation] = binaryOutputFor(document.path).map { out =>
//      () => {
//        IO.copy(document, out)
//        CopiedDocument(document)
//      }
//    }

      // TODO - 0.12 - resurrect check for output tree
//      def isOutputRoot (source: DocumentTree) = (source.sourcePaths.headOption, op.output) match {
//        case (Some(inPath), out: DirectoryOutput) => inPath == out.directory.getAbsolutePath
//        case _ => false
//      }
    
    val operations = finalRoot.allDocuments.map(renderDocument) /* ++ op.tree.staticDocuments.flatMap(copy) */  // TODO - 0.12 - handle static docs

    BatchRuntime.run(operations.toVector, 1, 1).map { results => // TODO - 0.12 - add parallelism option to builder

      def buildNode (path: Path, content: Seq[RenderContent], subTrees: Seq[RenderedTree]): RenderedTree =
        RenderedTree(path, finalRoot.tree.selectSubtree(path.relativeTo(Root)).fold(Seq.empty[Span])(_.title), content ++ subTrees) // TODO - 0.12 - handle title document

      val resultRoot = TreeBuilder.build(results, buildNode)

      val template = finalRoot.tree.getDefaultTemplate(fileSuffix).fold(TemplateRoot.fallback)(_.content)

      RenderedTreeRoot(resultRoot, template, finalRoot.config) // TODO - 0.12 - handle cover document
    }
  }

  def run[F[_]: Async] (op: binary.SequentialRenderer.Op[F]): F[Unit] = { // TODO - 0.12 - when delegating to the parallel executor this will need a parallel instance
    val root = DocumentTreeRoot(DocumentTree(Root, Seq(Document(Root / "input", RootElement(Seq(SpanSequence(Seq(TemplateElement(op.input)))))))))
    val parOp = binary.ParallelRenderer.Op(op.renderer, root, op.output)
    run(parOp)
  }

  def run[F[_]: Async] (op: binary.ParallelRenderer.Op[F]): F[Unit] = {
    // TODO - 0.12 - why is the template no longer required here? - val template =
    val preparedTree = op.renderer.prepareTree(op.input)
    for {
      out          <- op.output
      renderedTree <- run(ParallelRenderer.Op[F](op.renderer.interimRenderer, preparedTree, Async[F].pure(StringTreeOutput)))
    } yield
      op.renderer.postProcessor.process(renderedTree, out)
  }

}
