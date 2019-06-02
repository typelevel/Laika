package laika.execute

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
object RenderExecutor {

  def execute[F[_]: Async] (op: SequentialRenderer.Op[F], styles: Option[StyleDeclarationSet]): F[Unit] = {

    def write (result: String): F[Unit] = ???
    
    val rendered = op.renderer.render(op.input, op.path)
    
    write(rendered)
  }
  
//  def execute[FMT] (op: Render.MergeOp[FMT]): Done = {
//    val template = op.config.themeFor(op.processor.format).defaultTemplateOrFallback // TODO - 0.12 - look for templates in root tree
//    val preparedTree = op.processor.prepareTree(op.tree)
//    val renderedTree  = execute(Render.TreeOp(op.processor.format, op.config, preparedTree, StringTreeOutput))
//    op.processor.process(renderedTree, op.output)
//    Done
//  }

  def execute[F[_]: Async] (op: ParallelRenderer.Op[F]): RenderedTreeRoot = {

    type Operation = () => RenderContent
    
    val fileSuffix: String = op.renderer.format.fileSuffix

    def outputPath (path: Path): Path = path.withSuffix(fileSuffix)
    
    def textOutputFor (path: Path): F[TextOutput] = op.output flatMap {
      case StringTreeOutput => Async[F].pure(StringOutput(new mutable.StringBuilder, outputPath(path))) // TODO - 0.12 - temporary solution
      case DirectoryOutput(dir, codec) => Async[F].pure(TextFileOutput(new File(dir, outputPath(path).toString.drop(1)), outputPath(path), codec))
    }
//    def binaryOutputFor (path: Path): Seq[BinaryOutput] = op.output match {
//      case StringTreeOutput => Nil
//      case DirectoryOutput(dir, codec) => Seq(BinaryFileOutput(new File(dir, path.toString.drop(1)), path))
//    }

    def renderDocument (document: Document): Operation = {
      val textOp = SequentialRenderer.Op(op.renderer, document.content, document.path, textOutputFor(document.path))
      () => RenderedDocument(outputPath(document.path), document.title, document.sections, "TODO" /*execute(textOp, Some(styles))*/)
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
    
    val finalRoot = op.renderer.applyTheme(op.input)
    
    val operations = finalRoot.allDocuments.map(renderDocument) /* ++ op.tree.staticDocuments.flatMap(copy) */  // TODO - 0.12 - handle static docs

    val results = BatchExecutor.execute(operations, 1, 1) // TODO - 0.12 - add parallelism option to builder
    
    def buildNode (path: Path, content: Seq[RenderContent], subTrees: Seq[RenderedTree]): RenderedTree = 
      RenderedTree(path, finalRoot.tree.selectSubtree(path.relativeTo(Root)).fold(Seq.empty[Span])(_.title), content ++ subTrees) // TODO - 0.12 - handle title document
    
    val resultRoot = TreeBuilder.build(results, buildNode)
    
    val template = finalRoot.tree.getDefaultTemplate(fileSuffix).fold(TemplateRoot.fallback)(_.content)

    RenderedTreeRoot(resultRoot, template, finalRoot.config) // TODO - 0.12 - handle cover document
  }

}
