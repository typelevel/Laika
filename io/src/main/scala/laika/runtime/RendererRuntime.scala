package laika.runtime

import java.io.File

import cats.effect.Async
import cats.implicits._
import laika.ast.Path.Root
import laika.ast._
import laika.io.Parallel.ParallelRenderer
import laika.io.Sequential.SequentialRenderer
import laika.io.{TextOutput, _}

/**
  *  @author Jens Halm
  */
object RendererRuntime {

  def run[F[_]: Async] (op: SequentialRenderer.Op[F], styles: Option[StyleDeclarationSet] = None): F[String] = {

    val renderResult = styles.fold(op.renderer.render(op.input, op.path)){ st => 
      op.renderer.render(op.input, op.path, st) 
    }
    
    for {
      output <- op.output
      _      <- OutputRuntime.write(renderResult, output)
    } yield renderResult
  }
  
  def run[F[_]: Async] (op: ParallelRenderer.Op[F]): F[RenderedTreeRoot] = {

    val fileSuffix = op.renderer.format.fileSuffix
    val finalRoot = op.renderer.applyTheme(op.input)
    val styles = finalRoot.styles(fileSuffix)
    
    case class RenderTasks (mkDirOps: Seq[F[Unit]], renderOps: Seq[F[RenderContent]])
    
    def file (rootDir: File, path: Path): File = new File(rootDir, path.toString.drop(1))

    // TODO - 0.12 - handle static docs
//    def binaryOutputFor (path: Path): Seq[BinaryOutput] = op.output match {
//      case StringTreeOutput => Nil
//      case DirectoryOutput(dir, codec) => Seq(BinaryFileOutput(new File(dir, path.toString.drop(1)), path))
//    }
    //    def copy (document: BinaryInput): Seq[Operation] = binaryOutputFor(document.path).map { out =>
    //      () => {
    //        IO.copy(document, out)
    //        CopiedDocument(document)
    //      }
    //    }

    def renderDocuments (output: Path => TextOutput): Seq[F[RenderContent]] = finalRoot.allDocuments.map { document =>
      println(document.path)
      val outputPath = document.path.withSuffix(fileSuffix)
      val textOp = SequentialRenderer.Op(op.renderer, document.content, outputPath, Async[F].pure(output(outputPath)))
      run(textOp, Some(styles)).map { res =>
        RenderedDocument(outputPath, document.title, document.sections, res): RenderContent
      }
    }
    
    val renderTasks: F[RenderTasks] = op.output.map {
      case StringTreeOutput => RenderTasks(Nil, renderDocuments(p => StringOutput(p)))
      case DirectoryOutput(dir, codec) => 
        val operations = renderDocuments(p => TextFileOutput(file(dir, p), p, codec))
        val directories = finalRoot.allDocuments.map(_.path.parent).distinct
          .map(p => OutputRuntime.createDirectory(file(dir, p)))
        RenderTasks(directories, operations)
    }
    
    def processBatch (ops: Seq[F[RenderContent]]): F[RenderedTreeRoot] =
      
      BatchRuntime.run(ops.toVector, 1, 1).map { results => // TODO - 0.12 - add parallelism option to builder

        def buildNode (path: Path, content: Seq[RenderContent], subTrees: Seq[RenderedTree]): RenderedTree =
          RenderedTree(path, finalRoot.tree.selectSubtree(path.relativeTo(Root)).fold(Seq.empty[Span])(_.title), content ++ subTrees) // TODO - 0.12 - handle title document
  
        val resultRoot = TreeBuilder.build(results, buildNode)
        val template = finalRoot.tree.getDefaultTemplate(fileSuffix).fold(TemplateRoot.fallback)(_.content)
  
        RenderedTreeRoot(resultRoot, template, finalRoot.config) // TODO - 0.12 - handle cover document
      }

      // TODO - 0.12 - resurrect check for output tree
//      def isOutputRoot (source: DocumentTree) = (source.sourcePaths.headOption, op.output) match {
//        case (Some(inPath), out: DirectoryOutput) => inPath == out.directory.getAbsolutePath
//        case _ => false
//      }

    for {
      tasks <- renderTasks
      _     <- tasks.mkDirOps.toVector.sequence
      res   <- processBatch(tasks.renderOps)
    } yield res
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
      _            <- op.renderer.postProcessor.process(renderedTree, out)
    } yield ()
      
  }

}
