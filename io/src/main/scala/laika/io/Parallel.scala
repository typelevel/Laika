package laika.io

import java.io.File

import cats.effect.Async
import laika.api.{MarkupParser, Renderer, Transformer}
import laika.api.builder.{OperationConfig, ParserBuilder, RendererBuilder, TransformerBuilder}
import laika.ast._

import scala.io.Codec

object Parallel {

  def apply (parser: ParserBuilder): ParallelParser.Builder                   = ParallelParser.Builder(parser.build)
  def apply (renderer: RendererBuilder[_]): ParallelRenderer.Builder          = ParallelRenderer.Builder(renderer.build)
  def apply (transformer: TransformerBuilder[_]): ParallelTransformer.Builder = ParallelTransformer.Builder(transformer.build)

  def apply (parser: MarkupParser): ParallelParser.Builder           = ParallelParser.Builder(parser)
  def apply (renderer: Renderer): ParallelRenderer.Builder           = ParallelRenderer.Builder(renderer)
  def apply (transformer: Transformer): ParallelTransformer.Builder  = ParallelTransformer.Builder(transformer)


  class ParallelParser[F[_]: Async] (parser: MarkupParser) extends ParallelInputOps[F] {

    type Result = ParallelParser.Op[F]

    val F: Async[F] = Async[F]

    val docType: TextDocumentType = DocumentType.Markup

    val config: OperationConfig = parser.config


    def fromInput (input: F[TreeInput]): ParallelParser.Op[F] = ParallelParser.Op(parser, input)

  }

  object ParallelParser {

    case class Builder (parser: MarkupParser) {

      def build[F[_]: Async]: ParallelParser[F] = new ParallelParser[F](parser)

    }

    case class Op[F[_]: Async] (parser: MarkupParser, input: F[TreeInput]) {

      val config: OperationConfig = parser.config
      
      def parse: F[DocumentTreeRoot] = ???

    }

  }

  class ParallelRenderer[F[_]: Async] (renderer: Renderer) {

    def from (input: DocumentTreeRoot): ParallelRenderer.OutputOps[F] = 
      ParallelRenderer.OutputOps(renderer, input)

  }

  object ParallelRenderer {

    case class Builder (renderer: Renderer) {

      def build[F[_]: Async]: ParallelRenderer[F] = new ParallelRenderer[F](renderer)

    }

    case class OutputOps[F[_]: Async] (renderer: Renderer, input: DocumentTreeRoot) extends ParallelTextOutputOps[F] {

      val F: Async[F] = Async[F]

      type Result = Op[F]

      def toOutput (output: F[TreeOutput]): Op[F] = Op[F](renderer, input, output)

    }

    case class Op[F[_]: Async] (renderer: Renderer, input: DocumentTreeRoot, output: F[TreeOutput]) {

      val config: OperationConfig = renderer.config
      
      def render: F[RenderedTreeRoot] = ???

    }

  }

  class ParallelTransformer[F[_]: Async] (transformer: Transformer) extends ParallelInputOps[F] {

    type Result = ParallelTransformer.OutputOps[F]

    val F: Async[F] = Async[F]

    val docType: TextDocumentType = DocumentType.Markup
    
    val config: OperationConfig = transformer.parser.config


    def fromInput (input: F[TreeInput]): ParallelTransformer.OutputOps[F] = ParallelTransformer.OutputOps(transformer, input)

  }

  object ParallelTransformer {

    case class Builder (transformer: Transformer) {

      def build[F[_]: Async]: ParallelTransformer[F] = new ParallelTransformer[F](transformer)

    }

    case class OutputOps[F[_]: Async] (transformer: Transformer, input: F[TreeInput]) extends ParallelTextOutputOps[F] {

      val F: Async[F] = Async[F]

      type Result = Op[F]

      def toOutput (output: F[TreeOutput]): Op[F] = Op[F](transformer, input, output)

    }

    case class Op[F[_]: Async] (transformer: Transformer, input: F[TreeInput], output: F[TreeOutput]) {

      def transform: F[RenderedTreeRoot] = ???

    }

  }
  
}

/** API for producing a result from processing various types of input trees.
  *
  * This is essentially a collection of shortcuts that allow any class
  * merging in this trait to define all input related operations in terms of the only
  * abstract method `fromInputTree`. Calling `fromDirectory("src")` for example
  * is only a convenient shortcut for calling `fromInputTree(InputTree.fromDirectory("src")`.
  *
  * @author Jens Halm
  */
trait ParallelInputOps[F[_]] {

  def F: Async[F]
  
  type FileFilter = File => Boolean

  /** The type of the result returned by all operations of this trait.
    */
  type Result

  /** The configuration to use for all input operations.
    */
  def config: OperationConfig

  /**  Returns the result obtained by parsing files from the
    *  specified directory and its subdirectories.
    *
    *  @param name the name of the directory to traverse
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectory (name: String)(implicit codec: Codec): Result =
    fromDirectory(new File(name), DirectoryInput.hiddenFileFilter)(codec)

  /**  Returns the result obtained by parsing files from the
    *  specified directory and its subdirectories.
    *
    *  @param name the name of the directory to traverse
    *  @param exclude the files to exclude from processing
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectory (name: String, exclude: FileFilter)(implicit codec: Codec): Result =
    fromDirectory(new File(name), exclude)(codec)

  /**  Returns the result obtained by parsing files from the
    *  specified directory and its subdirectories.
    *
    *  @param dir the root directory to traverse
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectory (dir: File)(implicit codec: Codec): Result =
    fromDirectory(dir, DirectoryInput.hiddenFileFilter)(codec)

  /**  Returns the result obtained by parsing files from the
    *  specified directory and its subdirectories.
    *
    *  @param dir the root directory to traverse
    *  @param exclude the files to exclude from processing
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectory (dir: File, exclude: FileFilter)(implicit codec: Codec): Result =
    fromDirectories(Seq(dir), exclude)(codec)

  /**  Returns the result obtained by parsing files from the
    *  specified directories and its subdirectories, merging them into
    *  a tree with a single root.
    *
    *  @param roots the root directories to traverse
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectories (roots: Seq[File])(implicit codec: Codec): Result =
    fromDirectories(roots, DirectoryInput.hiddenFileFilter)(codec)

  /**  Returns the result obtained by parsing files from the
    *  specified directories and its subdirectories, merging them into
    *  a tree with a single root.
    *
    *  @param roots the root directories to traverse
    *  @param exclude the files to exclude from processing
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectories (roots: Seq[File], exclude: FileFilter)(implicit codec: Codec): Result =
    fromInput(F.pure(DirectoryInput(roots, codec, config.docTypeMatcher, exclude)))

  /**  Returns the result obtained by parsing files from the
    *  current working directory.
    *
    *  @param exclude the files to exclude from processing
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromWorkingDirectory (exclude: FileFilter = DirectoryInput.hiddenFileFilter)(implicit codec: Codec): Result =
    fromDirectories(Seq(new File(System.getProperty("user.dir"))), exclude)

  /** Returns the result obtained by parsing files from the
    *  specified input tree.
    *
    *  @param input the input tree to process
    */
  def fromInput(input: F[TreeInput]): Result

}

/** Represents a tree of output destinations for recursive render operations.
  *  Various types of output can be specified to trigger the actual rendering.
  */
trait ParallelTextOutputOps[F[_]] {

  def F: Async[F]
  
  type Result

  /** Renders the document tree to the
    *  specified directory and its subdirectories.
    *  Required subdirectories which do not exist yet will be created.
    *
    *  @param name the name of the directory to write to
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def toDirectory (name: String)(implicit codec: Codec): Result = toDirectory(new File(name))

  /** Renders the document tree to the
    *  specified directory and its subdirectories.
    *  Required subdirectories which do not exist yet will be created.
    *
    *  @param dir the directory to write to
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def toDirectory (dir: File)(implicit codec: Codec): Result = toOutput(F.pure(DirectoryOutput(dir, codec)))

  /** Renders the document tree to the
    *  current working directory and its subdirectories.
    *  Required subdirectories which do not exist yet will be created.
    *
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def toDefaultDirectory (implicit codec: Codec): Result = toOutput(F.pure(DirectoryOutput(new File(System.getProperty("user.dir")), codec)))

  /** Renders the document tree to the specified output tree.
    */
  def toOutput (tree: F[TreeOutput]): Result

}
