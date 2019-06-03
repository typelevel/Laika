package laika.io

import java.io.File

import cats.effect.{Async, Sync}
import laika.api.builder._
import laika.api.{MarkupParser, Renderer, Transformer}
import laika.ast.{Document, DocumentType, Element, Path, TextDocumentType}
import laika.factory.BinaryPostProcessor
import laika.runtime.{ParserRuntime, RendererRuntime, TransformerRuntime}

import scala.io.Codec

object Sequential {

  def apply (parser: ParserBuilder): SequentialParser.Builder                   = SequentialParser.Builder(parser.build)
  def apply (renderer: RendererBuilder[_]): SequentialRenderer.Builder          = SequentialRenderer.Builder(renderer.build)
  def apply (transformer: TransformerBuilder[_]): SequentialTransformer.Builder = SequentialTransformer.Builder(transformer.build)

  def apply (renderer: TwoPhaseRendererBuilder[_, BinaryPostProcessor]): binary.SequentialRenderer.Builder          = binary.SequentialRenderer.Builder(renderer.build)
  def apply (transformer: TwoPhaseTransformerBuilder[_, BinaryPostProcessor]): binary.SequentialTransformer.Builder = binary.SequentialTransformer.Builder(transformer.build)
  
  def apply (parser: MarkupParser): SequentialParser.Builder           = SequentialParser.Builder(parser)
  def apply (renderer: Renderer): SequentialRenderer.Builder           = SequentialRenderer.Builder(renderer)
  def apply (transformer: Transformer): SequentialTransformer.Builder  = SequentialTransformer.Builder(transformer)

  def apply (renderer: TwoPhaseRenderer[BinaryPostProcessor]): binary.SequentialRenderer.Builder           = binary.SequentialRenderer.Builder(renderer)
  def apply (transformer: TwoPhaseTransformer[BinaryPostProcessor]): binary.SequentialTransformer.Builder  = binary.SequentialTransformer.Builder(transformer)
  
  
  class SequentialParser[F[_]: Async] (parser: MarkupParser) extends SequentialInputOps[F] {

    type InputResult = SequentialParser.Op[F]
    
    val F: Async[F] = Async[F]
    
    val docType: TextDocumentType = DocumentType.Markup


    def fromInput (input: F[TextInput]): SequentialParser.Op[F] = SequentialParser.Op(parser, input)
    
  }
  
  object SequentialParser {

    case class Builder (parser: MarkupParser) {

      def build[F[_]: Async]: SequentialParser[F] = new SequentialParser[F](parser)

    }

    case class Op[F[_]: Async] (parser: MarkupParser, input: F[TextInput]) {
      
      def parse: F[Document] = ParserRuntime.run(this)
      
    }
    
  }

  class SequentialRenderer[F[_]: Async] (renderer: Renderer) {

    def from (input: Document): SequentialRenderer.OutputOps[F] = from(input.content, input.path)
    
    def from (element: Element): SequentialRenderer.OutputOps[F] = from(element, Path.Root)
    
    def from (element: Element, path: Path): SequentialRenderer.OutputOps[F] = 
      SequentialRenderer.OutputOps(renderer, element, path)

  }

  object SequentialRenderer {

    case class Builder (renderer: Renderer) {

      def build[F[_]: Async]: SequentialRenderer[F] = new SequentialRenderer[F](renderer)

    }

    case class OutputOps[F[_]: Async] (renderer: Renderer, input: Element, path: Path) extends SequentialTextOutputOps[F] {

      val F: Async[F] = Async[F]
      
      type Result = Op[F]
      
      def toOutput (output: F[TextOutput]): Op[F] = Op[F](renderer, input, path, output)

    }

    case class Op[F[_]: Async] (renderer: Renderer, input: Element, path: Path, output: F[TextOutput]) {

      def render: F[String] = RendererRuntime.run(this)

    }

  }

  class SequentialTransformer[F[_]: Async] (transformer: Transformer) extends SequentialInputOps[F] {

    type InputResult = SequentialTransformer.OutputOps[F]

    val F: Async[F] = Async[F]

    val docType: TextDocumentType = DocumentType.Markup


    def fromInput (input: F[TextInput]): SequentialTransformer.OutputOps[F] = SequentialTransformer.OutputOps(transformer, input)

  }

  object SequentialTransformer {

    case class Builder (transformer: Transformer) {

      def build[F[_]: Async]: SequentialTransformer[F] = new SequentialTransformer[F](transformer)

    }

    case class OutputOps[F[_]: Async] (transformer: Transformer, input: F[TextInput]) extends SequentialTextOutputOps[F] {

      val F: Async[F] = Async[F]

      type Result = Op[F]

      def toOutput (output: F[TextOutput]): Op[F] = Op[F](transformer, input, output)

    }

    case class Op[F[_]: Async] (transformer: Transformer, input: F[TextInput], output: F[TextOutput]) {

      def transform: F[String] = TransformerRuntime.run(this)

    }

  }
  
  
}

/** API for producing a result from processing various types of input.
  *
  * This is essentially a collection of shortcuts that allow any class
  * merging in this trait to define all input related operations in terms of the only
  * abstract method `fromInput`.
  *
  * @author Jens Halm
  */
trait SequentialInputOps[F[_]] {

  /** The type of the result returned by all operations of this trait.
    */
  type InputResult
  
  
  def F: Sync[F]

  /** The type of text document created by this instance.
    */
  def docType: TextDocumentType

  /**  Returns the result from parsing the specified string.
    *  Any kind of input is valid, including an empty string.
    */
  def fromString (str: String): InputResult = fromInput(F.pure(StringInput(str, docType)))

  /** Returns the result from parsing the file with the specified name.
    *  Any kind of character input is valid, including empty files.
    *
    *  @param name the name of the file to parse
    *  @param codec the character encoding of the file, if not specified the platform default will be used.
    */
  def fromFile (name: String)(implicit codec: Codec): InputResult = fromFile(new File(name))

  /** Returns the result from parsing the specified file.
    *  Any kind of character input is valid, including empty files.
    *
    *  @param file the file to use as input
    *  @param codec the character encoding of the file, if not specified the platform default will be used.
    */
  def fromFile (file: File)(implicit codec: Codec): InputResult = 
    fromInput(F.pure(TextFileInput(file, docType, Path(file.getName), codec)))

  /** Returns the result from parsing the specified input.
    *
    *  This is a generic method based on Laika's IO abstraction layer that concrete
    *  methods delegate to. Usually not used directly in application code, but
    *  might come in handy for very special requirements.
    *
    *  @param input the input for the parser
    */
  def fromInput (input: F[TextInput]): InputResult

}

trait SequentialTextOutputOps[F[_]] {

  type Result

  def F: Sync[F]

  /** Renders the model to the file with the specified name.
    *
    *  @param name the name of the file to parse
    *  @param codec the character encoding of the file, if not specified the platform default will be used.
    */
  def toFile (name: String)(implicit codec: Codec): Result = toFile(new File(name))

  /** Renders the model to the specified file.
    *
    *  @param file the file to write to
    *  @param codec the character encoding of the file, if not specified the platform default will be used.
    */
  def toFile (file: File)(implicit codec: Codec): Result = 
    toOutput(F.pure(TextFileOutput(file, Path(file.getName), codec)))
  
  // TODO - 0.12 - re-introduce string output

  /** Renders the model to the specified output.
    *
    *  This is a generic method based on Laika's IO abstraction layer that concrete
    *  methods delegate to. Usually not used directly in application code, but
    *  might come in handy for very special requirements.
    */
  def toOutput (output: F[TextOutput]): Result

}
