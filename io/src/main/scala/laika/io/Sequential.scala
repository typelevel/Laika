package laika.io

import java.io.{File, InputStream, OutputStream}

import cats.effect.{Async, ContextShift, Sync}
import cats.implicits._
import laika.api.builder._
import laika.api.{MarkupParser, Renderer, Transformer}
import laika.ast.Path.Root
import laika.ast.{Document, DocumentType, Element, Path, TextDocumentType}
import laika.factory.BinaryPostProcessor
import laika.runtime.{ParserRuntime, RendererRuntime, Runtime, TransformerRuntime}

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
  
  
  class SequentialParser[F[_]: Async: Runtime] (parser: MarkupParser) extends SequentialInputOps[F] {

    type InputResult = SequentialParser.Op[F]
    
    val F: Async[F] = Async[F]
    
    val docType: TextDocumentType = DocumentType.Markup


    def fromInput (input: F[TextInput]): SequentialParser.Op[F] = SequentialParser.Op(parser, input)
    
  }
  
  object SequentialParser {

    case class Builder (parser: MarkupParser) {

      def build[F[_]: Async] (processingContext: ContextShift[F], blockingContext: ContextShift[F]): SequentialParser[F] = 
        new SequentialParser[F](parser)(implicitly[Async[F]], Runtime.sequential(processingContext, blockingContext))

    }

    case class Op[F[_]: Async: Runtime] (parser: MarkupParser, input: F[TextInput]) {
      
      def parse: F[Document] = ParserRuntime.run(this)
      
    }
    
  }

  class SequentialRenderer[F[_]: Async: Runtime] (renderer: Renderer) {

    def from (input: Document): SequentialRenderer.OutputOps[F] = from(input.content, input.path)
    
    def from (element: Element): SequentialRenderer.OutputOps[F] = from(element, Path.Root)
    
    def from (element: Element, path: Path): SequentialRenderer.OutputOps[F] = 
      SequentialRenderer.OutputOps(renderer, element, path)

  }

  object SequentialRenderer {

    case class Builder (renderer: Renderer) {

      def build[F[_]: Async] (processingContext: ContextShift[F], blockingContext: ContextShift[F]): SequentialRenderer[F] = 
        new SequentialRenderer[F](renderer)(implicitly[Async[F]], Runtime.sequential(processingContext, blockingContext))

    }

    case class OutputOps[F[_]: Async: Runtime] (renderer: Renderer, input: Element, path: Path) extends SequentialTextOutputOps[F] {

      val F: Async[F] = Async[F]
      
      type Result = Op[F]
      
      def toOutput (output: F[TextOutput]): Op[F] = Op[F](renderer, input, path, output)

    }

    case class Op[F[_]: Async: Runtime] (renderer: Renderer, input: Element, path: Path, output: F[TextOutput]) {

      def render: F[String] = RendererRuntime.run(this)

    }

  }

  class SequentialTransformer[F[_]: Async: Runtime] (transformer: Transformer) extends SequentialInputOps[F] {

    type InputResult = SequentialTransformer.OutputOps[F]

    val F: Async[F] = Async[F]

    val docType: TextDocumentType = DocumentType.Markup


    def fromInput (input: F[TextInput]): SequentialTransformer.OutputOps[F] = SequentialTransformer.OutputOps(transformer, input)

  }

  object SequentialTransformer {

    case class Builder (transformer: Transformer) {

      def build[F[_]: Async] (processingContext: ContextShift[F], blockingContext: ContextShift[F]): SequentialTransformer[F] = 
        new SequentialTransformer[F](transformer)(implicitly[Async[F]], Runtime.sequential(processingContext, blockingContext))

    }

    case class OutputOps[F[_]: Async: Runtime] (transformer: Transformer, input: F[TextInput]) extends SequentialTextOutputOps[F] {

      val F: Async[F] = Async[F]

      type Result = Op[F]

      def toOutput (output: F[TextOutput]): Op[F] = Op[F](transformer, input, output)

    }

    case class Op[F[_]: Async: Runtime] (transformer: Transformer, input: F[TextInput], output: F[TextOutput]) {

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
  
  
  def F: Async[F]

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

  /** Returns the result from parsing the input from the specified stream.
    *
    *  @param stream the stream to use as input for the parser
    *  @param autoClose whether the stream should be closed after reading all input              
    *  @param codec the character encoding of the stream, if not specified the platform default will be used.
    */
  def fromStream (stream: F[InputStream], autoClose: Boolean = true)(implicit codec: Codec): InputResult =
    fromInput(F.map(stream)(CharStreamInput(_, docType, Root, autoClose, codec)))

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

  def F: Async[F]

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

  /** Renders the model to the specified output stream.
    *
    *  @param stream the stream to render to
    *  @param autoClose whether the stream should be closed after all output had been written   
    *  @param codec the character encoding of the stream, if not specified the platform default will be used.
    */
  def toStream (stream: F[OutputStream], autoClose: Boolean = true)(implicit codec: Codec): Result = 
    toOutput(F.map(stream)(CharStreamOutput(_, Root, autoClose, codec)))
  
  // TODO - 0.12 - re-introduce string output

  /** Renders the model to the specified output.
    *
    *  This is a generic method based on Laika's IO abstraction layer that concrete
    *  methods delegate to. Usually not used directly in application code, but
    *  might come in handy for very special requirements.
    */
  def toOutput (output: F[TextOutput]): Result

}
