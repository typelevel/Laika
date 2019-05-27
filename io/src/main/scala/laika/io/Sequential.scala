package laika.io

import java.io.File

import cats.effect.{Async, Sync}
import laika.api.builder.{ParserBuilder, RendererBuilder, TransformerBuilder}
import laika.api.{MarkupParser, Renderer, Transformer}
import laika.ast.{Document, DocumentType, Path, TextDocumentType}

import scala.io.Codec

object Sequential {

  def apply (parser: ParserBuilder): SequentialParser.Builder = apply(parser.build)
  def apply[FMT] (renderer: RendererBuilder[FMT]): Unit = apply(renderer.build)
  def apply[FMT] (transformer: TransformerBuilder[FMT]): Unit = apply(transformer.build)
  
  def apply (parser: MarkupParser): SequentialParser.Builder = new SequentialParser.Builder(parser)
  def apply (renderer: Renderer): Unit = ???
  def apply (transformer: Transformer): Unit = ???
  
  
  class SequentialParser[F[_]: Async] (parser: MarkupParser) extends SequentialInputOps[F] {

    type InputResult = SequentialParser.Op[F]
    
    val F: Async[F] = Async[F]
    
    val docType: TextDocumentType = DocumentType.Markup


    def fromInput (input: F[TextInput]): SequentialParser.Op[F] = SequentialParser.Op(parser, input)
    
    // type InputResult = SequentialParser.OutputOps[F]
    // def fromInput (input: F[TextInput]): InputResult = new SequentialParser.OutputOps[F](parser, input) this is for transformers
    
  }
  
  object SequentialParser {

    class Builder (parser: MarkupParser) {

      def build[F[_]: Async]: SequentialParser[F] = new SequentialParser[F](parser)

    }

    class OutputOps[F[_]: Async] (parser: MarkupParser, input: F[TextInput]) {

      def toFile (name: String): Op[F] = Op[F](parser, input, ())
      
    }
    
    case class Op[F[_]: Async] (parser: MarkupParser, input: F[TextInput]) {
      
      def parse: F[Document] = ???
      
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
