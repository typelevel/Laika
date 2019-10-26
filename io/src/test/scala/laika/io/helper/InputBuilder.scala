package laika.io.helper

import java.io.{ByteArrayInputStream, InputStream}

import cats.effect.{IO, Resource}
import laika.ast.DocumentType.Static
import laika.ast.{DocumentType, Path, TextDocumentType}
import laika.io.model._

import scala.io.Codec

trait InputBuilder {

  object ByteInput {
    def apply (input: String, path: Path)(implicit codec: Codec): BinaryInput[IO] =
      BinaryInput(path, Resource.fromAutoCloseable(IO(new ByteArrayInputStream(input.getBytes(codec.charSet)))))
  }
  
  def build (inputs: Seq[(Path, String)], docTypeMatcher: Path => DocumentType): TreeInput[IO] = {
    
    val mappedInputs = inputs.flatMap {
      case (path, content) => 
        docTypeMatcher(path) match {
          case docType: TextDocumentType => Seq(StringInput(content, docType, path))
          case Static => Seq(ByteInput(content, path))
          case _ => Nil
        }
        
    }

    TreeInput[IO](
      mappedInputs.collect { case i: TextInput   => TextDocument(i.path, i.docType, IO.pure[TextInput](i)) }, 
      mappedInputs.collect { case i: BinaryInput[IO] => i }
    )
  }
  
}
