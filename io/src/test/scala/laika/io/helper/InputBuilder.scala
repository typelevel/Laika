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
    
    val classified = inputs.map { case (path, content) => (path, content, docTypeMatcher(path)) }
    
    val textInputs: Seq[TextDocument[IO]] = classified.collect {
      case (path, content, docType: TextDocumentType) => TextDocument(path, docType, IO.pure[TextInput](StringInput(content, docType, path)))
    }

    val binaryInputs = classified.collect {
      case (path, content, Static) => ByteInput(content, path)
    }

    TreeInput(textInputs, binaryInputs)
  }
  
}
