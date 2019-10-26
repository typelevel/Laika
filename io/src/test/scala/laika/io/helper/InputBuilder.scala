package laika.io.helper

import java.io.ByteArrayInputStream

import cats.effect.IO
import laika.ast.DocumentType.Static
import laika.ast.{DocumentType, Path, TextDocumentType}
import laika.io.model._

import scala.io.Codec

trait InputBuilder {

  object ByteInput {
    def apply (input: String, path: Path)(implicit codec: Codec): BinaryStreamInput =
      BinaryStreamInput(new ByteArrayInputStream(input.getBytes(codec.charSet)), autoClose = true, path)
  }
  
  def build (inputs: Seq[(Path, String)], docTypeMatcher: Path => DocumentType): InputCollection[IO] = {
    
    val mappedInputs = inputs.flatMap {
      case (path, content) => 
        docTypeMatcher(path) match {
          case docType: TextDocumentType => Seq(StringInput(content, docType, path))
          case Static => Seq(ByteInput(content, path))
          case _ => Nil
        }
        
    }

    InputCollection[IO](
      mappedInputs.collect { case i: TextInput => i }, 
      mappedInputs.collect { case i: BinaryInput => StaticDocument[IO](i.path, IO.pure(i)) }
    )
  }
  
}
