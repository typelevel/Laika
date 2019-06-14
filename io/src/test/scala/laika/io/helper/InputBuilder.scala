package laika.io.helper

import laika.ast.DocumentType.Static
import laika.ast.{DocumentType, Path, TextDocumentType}
import laika.io._

trait InputBuilder {

  def build (inputs: Seq[(Path, String)], docTypeMatcher: Path => DocumentType): InputCollection = {
    
    val mappedInputs = inputs.flatMap {
      case (path, content) => 
        docTypeMatcher(path) match {
          case docType: TextDocumentType => Seq(StringInput(content, docType, path))
          case Static => Seq(ByteInput(content, path))
          case _ => Nil
        }
        
    }

    InputCollection(
      mappedInputs.collect { case i: TextInput => i }, 
      mappedInputs.collect { case i: BinaryInput => i }
    )
  }
  
}
