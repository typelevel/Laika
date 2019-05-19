package laika.io.helper

import laika.ast.DocumentType.{Ignored, Static}
import laika.ast.{DocumentType, Path, TextDocumentType}
import laika.io._

trait InputBuilder {

  
  def contents: String => String
  
  private def parseTree (dirStructure: List[String], path: Path = Path.Root, indent: Int = 0): (TestInputTreeBuilder, List[String]) = {
    val prefix = indent * 2
    val lines = dirStructure.takeWhile(_.startsWith(" " * prefix))
    
    def parseLines (lines: List[String]): (List[TestInputTreeBuilder], List[(String,String)], List[String]) = lines match {
      case Nil | "" :: _ => (Nil, Nil, Nil)
      case line :: rest => line.drop(prefix).take(2) match {
        case "+ " => {
          val name = line.drop(prefix).drop(2)
          val (dir, rest2) = parseTree(rest, path / name, indent + 1)
          val (dirs, files, rest3) = parseLines(rest2)
          (dir :: dirs, files, rest3)
        }
        case "- " => {
          val (dirs, files, rest2) = parseLines(rest)
          val n = line.drop(prefix).drop(2).split(":")
          (dirs, (n(0),n(1)) :: files, rest2)
        }
      }
    }
    
    val (dirs, files, _) = parseLines(lines)
    (new TestInputTreeBuilder(dirs, files, path), dirStructure.dropWhile(_.startsWith(" " * prefix)))
  }
  
  def parseTreeStructure (source: String, docTypeMatcher: Path => DocumentType): TreeInput = parseTree(source.split("\n").toList)._1.build(docTypeMatcher)
  
  private[InputBuilder] class TestInputTreeBuilder(dirs: List[TestInputTreeBuilder], files: List[(String,String)], val path: Path) {
    
    def build (docTypeMatcher: Path => DocumentType): InputCollection = {
    
      def text: Seq[TextInput] = files.map(f => (f._1, f._2, docTypeMatcher(path / f._1))) collect {
        case (name, content, docType: TextDocumentType) => StringInput(contents(content), docType, path / name)
      }

      def static: Seq[BinaryInput] = files collect {
        case (name, content) if docTypeMatcher(path / name) == Static => ByteInput(contents(content), path / name)
      }

      def merge (c1: InputCollection, c2: InputCollection): InputCollection = c1.copy(
        textInputs = c1.textInputs ++ c2.textInputs, 
        binaryInputs = c1.binaryInputs ++ c2.binaryInputs
      )
      
      val subtrees = dirs map (_.build(docTypeMatcher)) filter (d => docTypeMatcher(path / d.path.name) != Ignored)
      
      val thisTree = InputCollection(text, static)
      
      (thisTree +: subtrees).reduce(merge)
    }
  }
  
  
}
