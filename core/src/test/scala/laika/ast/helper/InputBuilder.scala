/*
 * Copyright 2013-2016 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package laika.ast.helper

import laika.ast.{DocumentType, Path, TextDocumentType}
import laika.ast.DocumentType._
import laika.io._
import laika.io.TreeInput.InputTreeBuilder

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
  
  def parseTreeStructure (source: String): InputTreeBuilder = parseTree(source.split("\n").toList)._1
  
  
  // TODO - 0.12 - simular type will soon be in Prod source
  case class TestInputTree(textInputs: Seq[TextInput],
                           binaryInputs: Seq[BinaryInput],
                           sourcePaths: Seq[String]
  ) extends TreeInput {
    def ++ (other: TestInputTree): TestInputTree = copy(textInputs = textInputs ++ other.textInputs, binaryInputs = binaryInputs ++ other.binaryInputs)
  }
  
  private[InputBuilder] class TestInputTreeBuilder(dirs: List[TestInputTreeBuilder], files: List[(String,String)], val path: Path) extends InputTreeBuilder {
    
    def build (docTypeMatcher: Path => DocumentType): TestInputTree = {
    
      def text: Seq[TextInput] = files.map(f => (f._1, f._2, docTypeMatcher(path / f._1))) collect {
        case (name, content, docType: TextDocumentType) => StringInput(contents(content), docType, path / name)
      }

      def static: Seq[BinaryInput] = files collect {
        case (name, content) if docTypeMatcher(path / name) == Static => ByteInput(contents(content), path / name)
      }
      
      val subtrees = dirs map (_.build(docTypeMatcher)) filter (d => docTypeMatcher(path / d.path.name) != Ignored) reduceOption(_ ++ _)
      
      val thisTree = TestInputTree(text, static, Nil)
      
      subtrees.fold(thisTree)(thisTree ++ _)
    }
  }
  
  
}
