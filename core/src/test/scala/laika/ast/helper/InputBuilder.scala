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

import laika.ast.Path
import laika.ast.Path.Root
import laika.io.DocumentType._
import laika.io.{DocumentType, Input, InputTree}
import laika.io.InputTree.InputTreeBuilder

trait InputBuilder {

  
  def contents: String => String
  
  private def parseTree (dirStructure: List[String], path: Path = Root, indent: Int = 0): (TestInputTreeBuilder, List[String]) = {
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
  
  
  case class TestInputTree(path: Path,
                           configDocuments: Seq[Input],
                           markupDocuments: Seq[Input],
                           dynamicDocuments: Seq[Input],
                           styleSheets: Map[String,Seq[Input]],
                           staticDocuments: Seq[Input],
                           templates: Seq[Input],
                           subtrees: Seq[InputTree],
                           sourcePaths: Seq[String]
  ) extends InputTree
  
  private[InputBuilder] class TestInputTreeBuilder(dirs: List[TestInputTreeBuilder], files: List[(String,String)], val path: Path) extends InputTreeBuilder {
    
    def build (docTypeMatcher: Path => DocumentType): InputTree = {
    
      def input (inputName: String, contentId: String, path: Path): Input = Input.fromString(contents(contentId), path / inputName)

      def pathFor (f: String) = path / f
  
      val documents = files map (f => (docTypeMatcher(pathFor(f._1)), input(f._1, f._2, path))) groupBy (_._1) mapValues (_.map(_._2)) withDefaultValue Nil
      
      val styleSheets = documents collect { case (StyleSheet(format), inputs) => (format, inputs) }
      
      val subtrees = dirs map (_.build(docTypeMatcher)) filter (d => docTypeMatcher(pathFor(d.path.name)) != Ignored)
      
      TestInputTree(path, documents(Config), documents(Markup), documents(Dynamic), styleSheets, documents(Static), documents(Template), subtrees, Nil)
      
    }
  }
  
  
}
