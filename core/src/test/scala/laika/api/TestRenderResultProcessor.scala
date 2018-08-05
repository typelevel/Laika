/*
 * Copyright 2015-2016 the original author or authors.
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

package laika.api

import laika.factory.RenderResultProcessor
import laika.io.Output.BinaryOutput
import laika.io.OutputTree
import laika.io.OutputTree._
import laika.render.{AST, TextWriter}
import laika.tree.Documents.{Document, DocumentTree}
import laika.tree.Templates.TemplateRoot

object TestRenderResultProcessor extends RenderResultProcessor[TextWriter] {

  val format = AST
  
  def process (tree: DocumentTree, render: (DocumentTree, OutputTree) => Unit, defaultTemplate: TemplateRoot, output: BinaryOutput): Unit = {
    
    def baseName(docName: String) = docName.takeWhile(_ != '.')
    
    def append (sb: StringBuilder, result: ResultTree, src: DocumentTree): Unit = {
      src.content.foreach {
        case d: Document => result.result(baseName(d.name) + ".txt").foreach(s => sb.append(s + "\n"))
        case t: DocumentTree => result.subtree(t.name).foreach(append(sb, _, t))
      }
    }
    
    val strOutput = new StringOutputTree(tree.path)
    
    render(tree, strOutput)
    
    val sb = new StringBuilder
    append(sb, strOutput.result, tree)
    val result = sb.toString

    val out = output.asStream
    try {
      out.write(result.getBytes("UTF-8"))
    } finally {
      out.close()
    }
    
  }
  
}
