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

import laika.ast.{Document, DocumentTree}
import laika.execute.OutputExecutor
import laika.factory.RenderResultProcessor2
import laika.format.AST2
import laika.io.{BinaryOutput, RenderResult2, RenderedDocument, RenderedTree}
import laika.render.TextFormatter

object TestRenderResultProcessor extends RenderResultProcessor2[TextFormatter] {

  val format = AST2

  def prepareTree (tree: DocumentTree): DocumentTree = tree

  def process (result: RenderResult2, output: BinaryOutput): Unit = {
    
    def append (sb: StringBuilder, result: RenderedTree): Unit = {
      result.content.foreach {
        case d: RenderedDocument => sb.append(d.content + "\n")
        case t: RenderedTree => append(sb, t)
        case _ => ()
      }
    }
    
    val sb = new StringBuilder
    append(sb, result.rootTree)
    val resultString = sb.toString

    val out = OutputExecutor.asStream(output)
    try {
      out.write(resultString.getBytes("UTF-8"))
    } finally {
      out.close()
    }
    
  }
  
}
