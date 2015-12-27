/*
 * Copyright 2015 the original author or authors.
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

package laika.render

import laika.api.Render
import laika.factory.RenderResultProcessor
import laika.io.Input
import laika.io.Output.BinaryOutput
import laika.io.OutputProvider.OutputConfig
import laika.tree.Documents.Document
import laika.tree.Documents.DocumentTree
import laika.tree.Documents.Root
import laika.tree.Elements.Paragraph
import laika.tree.Elements.RootElement
import laika.tree.Elements.Text
import laika.tree.Elements.Title

import java.io.ByteArrayOutputStream
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class FOforPDFSpec extends FlatSpec with Matchers {
  
  
  case class FOTest (config: PDFConfig) extends RenderResultProcessor[FOWriter] {
    
    val factory = XSLFO
    
    private val foForPDF = new FOforPDF(config)
    
    def process (tree: DocumentTree, render: (DocumentTree, OutputConfig) => Unit, output: BinaryOutput) = {
    
      val fo = foForPDF.renderFO(tree, render)
      val out = output.asStream
      out.write(fo.getBytes("UTF-8"))
      
    }
    
  }
  
  
  trait TreeModel {
    
    def doc(num: Int) = 
      new Document(Root / s"doc$num.md", RootElement(Seq(
          Title(Seq(Text(s"Title $num"))), 
          Paragraph(Seq(Text(s"Text $num")))
      )))
  }
  
  trait ResultModel {
    
    private lazy val defaultTemplate = Input.fromClasspath("/templates/default.template.fo", Root / "default.template.fo").asParserInput.source.toString
    
    def result(num: Int) = 
      s"""<fo:block font-family="sans-serif" font-size="18pt" keep-with-next="always">Title $num</fo:block>
        |<fo:block font-family="serif" font-size="10pt">Text $num</fo:block>""".stripMargin
        
    def withDefaultTemplate(results: String*): String =
      defaultTemplate.replace("{{document.content}}", results.reduce(_ + _))    
        
  }
  
  
  trait Setup extends TreeModel with ResultModel {
    
    def config: PDFConfig
    
    def tree: DocumentTree
    
    def result = {
      val stream = new ByteArrayOutputStream
      Render as FOTest(config) from tree toStream stream      
      stream.toString
    }
    
  }
  
  
  "The FOforPDF utility" should "render a tree with all structure elements disabled" in new Setup {
    
    val config = PDFConfig(treeTitles = false, docTitles = false, bookmarks = false, toc = false)
    
    val tree = new DocumentTree(Root,
      documents = Seq(doc(1), doc(2)),
      subtrees = Seq(
        new DocumentTree(Root / "tree1", documents = Seq(doc(3), doc(4))),
        new DocumentTree(Root / "tree2", documents = Seq(doc(5), doc(6)))
      )
    )
    
    result should be (withDefaultTemplate(result(1), result(2), result(3), result(4), result(5), result(6)))
  }
  
  
}
