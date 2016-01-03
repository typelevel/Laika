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

package laika.render

import laika.api.Render
import laika.factory.RenderResultProcessor
import laika.io.Input
import laika.io.Output.BinaryOutput
import laika.io.OutputProvider.OutputConfig
import laika.tree.Documents.Document
import laika.tree.Documents.DocumentTree
import laika.tree.Documents.Root
import laika.tree.Elements._

import java.io.ByteArrayOutputStream
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigValueFactory

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
    
    def doc(num: Int) = {
      val parent = if (num > 4) Root / "tree2" else if (num > 2) Root / "tree1" else Root
      new Document(parent / s"doc$num.md", RootElement(Seq(
          Title(Seq(Text(s"Title $num")), Id(s"title-$num") + Styles("title")), 
          Paragraph(Seq(Text(s"Text $num")))
      ))).removeRules
    }
      
    def configWithTreeTitle(num: Int) = Some(ConfigFactory.empty.withValue("title", ConfigValueFactory.fromAnyRef(s"Tree $num")))
  }
  
  trait ResultModel {
    
    private lazy val defaultTemplate = Input.fromClasspath("/templates/default.template.fo", Root / "default.template.fo").asParserInput.source.toString
    
    def results(num: Int) = (1 to num) map (result) reduce (_ + _)
    
    def idPrefix(num: Int) = if (num > 4) "_tree2" else if (num > 2) "_tree1" else ""
    
    def result(num: Int) = {
      s"""<fo:block id="${idPrefix(num)}_doc${num}_title-$num" font-family="sans-serif" font-size="18pt" font-weight="bold" keep-with-next="always">Title $num</fo:block>
        |<fo:block font-family="serif" font-size="10pt">Text $num</fo:block>""".stripMargin
    }
    
    def resultsWithDocTitle(num: Int) = (1 to num) map (resultWithDocTitle) reduce (_ + _)
    
    def resultWithDocTitle(num: Int) = {
      s"""<fo:block id="${idPrefix(num)}_doc${num}_">
        |  <fo:block id="${idPrefix(num)}_doc${num}_title-$num" font-family="sans-serif" font-size="18pt" font-weight="bold" keep-with-next="always">Title $num</fo:block>
        |</fo:block>
        |<fo:block font-family="serif" font-size="10pt">Text $num</fo:block>""".stripMargin
    }
    
    def treeTitleResult(num: Int) = {
      val idPrefix = if (num == 3) "_tree2" else if (num == 2) "_tree1" else ""
      s"""<fo:block id="${idPrefix}__title__" font-family="sans-serif" font-weight="bold" font-size="18pt" keep-with-next="always">Tree $num</fo:block>"""
    }
    
    def tocDocResult (num: Int) = 
      s"""<fo:block font-family="serif" font-size="10pt" text-align-last="justify"><fo:basic-link color="#3399FF" internal-destination="${idPrefix(num)}_doc${num}_">Title $num<fo:leader leader-pattern="dots"></fo:leader><fo:page-number-citation ref-id="${idPrefix(num)}_doc${num}_" /></fo:basic-link></fo:block>""" + "\n"
    
    def tocTreeResult (num: Int) = 
      s"""<fo:block font-family="serif" font-size="10pt" text-align-last="justify"><fo:basic-link color="#3399FF" internal-destination="_tree${num}__title__">Tree ${num+1}<fo:leader leader-pattern="dots"></fo:leader><fo:page-number-citation ref-id="_tree${num}__title__" /></fo:basic-link></fo:block>""" + "\n"  
    
    def withDefaultTemplate(result: String): String =
      defaultTemplate.replace("{{document.content}}", result)    
      
    def bookmarkTreeResult(treeNum: Int, docNum: Int) = s"""  <fo:bookmark internal-destination="_tree${treeNum}__title__">
      |    <fo:bookmark-title>
      |      Tree ${treeNum + 1}
      |    </fo:bookmark-title>
      |    <fo:bookmark internal-destination="_tree${treeNum}_doc${docNum}_">
      |      <fo:bookmark-title>
      |        Title $docNum
      |      </fo:bookmark-title></fo:bookmark>
      |    <fo:bookmark internal-destination="_tree${treeNum}_doc${docNum + 1}_">
      |      <fo:bookmark-title>
      |        Title ${docNum + 1}
      |      </fo:bookmark-title></fo:bookmark></fo:bookmark>
      |""".stripMargin  
      
    val bookmarkRootResult = """<fo:bookmark-tree>
      |  <fo:bookmark internal-destination="_doc1_">
      |    <fo:bookmark-title>
      |      Title 1
      |    </fo:bookmark-title></fo:bookmark>
      |  <fo:bookmark internal-destination="_doc2_">
      |    <fo:bookmark-title>
      |      Title 2
      |    </fo:bookmark-title></fo:bookmark>
      |""".stripMargin  
      
    val closeBookmarks = "</fo:bookmark-tree>"
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
    
    result should be (withDefaultTemplate(results(6)))
  }
  
  it should "render a tree with document titles" in new Setup {
    
    val config = PDFConfig(treeTitles = false, docTitles = true, bookmarks = false, toc = false)
    
    val tree = new DocumentTree(Root,
      documents = Seq(doc(1), doc(2)),
      subtrees = Seq(
        new DocumentTree(Root / "tree1", documents = Seq(doc(3), doc(4))),
        new DocumentTree(Root / "tree2", documents = Seq(doc(5), doc(6)))
      )
    )
    
    result should be (withDefaultTemplate(resultsWithDocTitle(6)))
  }
  
  it should "render a tree with tree titles" in new Setup {
    
    val config = PDFConfig(treeTitles = true, docTitles = false, bookmarks = false, toc = false)
    
    val tree = new DocumentTree(Root,
      documents = Seq(doc(1), doc(2)),
      subtrees = Seq(
        new DocumentTree(Root / "tree1", documents = Seq(doc(3), doc(4)), config = configWithTreeTitle(2)),
        new DocumentTree(Root / "tree2", documents = Seq(doc(5), doc(6)), config = configWithTreeTitle(3))
      ),
      config = configWithTreeTitle(1)
    )
    
    result should be (withDefaultTemplate(treeTitleResult(1) + result(1) + result(2)
        + treeTitleResult(2) + result(3) + result(4)
        + treeTitleResult(3) + result(5) + result(6)))
  }
  
  it should "render a tree with a table of content" in new Setup {
    
    val config = PDFConfig(treeTitles = false, docTitles = false, bookmarks = false, toc = true)
    
    val tree = new DocumentTree(Root,
      documents = Seq(doc(1), doc(2)),
      subtrees = Seq(
        new DocumentTree(Root / "tree1", documents = Seq(doc(3), doc(4)), config = configWithTreeTitle(2)),
        new DocumentTree(Root / "tree2", documents = Seq(doc(5), doc(6)), config = configWithTreeTitle(3))
      ),
      config = configWithTreeTitle(1)
    )
    
    result should be (withDefaultTemplate(tocDocResult(1) + tocDocResult(2)
        + tocTreeResult(1) + tocDocResult(3) + tocDocResult(4)
        + tocTreeResult(2) + tocDocResult(5) + tocDocResult(6).dropRight(1) + results(6)))
  }
  
  it should "render a tree with bookmarks" in new Setup {
    
    val config = PDFConfig(treeTitles = false, docTitles = false, bookmarks = true, toc = false)
    
    val tree = new DocumentTree(Root,
      documents = Seq(doc(1), doc(2)),
      subtrees = Seq(
        new DocumentTree(Root / "tree1", documents = Seq(doc(3), doc(4)), config = configWithTreeTitle(2)),
        new DocumentTree(Root / "tree2", documents = Seq(doc(5), doc(6)), config = configWithTreeTitle(3))
      ),
      config = configWithTreeTitle(1)
    )
    
    result should be (withDefaultTemplate(bookmarkRootResult + bookmarkTreeResult(1,3) + bookmarkTreeResult(2,5).dropRight(1) + closeBookmarks + results(6)))
  }
  
  it should "render a tree with all structure elements enabled" in new Setup {
    
    val config = PDFConfig(treeTitles = true, docTitles = true, bookmarks = true, toc = true)
    
    val tree = new DocumentTree(Root,
      documents = Seq(doc(1), doc(2)),
      subtrees = Seq(
        new DocumentTree(Root / "tree1", documents = Seq(doc(3), doc(4)), config = configWithTreeTitle(2)),
        new DocumentTree(Root / "tree2", documents = Seq(doc(5), doc(6)), config = configWithTreeTitle(3))
      ),
      config = configWithTreeTitle(1)
    )
    result should be (withDefaultTemplate(
        treeTitleResult(1) + tocDocResult(1) + tocDocResult(2)
        + tocTreeResult(1) + tocDocResult(3) + tocDocResult(4)
        + tocTreeResult(2) + tocDocResult(5) + tocDocResult(6).dropRight(1) 
        + bookmarkRootResult + bookmarkTreeResult(1,3) + bookmarkTreeResult(2,5).dropRight(1) + closeBookmarks 
        + resultWithDocTitle(1) + resultWithDocTitle(2)
        + treeTitleResult(2) + resultWithDocTitle(3) + resultWithDocTitle(4)
        + treeTitleResult(3) + resultWithDocTitle(5) + resultWithDocTitle(6)
    ))
  }
  
  
}
