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

package laika.api

import java.io.{ByteArrayOutputStream, File, StringWriter}

import laika.api.Render.RenderMappedOutput
import laika.ast.Path.Root
import laika.ast._
import laika.ast.helper.OutputBuilder._
import laika.ast.helper.{InputBuilder, ModelBuilder, OutputBuilder}
import laika.bundle.{BundleProvider, StaticDocuments}
import laika.format._
import laika.io.{ByteInput, Input, StringTreeOutput}
import laika.render._
import laika.render.helper.RenderResult
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Codec

class RenderAPISpec extends FlatSpec 
                    with Matchers
                    with ModelBuilder { self =>

  
  val rootElem = root(p("aaö"), p("bbb"))

  val expected = """RootElement - Blocks: 2
      |. Paragraph - Spans: 1
      |. . Text - 'aaö'
      |. Paragraph - Spans: 1
      |. . Text - 'bbb'""".stripMargin

  "The Render API" should "render a document to a string" in {
    (Render as AST from rootElem toString) should be (expected)
  }

  it should "render a document to a file" ignore {
    val f = File.createTempFile("output", null)

    (Render as AST from rootElem toFile f).execute

    readFile(f) should be (expected)
  }

  it should "render a document to a java.io.Writer" ignore {
//    val writer = new StringWriter
//    (Render as AST from rootElem toWriter writer).execute
//    writer.toString should be (expected)
  }

  it should "render a document to a java.io.OutputStream" ignore {
//    val stream = new ByteArrayOutputStream
//    (Render as AST from rootElem toStream stream).execute
//    stream.toString should be (expected)
  }

  it should "render a document to a java.io.OutputStream, specifying the encoding explicitly" ignore {
//    val stream = new ByteArrayOutputStream
//    (Render as AST from rootElem).toStream(stream)(Codec.ISO8859).execute
//    stream.toString("ISO-8859-1") should be (expected)
  }

  it should "render a document to a java.io.OutputStream, specifying the encoding implicitly" ignore {
//    implicit val codec:Codec = Codec.ISO8859
//    val stream = new ByteArrayOutputStream
//    (Render as AST from rootElem toStream stream).execute
//    stream.toString("ISO-8859-1") should be (expected)
  }

  it should "allow to override the default renderer for specific element types" in {
    val render = Render as AST rendering { case (_, Text(content,_)) => s"String - '$content'" }
    val modifiedResult = expected.replaceAllLiterally("Text", "String")
    (render from rootElem toString) should be (modifiedResult)
  }

  
  trait DocBuilder {
    def markupDoc (num: Int, path: Path = Root)  = Document(path / ("doc"+num), root(p("Doc"+num)))
    def dynamicDoc (num: Int, path: Path = Root) = DynamicDocument(path / ("doc"+num), root(TemplateRoot(List(TemplateString("Doc"+num)))))
    
    def staticDoc (num: Int, path: Path = Root) = StaticDocument(ByteInput("Static"+num, path / s"static$num.txt"))
    
    
    def renderedDynDoc (num: Int) = """RootElement - Blocks: 1
      |. TemplateRoot - TemplateSpans: 1
      |. . TemplateString - 'Doc""".stripMargin + num + "'"
      
    def renderedDoc (num: Int) = """RootElement - Blocks: 1
        |. Paragraph - Spans: 1
        |. . Text - 'Doc""".stripMargin + num + "'"
  }
  
  trait TreeRenderer[FMT] {
    def input: DocumentTree
    
    def render: RenderMappedOutput[FMT]
    
    def renderedTree: RenderedTree = OutputBuilder.RenderedTree.toTreeView(render
      .from(DocumentTreeRoot(input))
      .toOutputTree(StringTreeOutput)
      .execute.rootTree
    )

    def addPosition (tree: DocumentTree, pos: Seq[Int] = Nil): DocumentTree = {
      val nextNum = Iterator.from(1)
      tree.copy(content = tree.content.map {
        case d: Document => d.copy(position = TreePosition(pos :+ nextNum.next))
        case t: DocumentTree =>
          val num = pos :+ nextNum.next
          addPosition(t.copy(position = TreePosition(num)), num)
      })
    }
  }
  
  trait ASTRenderer extends TreeRenderer[TextFormatter] {
    lazy val render = Render as AST
  }

  trait HTMLRenderer extends TreeRenderer[HTMLFormatter] {
    val rootElem = root(title("Title"), p("bbb"))
    lazy val render = Render as HTML
  }

  trait EPUB_XHTMLRenderer extends TreeRenderer[HTMLFormatter] {
    val rootElem = root(title("Title"), p("bbb"))
    lazy val render = Render as EPUB.XHTML
  }

  trait FORenderer extends TreeRenderer[FOFormatter] {
    val foStyles = Map("fo" -> StyleDeclarationSet(Root / "styles.fo.css", StyleDeclaration(StylePredicate.ElementType("Paragraph"), "font-size" -> "11pt")))
    val rootElem = root(self.title("Title"), p("bbb"))
    val subElem = root(self.title("Sub Title"), p("ccc"))

    def marker(text: String) = s"""<fo:marker marker-class-name="chapter"><fo:block>$text</fo:block></fo:marker>"""

    def title(id: String, text: String) =
      s"""<fo:block id="$id" font-family="sans-serif" font-size="16pt" font-weight="bold" keep-with-next="always" space-after="7mm" space-before="12mm">$text</fo:block>"""

    def render: RenderMappedOutput[FOFormatter] = Render as XSLFO
  }

  it should "render an empty tree" in {
    new ASTRenderer {
      val input = DocumentTree(Root, Nil)
      renderedTree should be (RenderedTree(Root, Nil))
    }
  }

  it should "render a tree with a single document" in {
    new ASTRenderer {
      val input = DocumentTree(Root, List(Document(Root / "doc", rootElem)))
      renderedTree should be (RenderedTree(Root, List(Documents(List(RenderedDocument(Root / "doc.txt", expected))))))
    }
  }

  it should "render a tree with a single document to HTML using the default template" in {
    new HTMLRenderer {
      val input = DocumentTree(Root, List(Document(Root / "doc", rootElem)))
      val expected = RenderResult.html.withDefaultTemplate("Title", """<h1 id="title" class="title">Title</h1>
        |      <p>bbb</p>""".stripMargin)
      renderedTree should be (RenderedTree(Root, List(Documents(List(RenderedDocument(Root / "doc.html", expected))))))
    }
  }

  it should "render a tree with a single document to HTML using a custom template in the root directory" in {
    new HTMLRenderer {
      val template = TemplateDocument(Root / "default.template.html", tRoot(tt("["), TemplateContextReference("document.content"), tt("]")))
      val input = DocumentTree(Root, List(Document(Root / "doc", rootElem)), templates = Seq(template))
      val expected = """[<h1 id="title" class="title">Title</h1>
        |<p>bbb</p>]""".stripMargin
      renderedTree should be (RenderedTree(Root, List(Documents(List(RenderedDocument(Root / "doc.html", expected))))))
    }
  }

  it should "render a tree with a single document to HTML using a custom template in an extension bundle" in {
    new HTMLRenderer {
      val template = tRoot(tt("["), TemplateContextReference("document.content"), tt("]"))
      override lazy val render = Render as HTML using BundleProvider.forTheme(HTML.Theme(defaultTemplate = Some(template)))
      val input = DocumentTree(Root, List(Document(Root / "doc", rootElem)))
      val expected = """[<h1 id="title" class="title">Title</h1>
                       |<p>bbb</p>]""".stripMargin
      renderedTree should be (RenderedTree(Root, List(Documents(List(RenderedDocument(Root / "doc.html", expected))))))
    }
  }

  it should "render a tree with a single document to EPUB.XHTML using the default template" in {
    new EPUB_XHTMLRenderer {
      val input = DocumentTree(Root, List(Document(Root / "doc", rootElem)))
      val expected = RenderResult.epub.withDefaultTemplate("Title", """<h1 id="title" class="title">Title</h1>
                                                                      |      <p>bbb</p>""".stripMargin)
      renderedTree should be (RenderedTree(Root, List(Documents(List(RenderedDocument(Root / "doc.epub.xhtml", expected))))))
    }
  }

  it should "render a tree with a single document to EPUB.XHTML using a custom template in the root directory" in {
    new EPUB_XHTMLRenderer {
      val template = TemplateDocument(Root / "default.template.epub.xhtml", tRoot(tt("["), TemplateContextReference("document.content"), tt("]")))
      val input = DocumentTree(Root, List(Document(Root / "doc", rootElem)), templates = Seq(template))
      val expected = """[<h1 id="title" class="title">Title</h1>
                       |<p>bbb</p>]""".stripMargin
      renderedTree should be (RenderedTree(Root, List(Documents(List(RenderedDocument(Root / "doc.epub.xhtml", expected))))))
    }
  }

  it should "render a tree with a single document to EPUB.XHTML using a custom template in an extension bundle" in {
    new EPUB_XHTMLRenderer {
      val template = tRoot(tt("["), TemplateContextReference("document.content"), tt("]"))
      override lazy val render = Render as EPUB.XHTML using BundleProvider.forTheme(EPUB.XHTML.Theme(defaultTemplate = Some(template)))
      val input = DocumentTree(Root, List(Document(Root / "doc", rootElem)))
      val expected = """[<h1 id="title" class="title">Title</h1>
                       |<p>bbb</p>]""".stripMargin
      renderedTree should be (RenderedTree(Root, List(Documents(List(RenderedDocument(Root / "doc.epub.xhtml", expected))))))
    }
  }

  it should "render a tree with a single document to XSL-FO using the default template and default CSS" in {
    new FORenderer {
      val input = DocumentTree(Root, List(Document(Root / "doc", rootElem)))
      val expected = RenderResult.fo.withDefaultTemplate(s"""${marker("Title")}
        |      ${title("_doc_title", "Title")}
        |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>""".stripMargin)
      renderedTree should be (RenderedTree(Root, List(Documents(List(RenderedDocument(Root / "doc.fo", expected))))))
    }
  }

  it should "render a tree with a single document to XSL-FO using a custom template" in {
    new FORenderer {
      val template = TemplateDocument(Root / "default.template.fo", tRoot(tt("["), TemplateContextReference("document.content"), tt("]")))
      val input = DocumentTree(Root, List(Document(Root / "doc", rootElem)), templates = Seq(template))
      val expected = s"""[${marker("Title")}
        |${title("_doc_title", "Title")}
        |<fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>]""".stripMargin
      renderedTree should be (RenderedTree(Root, List(Documents(List(RenderedDocument(Root / "doc.fo", expected))))))
    }
  }

  it should "render a tree with two documents to XSL-FO using a custom style sheet in an extension bundle" in {
    new FORenderer {
      override val render = Render as XSLFO using BundleProvider.forTheme(XSLFO.Theme(defaultStyles = foStyles("fo")))
      val input = DocumentTree(Root, List(
        Document(Root / "doc", rootElem),
        DocumentTree(Root / "tree", List(Document(Root / "tree" / "subdoc", subElem)))
      ))
      val expectedRoot = RenderResult.fo.withDefaultTemplate(s"""${marker("Title")}
        |      ${title("_doc_title", "Title")}
        |      <fo:block font-family="serif" font-size="11pt" space-after="3mm">bbb</fo:block>""".stripMargin)
      val expectedSub = RenderResult.fo.withDefaultTemplate(s"""${marker("Sub Title")}
        |      ${title("_tree_subdoc_sub-title", "Sub Title")}
        |      <fo:block font-family="serif" font-size="11pt" space-after="3mm">ccc</fo:block>""".stripMargin)
      renderedTree should be (RenderedTree(Root, List(
        Documents(List(RenderedDocument(Root / "doc.fo", expectedRoot))),
        Subtrees(List(RenderedTree(Root / "tree", List(
          Documents(List(RenderedDocument(Root / "tree" / "subdoc.fo", expectedSub)))
        ))))
      )))
    }
  }

  it should "render a tree with two documents to XSL-FO using a custom style sheet in the root directory" in {
    new FORenderer {
      val input = DocumentTree(Root, List(
        Document(Root / "doc", rootElem),
        DocumentTree(Root / "tree", List(Document(Root / "tree" / "subdoc", subElem)))
      ), styles = foStyles)
      val expectedRoot = RenderResult.fo.withDefaultTemplate(s"""${marker("Title")}
        |      ${title("_doc_title", "Title")}
        |      <fo:block font-family="serif" font-size="11pt" space-after="3mm">bbb</fo:block>""".stripMargin)
      val expectedSub = RenderResult.fo.withDefaultTemplate(s"""${marker("Sub Title")}
        |      ${title("_tree_subdoc_sub-title", "Sub Title")}
        |      <fo:block font-family="serif" font-size="11pt" space-after="3mm">ccc</fo:block>""".stripMargin)
      renderedTree should be (RenderedTree(Root, List(
          Documents(List(RenderedDocument(Root / "doc.fo", expectedRoot))),
          Subtrees(List(RenderedTree(Root / "tree", List(
              Documents(List(RenderedDocument(Root / "tree" / "subdoc.fo", expectedSub)))
          ))))
      )))
    }
  }

  it should "render a tree with two documents to XSL-FO using a custom style sheet in the sub directory" in {
    new FORenderer {
      val input = DocumentTree(Root, List(
        Document(Root / "doc", rootElem),
        DocumentTree(Root / "tree", List(Document(Root / "tree" / "subdoc", subElem)), styles = foStyles)
      ))
      val expectedRoot = RenderResult.fo.withDefaultTemplate(s"""${marker("Title")}
        |      ${title("_doc_title", "Title")}
        |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>""".stripMargin)
      val expectedSub = RenderResult.fo.withDefaultTemplate(s"""${marker("Sub Title")}
        |      ${title("_tree_subdoc_sub-title", "Sub Title")}
        |      <fo:block font-family="serif" font-size="11pt" space-after="3mm">ccc</fo:block>""".stripMargin)
      renderedTree should be (RenderedTree(Root, List(
          Documents(List(RenderedDocument(Root / "doc.fo", expectedRoot))),
          Subtrees(List(RenderedTree(Root / "tree", List(
              Documents(List(RenderedDocument(Root / "tree" / "subdoc.fo", expectedSub)))
          ))))
      )))
    }
  }

  it should "render a tree with a single dynamic document" ignore {
    new ASTRenderer with DocBuilder {
      val input = DocumentTree(Root, Nil, additionalContent = List(dynamicDoc(1)))
      renderedTree should be (RenderedTree(Root, List(Documents(List(RenderedDocument(Root / "doc1.txt", renderedDynDoc(1)))))))
    }
  }

  it should "render a tree with a single static document" ignore {
    new ASTRenderer with DocBuilder {
      val input = DocumentTree(Root, Nil, additionalContent = List(staticDoc(1)))
      renderedTree should be (RenderedTree(Root, List(Documents(List(RenderedDocument(Root / "static1.txt", "Static1"))))))
    }
  }

  it should "render a tree with all available file types" ignore {
    new ASTRenderer with DocBuilder {
      val input = addPosition(DocumentTree(Root,
        content = List(
          markupDoc(1),
          markupDoc(2),
          DocumentTree(Root / "dir1",
            content = List(markupDoc(3), markupDoc(4)),
            additionalContent = List(staticDoc(3), staticDoc(4))
          ),
          DocumentTree(Root / "dir2",
            content = List(markupDoc(5), markupDoc(6)),
            additionalContent = List(staticDoc(5), staticDoc(6))
          )
        ),
        additionalContent = List(staticDoc(1), staticDoc(2))
      ))
      renderedTree should be (RenderedTree(Root, List(
        Documents(List(
          RenderedDocument(Root / "doc1.txt", renderedDoc(1)),
          RenderedDocument(Root / "doc2.txt", renderedDoc(2)),
          RenderedDocument(Root / "static1.txt", "Static1"),
          RenderedDocument(Root / "static2.txt", "Static2")
        )),
        Subtrees(List(
          RenderedTree(Root / "dir1", List(
            Documents(List(
              RenderedDocument(Root / "dir1" / "doc3.txt", renderedDoc(3)),
              RenderedDocument(Root / "dir1" / "doc4.txt", renderedDoc(4)),
              RenderedDocument(Root / "dir1" / "static3.txt", "Static3"),
              RenderedDocument(Root / "dir1" / "static4.txt", "Static4")
           ))
        )),
        RenderedTree(Root / "dir2", List(
          Documents(List(
            RenderedDocument(Root / "dir2" / "doc5.txt", renderedDoc(5)),
            RenderedDocument(Root / "dir2" / "doc6.txt", renderedDoc(6)),
            RenderedDocument(Root / "dir2" / "static5.txt", "Static5"),
            RenderedDocument(Root / "dir2" / "static6.txt", "Static6")
          ))
        ))))
      )))
    }
  }

  it should "render a tree with static files merged from a theme" ignore new TreeRenderer[TextFormatter] with DocBuilder with InputBuilder {
    def contents = 1.to(6).map(num => (s"name$num", s"Theme$num")).toMap
    val dirs = """- theme1.js:name1
                 |- theme2.js:name2
                 |+ dir1
                 |  - theme3.js:name3
                 |  - theme4.js:name4
                 |+ dir3
                 |  - theme5.js:name5
                 |  - theme6.js:name6""".stripMargin
    val theme = AST.Theme(
      staticDocuments = StaticDocuments.empty // fromInputTree(parseTreeStructure(dirs))
    )
    val bundle = BundleProvider.forTheme(theme)
    val render = Render.as(AST).using(bundle)

    val input = addPosition(DocumentTree(Root,
      content = List(
        markupDoc(1),
        markupDoc(2),
        DocumentTree(Root / "dir1",
          content = List(markupDoc(3), markupDoc(4)),
          additionalContent = List(dynamicDoc(3), dynamicDoc(4), staticDoc(3), staticDoc(4))
        ),
        DocumentTree(Root / "dir2",
          content = List(markupDoc(5), markupDoc(6)),
          additionalContent = List(dynamicDoc(5), dynamicDoc(6), staticDoc(5), staticDoc(6))
        )
      ),
      additionalContent = List(dynamicDoc(1), dynamicDoc(2), staticDoc(1), staticDoc(2))
    ))
    renderedTree should be (RenderedTree(Root, List(
      Documents(List(
        RenderedDocument(Root / "doc1.txt", renderedDoc(1)),
        RenderedDocument(Root / "doc2.txt", renderedDoc(2)),
        RenderedDocument(Root / "theme1.js", "Theme1"),
        RenderedDocument(Root / "theme2.js", "Theme2"),
        RenderedDocument(Root / "doc1.txt", renderedDynDoc(1)),
        RenderedDocument(Root / "doc2.txt", renderedDynDoc(2)),
        RenderedDocument(Root / "static1.txt", "Static1"),
        RenderedDocument(Root / "static2.txt", "Static2")
      )),
      Subtrees(List(
        RenderedTree(Root / "dir3", List(
          Documents(List(
            RenderedDocument(Root / "dir3" / "theme5.js", "Theme5"),
            RenderedDocument(Root / "dir3" / "theme6.js", "Theme6")
          ))
        )),
        RenderedTree(Root / "dir1", List(
          Documents(List(
            RenderedDocument(Root / "dir1" / "doc3.txt", renderedDoc(3)),
            RenderedDocument(Root / "dir1" / "doc4.txt", renderedDoc(4)),
            RenderedDocument(Root / "dir1" / "theme3.js", "Theme3"),
            RenderedDocument(Root / "dir1" / "theme4.js", "Theme4"),
            RenderedDocument(Root / "dir1" / "doc3.txt", renderedDynDoc(3)),
            RenderedDocument(Root / "dir1" / "doc4.txt", renderedDynDoc(4)),
            RenderedDocument(Root / "dir1" / "static3.txt", "Static3"),
            RenderedDocument(Root / "dir1" / "static4.txt", "Static4")
          ))
        )),
        RenderedTree(Root / "dir2", List(
          Documents(List(
            RenderedDocument(Root / "dir2" / "doc5.txt", renderedDoc(5)),
            RenderedDocument(Root / "dir2" / "doc6.txt", renderedDoc(6)),
            RenderedDocument(Root / "dir2" / "doc5.txt", renderedDynDoc(5)),
            RenderedDocument(Root / "dir2" / "doc6.txt", renderedDynDoc(6)),
            RenderedDocument(Root / "dir2" / "static5.txt", "Static5"),
            RenderedDocument(Root / "dir2" / "static6.txt", "Static6")
          ))
        ))))
    )))
  }
  
  trait GatherRenderer {
    val rootElem = root(self.title("Title"), p("bbb"))
    val subElem = root(self.title("Sub Title"), p("ccc"))

    val input = DocumentTree(Root, List(
      Document(Root / "doc", rootElem),
      DocumentTree(Root / "tree", List(
        Document(Root / "tree" / "sub", subElem)
      ))
    ))

    val expectedResult = """RootElement - Blocks: 2
      |. Title(Id(title) + Styles(title)) - Spans: 1
      |. . Text - 'Title'
      |. Paragraph - Spans: 1
      |. . Text - 'bbb'
      |RootElement - Blocks: 2
      |. Title(Id(sub-title) + Styles(title)) - Spans: 1
      |. . Text - 'Sub Title'
      |. Paragraph - Spans: 1
      |. . Text - 'ccc'
      |""".stripMargin
  }

  it should "render a tree with two documents using a RenderResultProcessor writing to an output stream" ignore {
//    new GatherRenderer {
//      val out = new ByteArrayOutputStream
//      (Render as TestRenderResultProcessor from input toStream out).execute
//      out.toString should be (expectedResult)
//    }
  }

  it should "render a tree with two documents using a RenderResultProcessor writing to a file" in {
    new GatherRenderer {
      val f = File.createTempFile("output", null)
      Render
        .as(TestRenderResultProcessor)
        .from(DocumentTreeRoot(input))
        .toFile(f)
        .execute
      readFile(f) should be (expectedResult)
    }
  }

  trait FileSystemTest extends DocBuilder {
    val input = DocumentTree(Root, List(
      markupDoc(1),
      markupDoc(2),
      DocumentTree(Root / "dir1", List(
        markupDoc(3),
        markupDoc(4)
      )),
      DocumentTree(Root / "dir2", List(
        markupDoc(5),
        markupDoc(6)
      ))
    ))

    def readFiles (base: String) = {
      readFile(base+"/doc1.txt") should be (renderedDoc(1))
      readFile(base+"/doc2.txt") should be (renderedDoc(2))
      readFile(base+"/dir1/doc3.txt") should be (renderedDoc(3))
      readFile(base+"/dir1/doc4.txt") should be (renderedDoc(4))
      readFile(base+"/dir2/doc5.txt") should be (renderedDoc(5))
      readFile(base+"/dir2/doc6.txt") should be (renderedDoc(6))
    }
  }

  ignore should "render to a directory using the toDirectory method" in {
//    new FileSystemTest {
//      val f = createTempDirectory("renderToDir")
//      (Render as AST from input toDirectory f).execute
//      readFiles(f.getPath)
//    }
  }

  ignore should "render to a directory using the Directory object" in {
//    new FileSystemTest {
//      val f = createTempDirectory("renderToTree")
//      (Render as AST from input toDirectory(f)).execute
//      readFiles(f.getPath)
//    }
  }

  ignore should "render to a directory in parallel" in {
//    new FileSystemTest {
//      val f = createTempDirectory("renderParallel")
//      Render.as(AST).inParallel.from(input).toDirectory(f).execute
//      readFiles(f.getPath)
//    }
  }

  ignore should "render to a directory using a document with non-ASCII characters" in new DocBuilder {
//    val expected = """RootElement - Blocks: 1
//                     |. Paragraph - Spans: 1
//                     |. . Text - 'Doc äöü'""".stripMargin
//    val f = createTempDirectory("renderNonASCII")
//    val input = DocumentTree(Root, List(
//      Document(Root / "doc", root(p("Doc äöü")))
//    ))
//    Render.as(AST).from(input).toDirectory(f)(Codec.ISO8859).execute
//    readFile(new File(f, "doc.txt"), Codec.ISO8859) should be (expected)
  }
  

}
  