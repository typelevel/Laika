/*
 * Copyright 2012-2019 the original author or authors.
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

package laika.io

import java.io.{ByteArrayOutputStream, File}

import cats.effect.{ContextShift, IO}
import laika.api.Renderer
import laika.config.Key
import laika.ast.Path.Root
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.bundle.BundleProvider
import laika.format._
import laika.io.implicits._
import laika.io.text.ParallelRenderer
import laika.io.helper.OutputBuilder._
import laika.io.helper.{InputBuilder, OutputBuilder, RenderResult}
import laika.io.model.{BinaryInput, StringTreeOutput}
import laika.render._
import laika.io.runtime.RendererRuntime.{DuplicatePath, RendererErrors}
import laika.io.runtime.TestContexts._
import org.scalatest.{Assertion, FlatSpec, Matchers}

import scala.concurrent.ExecutionContext
import scala.io.Codec

class ParallelRendererSpec extends FlatSpec 
                    with Matchers
                    with ModelBuilder { self =>

  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  
  val rootElem: RootElement = root(p("aaö"), p("bbb"))

  val expected: String = """RootElement - Blocks: 2
      |. Paragraph - Spans: 1
      |. . Text - 'aaö'
      |. Paragraph - Spans: 1
      |. . Text - 'bbb'""".stripMargin

  trait DocBuilder extends InputBuilder {
    def markupDoc (num: Int, path: Path = Root)  = Document(path / ("doc"+num), root(p("Doc"+num)))
    
    def staticDoc (num: Int, path: Path = Root) = ByteInput("Static"+num, path / s"static$num.txt")
    
    
    def renderedDynDoc (num: Int): String = """RootElement - Blocks: 1
      |. TemplateRoot - TemplateSpans: 1
      |. . TemplateString - 'Doc""".stripMargin + num + "'"
      
    def renderedDoc (num: Int): String = """RootElement - Blocks: 1
        |. Paragraph - Spans: 1
        |. . Text - 'Doc""".stripMargin + num + "'"
  }
  
  trait TreeRenderer[FMT] {
    
    def treeRoot: DocumentTreeRoot = DocumentTreeRoot(input)
    
    def input: DocumentTree
    
    def renderer: ParallelRenderer[IO]
    
    def renderedTree: RenderedTreeViewRoot = RenderedTreeView.toTreeView(renderer
      .from(treeRoot)
      .toOutput(StringTreeOutput)
      .render.unsafeRunSync()
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
    lazy val renderer: ParallelRenderer[IO] = Renderer.of(AST).io(blocker).parallel[IO].build
  }

  trait HTMLRenderer extends TreeRenderer[HTMLFormatter] {
    val rootElem: RootElement = root(title("Title"), p("bbb"))
    lazy val renderer: ParallelRenderer[IO] = Renderer.of(HTML).io(blocker).parallel[IO].build
  }

  trait EPUB_XHTMLRenderer extends TreeRenderer[HTMLFormatter] {
    val rootElem: RootElement = root(title("Title"), p("bbb"))
    lazy val renderer: ParallelRenderer[IO] = Renderer.of(EPUB.XHTML).io(blocker).parallel[IO].build
  }

  trait FORenderer extends TreeRenderer[FOFormatter] {
    def foStyles (path: Path = Root) = Map("fo" -> StyleDeclarationSet(path / "styles.fo.css", StyleDeclaration(StylePredicate.ElementType("Paragraph"), "font-size" -> "11pt")))
    val rootElem: RootElement = root(self.title("Title"), p("bbb"))
    val subElem: RootElement = root(self.title("Sub Title"), p("ccc"))

    def marker(text: String) = s"""<fo:marker marker-class-name="chapter"><fo:block>$text</fo:block></fo:marker>"""

    def title(id: String, text: String) =
      s"""<fo:block id="$id" font-family="sans-serif" font-size="16pt" font-weight="bold" keep-with-next="always" space-after="7mm" space-before="12mm">$text</fo:block>"""

    def renderer: ParallelRenderer[IO] = Renderer.of(XSLFO).io(blocker).parallel[IO].build
  }

  "The parallel renderer" should "render an empty tree" in {
    new ASTRenderer {
      val input = DocumentTree(Root, Nil)
      renderedTree.tree should be (RenderedTreeView(Root, Nil))
    }
  }

  it should "render a tree with a single document" in {
    new ASTRenderer {
      val input = DocumentTree(Root, List(Document(Root / "doc", rootElem)))
      renderedTree.tree should be (RenderedTreeView(Root, List(DocumentViews(List(RenderedDocumentView(Root / "doc.txt", expected))))))
    }
  }

  it should "render a tree with a single document to HTML using the default template" in {
    new HTMLRenderer {
      val input = DocumentTree(Root, List(Document(Root / "doc", rootElem)))
      val expected = RenderResult.html.withDefaultTemplate("Title", """<h1 id="title" class="title">Title</h1>
        |      <p>bbb</p>""".stripMargin)
      renderedTree.tree should be (RenderedTreeView(Root, List(DocumentViews(List(RenderedDocumentView(Root / "doc.html", expected))))))
    }
  }

  it should "fail with duplicate paths" in {
    new HTMLRenderer {
      val input = DocumentTree(Root, List(
        Document(Root / "doc1", rootElem),
        Document(Root / "doc2", rootElem),
        Document(Root / "doc2", rootElem),
        Document(Root / "sub" / "doc", rootElem),
        Document(Root / "sub" / "doc", rootElem)
      ))
      val res = renderer
        .from(treeRoot)
        .toOutput(StringTreeOutput)
        .render
        .attempt
        .unsafeRunSync()
      res shouldBe Left(
        RendererErrors(Seq(DuplicatePath(Root / "sub" / "doc"), DuplicatePath(Root / "doc2")))
      )
    }
  }

  it should "render a tree with a single document to HTML using a custom template in the root directory" in {
    new HTMLRenderer {
      val template = TemplateDocument(Root / "default.template.html", tRoot(tt("["), TemplateContextReference(Key("document.content"), required = true), tt("]")))
      val input = DocumentTree(Root, List(Document(Root / "doc", rootElem)), templates = Seq(template))
      val expected = """[<h1 id="title" class="title">Title</h1>
        |<p>bbb</p>]""".stripMargin
      renderedTree.tree should be (RenderedTreeView(Root, List(DocumentViews(List(RenderedDocumentView(Root / "doc.html", expected))))))
    }
  }

  it should "render a tree with a single document to HTML using a custom template in an extension bundle" in {
    new HTMLRenderer {
      val template = tRoot(tt("["), TemplateContextReference(Key("document.content"), required = true), tt("]"))
      val bundle = BundleProvider.forTheme(HTML.Theme(defaultTemplate = Some(template)))
      override lazy val renderer = Renderer.of(HTML).using(bundle).io(blocker).parallel[IO].build
      val input = DocumentTree(Root, List(Document(Root / "doc", rootElem)))
      val expected = """[<h1 id="title" class="title">Title</h1>
                       |<p>bbb</p>]""".stripMargin
      renderedTree.tree should be (RenderedTreeView(Root, List(DocumentViews(List(RenderedDocumentView(Root / "doc.html", expected))))))
    }
  }

  it should "render a tree with a cover and title document to HTML" in {
    new HTMLRenderer {
      val input = DocumentTree(Root, List(Document(Root / "doc", rootElem)), Some(Document(Root / "title", rootElem)))
      override def treeRoot = DocumentTreeRoot(input, coverDocument = Some(Document(Root / "cover", rootElem)))
      val expected = RenderResult.html.withDefaultTemplate("Title", """<h1 id="title" class="title">Title</h1>
                                                                      |      <p>bbb</p>""".stripMargin)
      renderedTree should be (RenderedTreeViewRoot(
        RenderedTreeView(Root, List( 
          TitleDocument(RenderedDocumentView(Root / "title.html", expected)),
          DocumentViews(List(RenderedDocumentView(Root / "doc.html", expected)))
        )),
        Some(RenderedDocumentView(Root / "cover.html", expected))
      ))
    }
  }

  it should "render a tree with a single document to EPUB.XHTML using the default template" in {
    new EPUB_XHTMLRenderer {
      val input = DocumentTree(Root, List(Document(Root / "doc", rootElem)))
      val expected = RenderResult.epub.withDefaultTemplate("Title", """<h1 id="title" class="title">Title</h1>
                                                                      |      <p>bbb</p>""".stripMargin)
      renderedTree.tree should be (RenderedTreeView(Root, List(DocumentViews(List(RenderedDocumentView(Root / "doc.epub.xhtml", expected))))))
    }
  }

  it should "render a tree with a single document to EPUB.XHTML using a custom template in the root directory" in {
    new EPUB_XHTMLRenderer {
      val template = TemplateDocument(Root / "default.template.epub.xhtml", tRoot(tt("["), TemplateContextReference(Key("document.content"), required = true), tt("]")))
      val input = DocumentTree(Root, List(Document(Root / "doc", rootElem)), templates = Seq(template))
      val expected = """[<h1 id="title" class="title">Title</h1>
                       |<p>bbb</p>]""".stripMargin
      renderedTree.tree should be (RenderedTreeView(Root, List(DocumentViews(List(RenderedDocumentView(Root / "doc.epub.xhtml", expected))))))
    }
  }

  it should "render a tree with a single document to EPUB.XHTML using a custom template in an extension bundle" in {
    new EPUB_XHTMLRenderer {
      val template = tRoot(tt("["), TemplateContextReference(Key("document.content"), required = true), tt("]"))
      override lazy val renderer = 
        Renderer
          .of(EPUB.XHTML)
          .using(BundleProvider.forTheme(EPUB.XHTML.Theme(defaultTemplate = Some(template))))
          .io(blocker)
          .parallel[IO]
          .build
      val input = DocumentTree(Root, List(Document(Root / "doc", rootElem)))
      val expected = """[<h1 id="title" class="title">Title</h1>
                       |<p>bbb</p>]""".stripMargin
      renderedTree.tree should be (RenderedTreeView(Root, List(DocumentViews(List(RenderedDocumentView(Root / "doc.epub.xhtml", expected))))))
    }
  }

  it should "render a tree with a single document to XSL-FO using the default template and default CSS" in {
    new FORenderer {
      val input = DocumentTree(Root, List(Document(Root / "doc", rootElem)))
      val expected = RenderResult.fo.withDefaultTemplate(s"""${marker("Title")}
        |      ${title("_doc_title", "Title")}
        |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>""".stripMargin)
      renderedTree.tree should be (RenderedTreeView(Root, List(DocumentViews(List(RenderedDocumentView(Root / "doc.fo", expected))))))
    }
  }

  it should "render a tree with a single document to XSL-FO using a custom template" in {
    new FORenderer {
      val template = TemplateDocument(Root / "default.template.fo", tRoot(tt("["), TemplateContextReference(Key("document.content"), required = true), tt("]")))
      val input = DocumentTree(Root, List(Document(Root / "doc", rootElem)), templates = Seq(template))
      val expected = s"""[${marker("Title")}
        |${title("_doc_title", "Title")}
        |<fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>]""".stripMargin
      renderedTree.tree should be (RenderedTreeView(Root, List(DocumentViews(List(RenderedDocumentView(Root / "doc.fo", expected))))))
    }
  }

  it should "render a tree with two documents to XSL-FO using a custom style sheet in an extension bundle" in {
    new FORenderer {
      override val renderer =
        Renderer
          .of(XSLFO)
          .using(BundleProvider.forTheme(XSLFO.Theme(defaultStyles = foStyles()("fo"))))
          .io(blocker)
          .parallel[IO]
          .build
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
      renderedTree.tree should be (RenderedTreeView(Root, List(
        DocumentViews(List(RenderedDocumentView(Root / "doc.fo", expectedRoot))),
        SubtreeViews(List(RenderedTreeView(Root / "tree", List(
          DocumentViews(List(RenderedDocumentView(Root / "tree" / "subdoc.fo", expectedSub)))
        ))))
      )))
    }
  }

  it should "render a tree with two documents to XSL-FO using a custom style sheet in the tree root" in {
    new FORenderer {
      val input = DocumentTree(Root, List(
        Document(Root / "doc", rootElem),
        DocumentTree(Root / "tree", List(Document(Root / "tree" / "subdoc", subElem)))
      ))
      override def treeRoot = DocumentTreeRoot(input, styles = foStyles(Root / "sub"))
      val expectedRoot = RenderResult.fo.withDefaultTemplate(s"""${marker("Title")}
        |      ${title("_doc_title", "Title")}
        |      <fo:block font-family="serif" font-size="11pt" space-after="3mm">bbb</fo:block>""".stripMargin)
      val expectedSub = RenderResult.fo.withDefaultTemplate(s"""${marker("Sub Title")}
        |      ${title("_tree_subdoc_sub-title", "Sub Title")}
        |      <fo:block font-family="serif" font-size="11pt" space-after="3mm">ccc</fo:block>""".stripMargin)
      renderedTree.tree should be (RenderedTreeView(Root, List(
        DocumentViews(List(RenderedDocumentView(Root / "doc.fo", expectedRoot))),
        SubtreeViews(List(RenderedTreeView(Root / "tree", List(
            DocumentViews(List(RenderedDocumentView(Root / "tree" / "subdoc.fo", expectedSub)))
          ))))
      )))
    }
  }

  it should "render a tree with a single static document" in new ASTRenderer with DocBuilder {
    val input = DocumentTree(Root, Nil)
    override def treeRoot = DocumentTreeRoot(input, staticDocuments = Seq(staticDoc(1).path))
    RenderedTreeView.toTreeView(renderer
      .from(treeRoot)
      .copying(Seq(ByteInput("...", Root / "static1.txt")))
      .toOutput(StringTreeOutput)
      .render.unsafeRunSync()
    ) should be (RenderedTreeViewRoot(RenderedTreeView(Root, Nil), staticDocuments = Seq(Root / "static1.txt")))
  }

  it should "render a tree with all available file types" in new ASTRenderer with DocBuilder {
    val input = addPosition(DocumentTree(Root,
      content = List(
        markupDoc(1),
        markupDoc(2),
        DocumentTree(Root / "dir1",
          content = List(markupDoc(3, Root / "dir1"), markupDoc(4, Root / "dir1"))
        ),
        DocumentTree(Root / "dir2",
          content = List(markupDoc(5, Root / "dir2"), markupDoc(6, Root / "dir2"))
        )
      )
    ))
    val staticDocs = Seq(
      staticDoc(1, Root),
      staticDoc(2, Root),
      staticDoc(3, Root / "dir1"),
      staticDoc(4, Root / "dir1"),
      staticDoc(5, Root / "dir2"),
      staticDoc(6, Root / "dir2")
    )
    override def treeRoot = DocumentTreeRoot(input, staticDocuments = staticDocs.map(_.path))
    
    val result = RenderedTreeView.toTreeView(renderer
      .from(treeRoot)
      .copying(staticDocs)
      .toOutput(StringTreeOutput)
      .render.unsafeRunSync()
    )
    
    val expectedStatic = staticDocs.map(_.path)
    val expectedRendered = RenderedTreeView(Root, List(
      DocumentViews(List(
        RenderedDocumentView(Root / "doc1.txt", renderedDoc(1)),
        RenderedDocumentView(Root / "doc2.txt", renderedDoc(2))
      )),
      SubtreeViews(List(
        RenderedTreeView(Root / "dir1", List(
          DocumentViews(List(
            RenderedDocumentView(Root / "dir1" / "doc3.txt", renderedDoc(3)),
            RenderedDocumentView(Root / "dir1" / "doc4.txt", renderedDoc(4))
         ))
      )),
      RenderedTreeView(Root / "dir2", List(
        DocumentViews(List(
          RenderedDocumentView(Root / "dir2" / "doc5.txt", renderedDoc(5)),
          RenderedDocumentView(Root / "dir2" / "doc6.txt", renderedDoc(6))
        ))
      ))))
    ))
    
    result shouldBe RenderedTreeViewRoot(expectedRendered, staticDocuments = expectedStatic)
  }
  
  trait GatherRenderer {
    val rootElem: RootElement = root(self.title("Title"), p("bbb"))
    val subElem: RootElement = root(self.title("Sub Title"), p("ccc"))

    val input = DocumentTree(Root, List(
      Document(Root / "doc", rootElem),
      DocumentTree(Root / "tree", List(
        Document(Root / "tree" / "sub", subElem)
      ))
    ))

    val expectedResult: String = """RootElement - Blocks: 2
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

  it should "render a tree with two documents using a RenderResultProcessor writing to an output stream" in new GatherRenderer {
    val out = new ByteArrayOutputStream
    Renderer
      .of(TestRenderResultProcessor)
      .io(blocker)
      .parallel[IO]
      .build
      .from(DocumentTreeRoot(input))
      .toStream(IO.pure(out))
      .render
      .unsafeRunSync()
    out.toString should be (expectedResult)
  }

  it should "render a tree with two documents using a RenderResultProcessor writing to a file" in new GatherRenderer {
    val f = File.createTempFile("output", null)
    Renderer
      .of(TestRenderResultProcessor)
      .io(blocker)
      .parallel[IO]
      .build
      .from(DocumentTreeRoot(input))
      .toFile(f)
      .render
      .unsafeRunSync()
    OutputBuilder.readFile(f) should be (expectedResult)
  }

  trait FileSystemTest extends DocBuilder {
    val input = DocumentTreeRoot(DocumentTree(Root, List(
      markupDoc(1),
      markupDoc(2),
      DocumentTree(Root / "dir1", List(
        markupDoc(3, Root / "dir1"),
        markupDoc(4, Root / "dir1")
      )),
      DocumentTree(Root / "dir2", List(
        markupDoc(5, Root / "dir2"),
        markupDoc(6, Root / "dir2")
      ))
    )))

    def readFiles (base: String): Assertion = {
      import laika.io.helper.OutputBuilder._
      readFile(base+"/doc1.txt") should be (renderedDoc(1))
      readFile(base+"/doc2.txt") should be (renderedDoc(2))
      readFile(base+"/dir1/doc3.txt") should be (renderedDoc(3))
      readFile(base+"/dir1/doc4.txt") should be (renderedDoc(4))
      readFile(base+"/dir2/doc5.txt") should be (renderedDoc(5))
      readFile(base+"/dir2/doc6.txt") should be (renderedDoc(6))
    }
  }

  it should "render to a directory using the toDirectory method" in {
    new FileSystemTest {
      val f = OutputBuilder.createTempDirectory("renderToDir")
      Renderer.of(AST)
        .io(blocker)
        .parallel[IO]
        .build
        .from(input)
        .toDirectory(f)
        .render
        .unsafeRunSync()
      readFiles(f.getPath)
    }
  }

  it should "render to a directory using a document with non-ASCII characters" in new DocBuilder {
    val expected = """RootElement - Blocks: 1
                     |. Paragraph - Spans: 1
                     |. . Text - 'Doc äöü'""".stripMargin
    val f = OutputBuilder.createTempDirectory("renderNonASCII")
    val input = DocumentTreeRoot(DocumentTree(Root, List(
      Document(Root / "doc", root(p("Doc äöü")))
    )))
    Renderer.of(AST)
      .io(blocker)
      .parallel[IO]
      .build
      .from(input)
      .toDirectory(f)(Codec.ISO8859)
      .render
      .unsafeRunSync()
    OutputBuilder.readFile(new File(f, "doc.txt"), Codec.ISO8859) should be (expected)
  }
  

}
  