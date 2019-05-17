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

package laika.render.epub

import laika.api.Render
import laika.ast.Path.Root
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.format.EPUB
import laika.io.{ByteInput, RenderedDocument, RenderedTree, StringTreeOutput}
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Jens Halm
  */
class XHTMLRendererSpec extends FlatSpec with Matchers with ModelBuilder {

  trait DocBuilder {

    def markupDoc (num: Int, path: Path = Root) = Document(path / ("doc"+num), root(p("Doc"+num)))

    def staticDoc (num: Int, path: Path = Root) = StaticDocument(ByteInput(s"p { margin: ${num}px; }", path / s"styles$num.css"))
  }

  trait StringRenderer {

    def renderedDocs (tree: DocumentTree): Seq[RenderedDocument] = {

      def collectDocuments (result: RenderedTree): Seq[RenderedDocument] =
        (result.content.collect { case doc: RenderedDocument => Seq(doc) } ++ 
          result.content.collect { case tree: RenderedTree => collectDocuments(tree) }).flatten

      val res = Render.as(EPUB.XHTML).from(DocumentTreeRoot(tree)).toOutputTree(StringTreeOutput).execute

      collectDocuments(res.rootTree)
    }

    def renderedXhtml (num: Int, style1: String, style2: String): String = s"""<?xml version="1.0" encoding="UTF-8"?>
     |<!DOCTYPE html>
     |<html xmlns="http://www.w3.org/1999/xhtml" xmlns:epub="http://www.idpf.org/2007/ops">
     |  <head>
     |    <meta charset="utf-8" />
     |    <meta name="generator" content="laika" />
     |    <title></title>
     |    <link rel="stylesheet" type="text/css" href="$style1" />
     |    <link rel="stylesheet" type="text/css" href="$style2" />
     |  </head>
     |  <body epub:type="bodymatter">
     |    <div class="content">
     |      <p>Doc$num</p>
     |    </div>
     |  </body>
     |</html>""".stripMargin

  }

  "The XHTML Renderer for EPUB" should "render a tree with 2 documents and 2 style sheets" in {
    new DocBuilder with StringRenderer {
      val input = DocumentTree(Root,
        content = List(
          markupDoc(1),
          DocumentTree(Root / "sub",
            content = List(markupDoc(2, Root / "sub")),
            additionalContent = List(staticDoc(2, Root / "sub"))
          )
        ),
        additionalContent = List(staticDoc(1))
      )

      val expected = Seq(
        RenderedDocument(Root / "doc1.epub.xhtml", Nil, Nil, renderedXhtml(1, "sub/styles2.css", "styles1.css")),
        RenderedDocument(Root / "sub" / "doc2.epub.xhtml", Nil, Nil, renderedXhtml(2, "styles2.css", "../styles1.css"))
      )

      renderedDocs(input) shouldBe expected
    }
  }

  it should "render a paragraph containing a citation link with an epub:type attribute" in {
    val elem = p(txt("some "), CitationLink("ref","label"), txt(" span"))
    Render.as(EPUB.XHTML).from(elem).toString should be ("""<p>some <a class="citation" href="#ref" epub:type="noteref">[label]</a> span</p>""")
  }

  it should "render a paragraph containing a footnote link with an epub:type attribute" in {
    val elem = p(txt("some "), FootnoteLink("id","label"), txt(" span"))
    Render.as(EPUB.XHTML).from(elem).toString should be ("""<p>some <a class="footnote" href="#id" epub:type="noteref">[label]</a> span</p>""")
  }

  it should "render a footnote with an epub:type attribute" in {
    val elem = Footnote("label", List(p("a"),p("b")), Id("id"))
    val html = """<aside id="id" class="footnote" epub:type="footnote">
     |  <p>a</p>
     |  <p>b</p>
     |</aside>""".stripMargin
    Render.as(EPUB.XHTML).from(elem).toString should be (html)
  }

  it should "render a citation with an epub:type attribute" in {
    val elem = Citation("ref", List(p("a"),p("b")), Id("ref"))
    val html = """<aside id="ref" class="citation" epub:type="footnote">
     |  <p>a</p>
     |  <p>b</p>
     |</aside>""".stripMargin
    Render.as(EPUB.XHTML).from(elem).toString should be (html)
  }


}
