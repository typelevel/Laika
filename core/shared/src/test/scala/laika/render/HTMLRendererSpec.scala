/*
 * Copyright 2012-2020 the original author or authors.
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

import laika.api.Renderer
import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentTree
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.format.HTML
import laika.parse.code.CodeCategory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HTMLRendererSpec extends AnyFlatSpec 
                       with Matchers
                       with ModelBuilder {
 
  
  def render (elem: Element): String = Renderer.of(HTML).build.render(elem) 
  
  def render (elem: Element, messageFilter: MessageFilter): String = 
    Renderer.of(HTML).renderMessages(messageFilter).build.render(elem)
    
  def renderUnformatted (elem: Element): String = Renderer.of(HTML).unformatted.build.render(elem)
  
  val imageTarget = InternalTarget(Root / "foo.jpg", CurrentTree / "foo.jpg")


  "The HTML renderer" should "render a paragraph with plain text" in {
    val elem = p("some text")
    render (elem) should be ("<p>some text</p>") 
  }
  
  it should "render a document with two paragraphs with plain text" in {
    val elem = root( p("aaa"), p("bbb"))
    val html = """<p>aaa</p>
      |<p>bbb</p>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a blockquote with two paragraphs with plain text" in {
    val elem = QuotedBlock( p("aaa"), p("bbb"))
    val html = """<blockquote>
      |  <p>aaa</p>
      |  <p>bbb</p>
      |</blockquote>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a block sequence with a style" in {
    val elem = root(BlockSequence(List(p("aaa"), p("bbb")), Styles("foo")))
    val html = """<div class="foo">
      |  <p>aaa</p>
      |  <p>bbb</p>
      |</div>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a block sequence without a style" in {
    val elem = root(p("aaa"), BlockSequence(p("bbb"), p("ccc")))
    val html = """<p>aaa</p>
      |<p>bbb</p>
      |<p>ccc</p>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a block sequence with a single element" in {
    val elem = root(p("aaa"), BlockSequence("bbb"), p("ccc"))
    val html = """<p>aaa</p>
      |<p>bbb</p>
      |<p>ccc</p>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a blockquote with simple flow content" in {
    val elem = QuotedBlock("aaa")
    val html = "<blockquote>aaa</blockquote>"
    render (elem) should be (html) 
  }
  
  it should "render a blockquote with an attribution" in {
    val elem = quote("aaa","bbb")
    val html = """<blockquote>
      |  <p>aaa</p>
      |  <p class="attribution">bbb</p>
      |</blockquote>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a bullet list with simple flow content" in {
    val elem = (bulletList() + "aaa" + "bbb").toList
    val html = """<ul>
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ul>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with simple flow content" in {
    val elem = enumList() + "aaa" + "bbb"
    val html = """<ol class="arabic">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with lower roman enumeration style" in {
    val elem = enumList(EnumFormat(EnumType.LowerRoman)) + "aaa" + "bbb"
    val html = """<ol class="lowerroman">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with upper roman enumeration style" in {
    val elem = enumList(EnumFormat(EnumType.UpperRoman)) + "aaa" + "bbb"
    val html = """<ol class="upperroman">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with lower alpha enumeration style" in {
    val elem = enumList(EnumFormat(EnumType.LowerAlpha)) + "aaa" + "bbb"
    val html = """<ol class="loweralpha">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with upper alpha enumeration style" in {
    val elem = enumList(EnumFormat(EnumType.UpperAlpha)) + "aaa" + "bbb"
    val html = """<ol class="upperalpha">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with the start value if it is not 1" in {
    val elem = enumList(EnumFormat(EnumType.Arabic), 7) + "aaa" + "bbb"
    val html = """<ol class="arabic" start="7">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  private def fp (content: String) = ForcedParagraph(List(Text(content)))
  
  it should "render a bullet list with forced paragraphs as list items" in {
    val elem = (bulletList() + fp("aaa") + fp("bbb")).toList
    val html = """<ul>
      |  <li>
      |    <p>aaa</p>
      |  </li>
      |  <li>
      |    <p>bbb</p>
      |  </li>
      |</ul>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with forced paragraphs as list items" in {
    val elem = enumList() + fp("aaa") + fp("bbb")
    val html = """<ol class="arabic">
      |  <li>
      |    <p>aaa</p>
      |  </li>
      |  <li>
      |    <p>bbb</p>
      |  </li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a definition list with paragraphs" in {
    val elem = defList + ("term 1", p("1"), p("1")) + ("term 2", p("2"), p("2"))
    val html = """<dl>
      |  <dt>term 1</dt>
      |  <dd>
      |    <p>1</p>
      |    <p>1</p>
      |  </dd>
      |  <dt>term 2</dt>
      |  <dd>
      |    <p>2</p>
      |    <p>2</p>
      |  </dd>
      |</dl>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a definition list with simple flow content" in {
    val elem = defList + ("term 1", p("1")) + ("term 2", p("2"))
    val html = """<dl>
      |  <dt>term 1</dt>
      |  <dd>1</dd>
      |  <dt>term 2</dt>
      |  <dd>2</dd>
      |</dl>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a navigation list" in {
    val elem = NavigationList(Seq(
      NavigationLink(SpanSequence("Link-1"), InternalTarget.fromPath(Root / "doc-1", Root), Seq(
        NavigationLink(SpanSequence("Link-2"), InternalTarget.fromPath(Root / "doc-2", Root), Nil, options = Style.level(2))
      ), options = Style.level(1)),
      NavigationHeader(SpanSequence("Header-3"), Seq(
        NavigationLink(SpanSequence("Link-4"), InternalTarget.fromPath(Root / "doc-4", Root), Nil, selfLink = true, options = Style.level(2))
      ), options = Style.level(1)),
      NavigationLink(SpanSequence("Link-5"), InternalTarget.fromPath(Root / "doc-5", Root), Nil,
        options = Style.level(1))
    ))
    val html = """<ul>
                 |  <li>
                 |    <p class="level1"><a href="doc-1">Link-1</a></p>
                 |    <ul>
                 |      <li><span class="level2"><a href="doc-2">Link-2</a></span></li>
                 |    </ul>
                 |  </li>
                 |  <li>
                 |    <p class="level1 nav-header">Header-3</p>
                 |    <ul>
                 |      <li><span class="level2 active"><a href="doc-4">Link-4</a></span></li>
                 |    </ul>
                 |  </li>
                 |  <li><span class="level1"><a href="doc-5">Link-5</a></span></li>
                 |</ul>""".stripMargin
    render (elem) should be (html)
  }
  
  it should "render a footnote" in {
    val elem = Footnote("label", List(p("a"),p("b")), Id("id"))
    val html = """<table id="id" class="footnote">
      |  <colgroup>
      |    <col class="label"></col>
      |    <col></col>
      |  </colgroup>
      |  <tbody>
      |    <tr>
      |      <td>[label]</td>
      |      <td>
      |        <p>a</p>
      |        <p>b</p>
      |      </td>
      |    </tr>
      |  </tbody>
      |</table>""".stripMargin
    render(elem) should be (html)
  }
  
  it should "render a citation" in {
    val elem = Citation("ref", List(p("a"),p("b")), Id("ref"))
    val html = """<table id="ref" class="citation">
      |  <colgroup>
      |    <col class="label"></col>
      |    <col></col>
      |  </colgroup>
      |  <tbody>
      |    <tr>
      |      <td>[ref]</td>
      |      <td>
      |        <p>a</p>
      |        <p>b</p>
      |      </td>
      |    </tr>
      |  </tbody>
      |</table>""".stripMargin
    render(elem) should be (html)
  }
  
  it should "render a table without header cells" in {
    val elem = Table(Row(BodyCell("a"),BodyCell("b")),Row(BodyCell("c"),BodyCell("d")))
    val html = """<table>
      |  <tbody>
      |    <tr>
      |      <td>a</td>
      |      <td>b</td>
      |    </tr>
      |    <tr>
      |      <td>c</td>
      |      <td>d</td>
      |    </tr>
      |  </tbody>
      |</table>""".stripMargin
    render(elem) should be (html)
  }
  
  it should "render a table with header cells" in {
    val elem = Table(TableHead(List(Row(BodyCell("a"), BodyCell("b")))),
                     TableBody(List(Row(BodyCell("c"), BodyCell("d")))))
    val html = """<table>
      |  <thead>
      |    <tr>
      |      <td>a</td>
      |      <td>b</td>
      |    </tr>
      |  </thead>
      |  <tbody>
      |    <tr>
      |      <td>c</td>
      |      <td>d</td>
      |    </tr>
      |  </tbody>
      |</table>""".stripMargin
    render(elem) should be (html)
  }
  
  it should "render a table with a caption" in {
    val caption = Caption(List(Text("caption")))
    val elem = Table(Row(BodyCell("a"),BodyCell("b")),Row(BodyCell("c"),BodyCell("d"))).copy(caption = caption)
    val html = """<table>
      |  <caption>caption</caption>
      |  <tbody>
      |    <tr>
      |      <td>a</td>
      |      <td>b</td>
      |    </tr>
      |    <tr>
      |      <td>c</td>
      |      <td>d</td>
      |    </tr>
      |  </tbody>
      |</table>""".stripMargin
    render(elem) should be (html)
  }
  
  it should "render a cell using colspan and rowspan attributes" in {
    val elem = cell("a",3,2)
    val html = """<td colspan="3" rowspan="2">a</td>"""
    render(elem) should be (html)
  } 
  
  it should "render a cell with two paragraphs" in {
    val elem = BodyCell(p("a"),p("b"))
    val html = """<td>
      |  <p>a</p>
      |  <p>b</p>
      |</td>""".stripMargin
    render(elem) should be (html)
  } 
  
  it should "render a titled block" in {
    val elem = TitledBlock(List(Text("some "), Emphasized("em"), Text(" text")), List(p("aaa"), Rule(), p("bbb")))
    val html = """<div>
      |  <p class="title">some <em>em</em> text</p>
      |  <p>aaa</p>
      |  <hr>
      |  <p>bbb</p>
      |</div>""".stripMargin
    render(elem) should be (html)
  }
  
  it should "render a figure" in {
    val elem = Figure(Image("alt", InternalTarget(Root / "image.jpg", CurrentTree / "image.jpg")), List(Text("some "), Emphasized("caption"), Text(" text")), List(p("aaa"), Rule(), p("bbb")))
    val html = """<div class="figure">
      |  <img src="image.jpg" alt="alt">
      |  <p class="caption">some <em>caption</em> text</p>
      |  <div class="legend">
      |    <p>aaa</p>
      |    <hr>
      |    <p>bbb</p>
      |  </div>
      |</div>""".stripMargin
    render(elem) should be (html)
  }
  
  it should "render nested line blocks" in {
    val elem = LineBlock(LineBlock(Line("1"),Line("2")), Line("3"))
    val html = """<div class="line-block">
      |  <div class="line-block">
      |    <div class="line">1</div>
      |    <div class="line">2</div>
      |  </div>
      |  <div class="line">3</div>
      |</div>""".stripMargin
    render(elem) should be (html)
  }
  
  it should "render a document with two paragraphs separated by a horizontal rule" in {
    val elem = root( p("aaa"), Rule(), p("bbb"))
    val html = """<p>aaa</p>
      |<hr>
      |<p>bbb</p>""".stripMargin
    render (elem) should be (html) 
  } 
  
  it should "render a document with two nested sections" in {
    val nested = Section(Header(2, Text("Title 2")), List(p("Line 1"), p("Line 2")))
    val rootElem = root(Section(Header(1, Text("Title 1")), List(p("Line 1"), p("Line 2"))), nested)
    val html = """
      |<h1>Title 1</h1>
      |<p>Line 1</p>
      |<p>Line 2</p>
      |
      |<h2>Title 2</h2>
      |<p>Line 1</p>
      |<p>Line 2</p>""".stripMargin
    render (rootElem) should be (html) 
  }
  
  it should "render a title containing emphasized text" in {
    val elem = Title(Text("some "), Emphasized("em"), Text(" text"))
    render (elem) should be ("<h1>some <em>em</em> text</h1>") 
  }
  
  it should "render a title containing a section number" in {
    val elem = Title(SectionNumber(Seq(1,2,3)), Text(" Title"))
    render (elem) should be ("""<h1><span class="section-number">1.2.3 </span> Title</h1>""") 
  }
  
  it should "render a paragraph containing emphasized text" in {
    val elem = p(Text("some "), Emphasized("em"), Text(" text"))
    render (elem) should be ("<p>some <em>em</em> text</p>") 
  }
  
  it should "render a paragraph containing strong text" in {
    val elem = p(Text("some "), Strong("strong"), Text(" text")) 
    render (elem) should be ("<p>some <strong>strong</strong> text</p>") 
  }

  it should "render a paragraph containing a deleted span" in {
    val elem = p(Text("some "), Deleted("deleted"), Text(" text"))
    render (elem) should be ("<p>some <del>deleted</del> text</p>")
  }

  it should "render a paragraph containing an inserted span" in {
    val elem = p(Text("some "), Inserted("inserted"), Text(" text"))
    render (elem) should be ("<p>some <ins>inserted</ins> text</p>")
  }
  
  it should "render a paragraph containing a literal span" in {
    val elem = p(Text("some "), Literal("code"), Text(" span"))
    render (elem) should be ("<p>some <code>code</code> span</p>") 
  }
  
  it should "render a paragraph containing a code span" in {
    val elem = p(Text("some "), InlineCode("banana-script", List(Text("code"))), Text(" span"))
    render (elem) should be ("<p>some <code class=\"banana-script\">code</code> span</p>") 
  }
  
  it should "render a paragraph containing a link without title" in {
    val elem = p(Text("some "), link(Text("link")).url("/foo"), Text(" span"))
    render (elem) should be ("""<p>some <a href="/foo">link</a> span</p>""") 
  }
  
  it should "render a paragraph containing a link with title" in {
    val elem = p(Text("some "), link(Text("link")).url("/foo").title("title"), Text(" span"))
    render (elem) should be ("""<p>some <a href="/foo" title="title">link</a> span</p>""") 
  }
  
  it should "render a paragraph containing a link with emphasized text" in {
    val elem = p(Text("some "), link(Text("link"),Emphasized("text")).url("/foo"), Text(" span"))
    render (elem) should be ("""<p>some <a href="/foo">link<em>text</em></a> span</p>""") 
  }
  
  it should "render a paragraph containing an internal link with emphasized text" in {
    val elem = p(Text("some "), SpanLink(List(Text("link"),Emphasized("text")),InternalTarget(Path.parse("/#foo"),RelativePath.parse("#foo"))), Text(" span"))
    render (elem) should be ("""<p>some <a href="#foo">link<em>text</em></a> span</p>""") 
  }
  
  it should "render a paragraph containing a internal link with a fragment part" in {
    val elem = p(Text("some "), SpanLink(List(Text("link"),Emphasized("text")), InternalTarget(Path.parse("/bar#foo"),RelativePath.parse("../bar.md#foo"))), Text(" span"))
    render (elem) should be ("""<p>some <a href="../bar.html#foo">link<em>text</em></a> span</p>""") 
  }
  
  it should "render a paragraph containing a internal link without a fragment part" in {
    val elem = p(Text("some "), SpanLink(List(Text("link"),Emphasized("text")), InternalTarget(Path.parse("/bar"),RelativePath.parse("../bar.md"))), Text(" span"))
    render (elem) should be ("""<p>some <a href="../bar.html">link<em>text</em></a> span</p>""") 
  }
  
  it should "render a paragraph containing a internal link with a filename without suffix" in {
    val elem = p(Text("some "), SpanLink(List(Text("link"),Emphasized("text")), InternalTarget(Path.parse("/bar"),RelativePath.parse("../bar"))), Text(" span"))
    render (elem) should be ("""<p>some <a href="../bar">link<em>text</em></a> span</p>""") 
  }
  
  it should "render a paragraph containing a citation link" in {
    val elem = p(Text("some "), CitationLink("ref","label"), Text(" span"))
    render (elem) should be ("""<p>some <a class="citation" href="#ref">[label]</a> span</p>""") 
  }
  
  it should "render a paragraph containing a footnote link" in {
    val elem = p(Text("some "), FootnoteLink("id","label"), Text(" span"))
    render (elem) should be ("""<p>some <a class="footnote" href="#id">[label]</a> span</p>""") 
  }
  
  it should "render a paragraph containing an image without title" in {
    val elem = p(Text("some "), Image("img", imageTarget), Text(" span"))
    render (elem) should be ("""<p>some <img src="foo.jpg" alt="img"> span</p>""") 
  }
  
  it should "render a paragraph containing an image with title" in {
    val elem = p(Text("some "), Image("img", imageTarget, title = Some("title")), Text(" span"))
    render (elem) should be ("""<p>some <img src="foo.jpg" alt="img" title="title"> span</p>""") 
  }

  it should "render a paragraph containing an image with width and height in pixels" in {
    val image = Image("img", imageTarget, width = Some(Size(200,"px")), height = Some(Size(120,"px")))
    val elem = p(Text("some "), image, Text(" span"))
    render (elem) should be ("""<p>some <img src="foo.jpg" alt="img" width="200" height="120"> span</p>""")
  }

  it should "render a paragraph containing an image with width and height in a unit other than pixels" in {
    val image = Image("img", imageTarget, width = Some(Size(12.4,"in")), height = Some(Size(6.8,"in")))
    val elem = p(Text("some "), image, Text(" span"))
    render (elem) should be ("""<p>some <img src="foo.jpg" alt="img" style="width:12.4in;height:6.8in"> span</p>""")
  }

  it should "render a paragraph containing an image with just width in a unit other than pixels" in {
    val image = Image("img", imageTarget, width = Some(Size(12.4,"in")))
    val elem = p(Text("some "), image, Text(" span"))
    render (elem) should be ("""<p>some <img src="foo.jpg" alt="img" style="width:12.4in"> span</p>""")
  }
  
  it should "render a paragraph containing an unresolved link reference" in {
    val elem = p(Text("some "), linkRef(Text("link")).id("id").source("[link] [id]"), Text(" span"))
    render (elem) should be ("""<p>some [link] [id] span</p>""")
  }
  
  it should "render a paragraph containing an unresolved image reference" in {
    val elem = p(Text("some "), imgRef("img","id","![img] [id]"), Text(" span"))
    render (elem) should be ("""<p>some ![img] [id] span</p>""") 
  }
  
  it should "render a paragraph containing an internal link target" in {
    val elem = p(Text("some "), InternalLinkTarget(Id("target")), Text(" span"))
    render (elem) should be ("""<p>some <a id="target"></a> span</p>""") 
  }
  
  it should "render a template root containing string elements" in {
    val elem = TemplateRoot(t("aa"),t("bb"),t("cc"))
    render (elem) should be ("aabbcc")
  }
  
  it should "render a template span sequence containing string elements" in {
    val elem = TemplateSpanSequence(t("aa"),t("bb"),t("cc"))
    render (elem) should be ("aabbcc")
  }
  
  it should "render a template string without creating html entities" in {
    val elem = TemplateRoot(t("aa & bb"))
    render (elem) should be ("aa & bb")
  }
  
  it should "render a template root containing a TemplateElement" in {
    val elem = TemplateRoot(t("aa"),TemplateElement(BlockSequence(List(p("aaa"), p("bbb")),Styles("foo"))),t("cc"))
    val html = """aa<div class="foo">
      |  <p>aaa</p>
      |  <p>bbb</p>
      |</div>cc""".stripMargin
    render (elem) should be (html)
  }

  it should "render a system message" in {
    val html = """<span class="system-message warning">some message</span>"""
    render(RuntimeMessage(MessageLevel.Warning, "some message"), MessageFilter.Warning) should be (html)
  }
  
  it should "render a comment" in {
    render(Comment("foo")) should be ("<!-- foo -->")
  }
  
  it should "render an invalid block without the system message in default mode" in {
    val elem = InvalidBlock(RuntimeMessage(MessageLevel.Warning, "some message"), p("fallback"))
    val html = "<p>fallback</p>"
    render(elem) should be (html)
  }
  
  it should "render an invalid block without the system message if the configured message level is higher" in {
    val elem = InvalidBlock(RuntimeMessage(MessageLevel.Warning, "some message"), p("fallback"))
    val html = "<p>fallback</p>"
    render(elem, MessageFilter.Error) should be (html)
  }
  
  it should "render an invalid block with the system message if the configured message level is lower or equal" in {
    val elem = InvalidBlock(RuntimeMessage(MessageLevel.Warning, "some message"), p("fallback"))
    val html = """<p><span class="system-message warning">some message</span></p><p>fallback</p>"""
    render(elem, MessageFilter.Info) should be (html)
  }
  
  it should "render an invalid span without the system message in default mode" in {
    val elem = InvalidSpan(RuntimeMessage(MessageLevel.Warning, "some message"), Text("fallback"))
    render(elem) should be ("fallback")
  }
  
  it should "render an invalid span without the system message if the configured message level is higher" in {
    val elem = InvalidSpan(RuntimeMessage(MessageLevel.Warning, "some message"), Text("fallback"))
    render(elem, MessageFilter.Error) should be ("fallback")
  }
  
  it should "render an invalid span with the system message if the configured message level is lower or equal" in {
    val elem = InvalidSpan(RuntimeMessage(MessageLevel.Warning, "some message"), Text("fallback"))
    val html = """<span class="system-message warning">some message</span> fallback"""
    render(elem, MessageFilter.Info) should be (html)
  }
  
  it should "render a literal block" in {
    val code = """line 1
      |
      |    <line 2
      |
      |line 3""".stripMargin
    val elem = LiteralBlock(code)
    render(elem) should be ("<pre><code>" + code.replaceAllLiterally("<", "&lt;") + "</code></pre>") 
  }
  
  it should "render a parsed literal block" in {
    val code = """line 1
      |
      |    #<line 2
      |
      |line 3""".stripMargin.split("#")
    val elem = ParsedLiteralBlock(List(Text(code(0)), Emphasized("em"), Text(code(1))))
    val html = "<pre><code>" + code(0) + "<em>em</em>" + code(1).replaceAllLiterally("<", "&lt;") + "</code></pre>"
    render(elem) should be (html) 
  }
  
  it should "render a code block" in {
    val code = """line 1
      |
      |    <line 2
      |
      |line 3""".stripMargin
    val elem = CodeBlock("banana-script", List(Text(code)))
    render(elem) should be ("<pre><code class=\"banana-script\">" + code.replaceAllLiterally("<", "&lt;") + "</code></pre>") 
  }

  it should "render a code block with syntax highlighting" in {
    val code = List(
      CodeSpan("{{", CodeCategory.Keyword),
      CodeSpan("foo"), 
      CodeSpan("}}", Set(CodeCategory.Tag.Punctuation, CodeCategory.Identifier))
    )
    val elem = CodeBlock("banana-script", code)
    val renderedCode = """<span class="keyword">{{</span><span>foo</span><span class="tag-punctuation identifier">}}</span>"""
    render(elem) should be (s"""<pre><code class="nohighlight">$renderedCode</code></pre>""")
  }
  
  it should "render a literal block inside a blockquote without indentation" in {
    val code = """line 1
      |
      |    line 2
      |
      |line 3""".stripMargin
    val html = """<blockquote>
      |  <pre><code>%s</code></pre>
      |</blockquote>""".stripMargin.format(code)
    val elem = QuotedBlock(LiteralBlock(code))
    render(elem) should be (html) 
  }
  
  it should "render a parsed literal block inside a blockquote without indentation" in {
    val code = """line 1
      |
      |    line 2
      |
      |line 3""".stripMargin
    val html = """<blockquote>
      |  <pre><code>:<em>%s</em>:</code></pre>
      |</blockquote>""".stripMargin.format(code)
    val elem = QuotedBlock(ParsedLiteralBlock(List(Text(":"),Emphasized(code),Text(":"))))
    render(elem) should be (html) 
  }
  
  it should "render a code block inside a blockquote without indentation" in {
    val code = """line 1
      |
      |    line 2
      |
      |line 3""".stripMargin
    val html = """<blockquote>
      |  <pre><code class="nohighlight">:<em>%s</em>:</code></pre>
      |</blockquote>""".stripMargin.format(code)
    val elem = QuotedBlock(CodeBlock("banana-script", List(Text(":"),Emphasized(code),Text(":"))))
    render(elem) should be (html) 
  }
  
  it should "render a table cell unformatted" in {
    val elem = BodyCell(p("a"),p("b"))
    val html = """<td>
      |<p>a</p>
      |<p>b</p>
      |</td>""".stripMargin
    renderUnformatted(elem) should be (html)
  } 
  
  it should "render raw content unchanged if the html format is specified" in {
    val raw = "<span>foo</span>"
    val elem = RawContent(List("html", "spooky"), raw)
    render(elem) should be (raw) 
  }
  
  it should "ignore raw content if the html format is not specified" in {
    val raw = "<span>foo</span>"
    val elem = RawContent(List("dodgy", "spooky"), raw)
    render(elem) should be ("") 
  }
  
  it should "render an embedded root with correct indentation" in {
    val elem = root(TemplateRoot(
      t("<div>\n  "),
      EmbeddedRoot(List(p("aaa"),p("bbb")), 2),
      t("\n</div>")
    ))
    val html = """<div>
      |  <p>aaa</p>
      |  <p>bbb</p>
      |</div>""".stripMargin
    render(elem) should be (html) 
  }
  
  it should "render an embedded root without indentation" in {
    val elem = root(TemplateRoot(
      t("<div>\n"),
      EmbeddedRoot(p("aaa"),p("bbb")),
      t("\n</div>")
    ))
    val html = """<div>
      |<p>aaa</p>
      |<p>bbb</p>
      |</div>""".stripMargin
    render(elem) should be (html) 
  }
  
  
}
