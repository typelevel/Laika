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

import cats.data.NonEmptySet
import laika.api.Renderer
import laika.ast.Path.Root
import laika.ast._
import laika.ast.sample.{ParagraphCompanionShortcuts, TestSourceBuilders}
import laika.format.HTML
import laika.parse.GeneratedSource
import laika.parse.code.CodeCategory
import laika.rewrite.nav.TargetFormats
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HTMLRendererSpec extends AnyFlatSpec 
  with Matchers
  with ParagraphCompanionShortcuts
  with TestSourceBuilders {
 
  
  def render (elem: Element): String = Renderer.of(HTML).build.render(elem) 
  
  def render (elem: Element, messageFilter: MessageFilter): String = 
    Renderer.of(HTML).renderMessages(messageFilter).build.render(elem)
    
  def renderUnformatted (elem: Element): String = Renderer.of(HTML).unformatted.build.render(elem)
  
  private val imageTarget = InternalTarget(Root / "foo.jpg")
  
  def testPar (span: Span): Paragraph = p(Text("some "), span, Text(" span"))


  "The HTML renderer" should "render a paragraph with plain text" in {
    val elem = p("some text")
    render (elem) should be ("<p>some text</p>") 
  }
  
  it should "render a document with two paragraphs with plain text" in {
    val elem = RootElement( p("aaa"), p("bbb"))
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
    val elem = RootElement(BlockSequence(List(p("aaa"), p("bbb")), Styles("foo")))
    val html = """<div class="foo">
      |  <p>aaa</p>
      |  <p>bbb</p>
      |</div>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a block sequence without a style" in {
    val elem = RootElement(p("aaa"), BlockSequence(p("bbb"), p("ccc")))
    val html = """<p>aaa</p>
      |<p>bbb</p>
      |<p>ccc</p>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a block sequence with a single element" in {
    val elem = RootElement(p("aaa"), BlockSequence("bbb"), p("ccc"))
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
    val elem = QuotedBlock(List(p("aaa")), List(Text("bbb")))
    val html = """<blockquote>
      |  <p>aaa</p>
      |  <p class="attribution">bbb</p>
      |</blockquote>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a bullet list with simple flow content" in {
    val elem = RootElement(BulletList("aaa","bbb"))
    val html = """<ul>
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ul>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with simple flow content" in {
    val elem = EnumList("aaa", "bbb")
    val html = """<ol class="arabic">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with lower roman enumeration style" in {
    val elem = EnumList(EnumFormat(EnumType.LowerRoman))("aaa", "bbb")
    val html = """<ol class="lowerroman">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with upper roman enumeration style" in {
    val elem = EnumList(EnumFormat(EnumType.UpperRoman))("aaa", "bbb")
    val html = """<ol class="upperroman">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with lower alpha enumeration style" in {
    val elem = EnumList(EnumFormat(EnumType.LowerAlpha))("aaa", "bbb")
    val html = """<ol class="loweralpha">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with upper alpha enumeration style" in {
    val elem = EnumList(EnumFormat(EnumType.UpperAlpha))("aaa", "bbb")
    val html = """<ol class="upperalpha">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with the start value if it is not 1" in {
    val elem = EnumList(EnumFormat(EnumType.Arabic), 7)("aaa", "bbb")
    val html = """<ol class="arabic" start="7">
      |  <li>aaa</li>
      |  <li>bbb</li>
      |</ol>""".stripMargin
    render (elem) should be (html) 
  }
  
  private def fp (content: String) = ForcedParagraph(List(Text(content)))
  
  it should "render a bullet list with forced paragraphs as list items" in {
    val elem = BulletList(fp("aaa"), fp("bbb"))
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
    val elem = EnumList(fp("aaa"), fp("bbb"))
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
    val elem = DefinitionList(
      DefinitionListItem("term 1", p("1"), p("1")),
      DefinitionListItem("term 2", p("2"), p("2"))
    )
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
    val elem = DefinitionList(
      DefinitionListItem("term 1", p("1")),
      DefinitionListItem("term 2", p("2"))
    )
    val html = """<dl>
      |  <dt>term 1</dt>
      |  <dd>1</dd>
      |  <dt>term 2</dt>
      |  <dd>2</dd>
      |</dl>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a navigation list" in {
    val refPath = Root / "doc-99"
    val elem = NavigationList(Seq(
      NavigationItem(
        SpanSequence("Link-1"), 
        Seq(NavigationItem(
          SpanSequence("Link-2"),
          Nil,
          Some(NavigationLink(InternalTarget(Root / "doc-2").relativeTo(refPath))), 
          options = Style.level(2)
        )),
        Some(NavigationLink(InternalTarget(Root / "doc-1").relativeTo(refPath))), 
        options = Style.level(1)
      ),
      NavigationItem(
        SpanSequence("Header-3"), 
        Seq(NavigationItem(
          SpanSequence("Link-4"), Nil,
          Some(NavigationLink(InternalTarget(Root / "doc-4").relativeTo(refPath), selfLink = true)), 
          options = Style.level(2)
        )), 
        options = Style.level(1)),
      NavigationItem(
        SpanSequence("Link-5"),
        Nil,
        Some(NavigationLink(InternalTarget(Root / "doc-5").relativeTo(refPath))), 
        options = Style.level(1)
      )
    ))
    val html =
      """<ul class="nav-list">
        |  <li class="level1"><a href="doc-1.html">Link-1</a></li>
        |  <li class="level2"><a href="doc-2.html">Link-2</a></li>
        |  <li class="level1 nav-header">Header-3</li>
        |  <li class="level2 active"><a href="doc-4.html">Link-4</a></li>
        |  <li class="level1"><a href="doc-5.html">Link-5</a></li>
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
    val elem = Cell(BodyCell, List(p("a")), 3, 2)
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
    val elem = Figure(Image(InternalTarget(Root / "image.jpg"), alt = Some("alt")), List(Text("some "), Emphasized("caption"), Text(" text")), List(p("aaa"), Rule(), p("bbb")))
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
  
  it should "render a document with two paragraphs separated by a horizontal rule" in {
    val elem = RootElement( p("aaa"), Rule(), p("bbb"))
    val html = """<p>aaa</p>
      |<hr>
      |<p>bbb</p>""".stripMargin
    render (elem) should be (html) 
  } 
  
  it should "render a document with two nested sections" in {
    val nested = Section(Header(2, Text("Title 2")), List(p("Line 1"), p("Line 2")))
    val rootElem = RootElement(Section(Header(1, Text("Title 1")), List(p("Line 1"), p("Line 2"))), nested)
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
  
  it should "render a selection" in {
    val group = Selection("config", Seq(
      Choice("name-a","label-a", List(p("common"), p("11\n22"))),
      Choice("name-b","label-b", List(p("common"), p("33\n44")))
    ))
    val elem = RootElement( p("aaa"), group, p("bbb"))
    val html = """<p>aaa</p>
                 |<p><strong>label-a</strong></p>
                 |<p>common</p>
                 |<p>11
                 |22</p>
                 |<p><strong>label-b</strong></p>
                 |<p>common</p>
                 |<p>33
                 |44</p>
                 |<p>bbb</p>""".stripMargin
    render (elem) should be (html)
  }
  
  it should "render a choice group" ignore {
    val group = Selection("config", Seq(
      Choice("name-a","label-a", List(p("common"), p("11\n22"))),
      Choice("name-b","label-b", List(p("common"), p("33\n44")))
    ))
    val elem = RootElement( p("aaa"), group, p("bbb"))
    val html = """<p>aaa</p>
                 |<div class="tab-container" data-tab-group="config">
                 |  <ul class="tab-group">
                 |    <li class="tab" data-choice-name="name-a">label-a</li>
                 |    <li class="tab" data-choice-name="name-b">label-b</li>
                 |  </ul>
                 |  <div class="tab-content" data-choice-name="name-a">
                 |    <p>common</p>
                 |    <p>11
                 |    22</p>
                 |  </div>
                 |  <div class="tab-content" data-choice-name="name-b">
                 |    <p>common</p>
                 |    <p>33
                 |    44</p>
                 |  </div>
                 |</div>
                 |<p>bbb</p>""".stripMargin
    render (elem) should be (html)
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
    val elem = testPar(Emphasized("em"))
    render (elem) should be ("<p>some <em>em</em> span</p>") 
  }
  
  it should "render a paragraph containing strong text" in {
    val elem = testPar(Strong("strong")) 
    render (elem) should be ("<p>some <strong>strong</strong> span</p>") 
  }

  it should "render a paragraph containing a deleted span" in {
    val elem = testPar(Deleted("deleted"))
    render (elem) should be ("<p>some <del>deleted</del> span</p>")
  }

  it should "render a paragraph containing an inserted span" in {
    val elem = testPar(Inserted("inserted"))
    render (elem) should be ("<p>some <ins>inserted</ins> span</p>")
  }
  
  it should "render a paragraph containing a literal span" in {
    val elem = testPar(Literal("code"))
    render (elem) should be ("<p>some <code>code</code> span</p>") 
  }
  
  it should "render a paragraph containing a code span" in {
    val elem = testPar(InlineCode("banana-script", List(Text("code"))))
    render (elem) should be ("<p>some <code class=\"banana-script\">code</code> span</p>") 
  }
  
  it should "render a paragraph containing a link without title" in {
    val elem = testPar(SpanLink.external("/foo")("link"))
    render (elem) should be ("""<p>some <a href="/foo">link</a> span</p>""") 
  }
  
  it should "render a paragraph containing a link with title" in {
    val elem = testPar(SpanLink.external("/foo")("link").copy(title = Some("title")))
    render (elem) should be ("""<p>some <a href="/foo" title="title">link</a> span</p>""") 
  }
  
  it should "render a paragraph containing a link with emphasized text" in {
    val elem = testPar(SpanLink.external("/foo")(Text("link"), Emphasized("text")))
    render (elem) should be ("""<p>some <a href="/foo">link<em>text</em></a> span</p>""") 
  }
  
  it should "render a paragraph containing an internal link with emphasized text" in {
    val elem = testPar(SpanLink.internal("#foo")(Text("link"), Emphasized("text")))
    render (elem) should be ("""<p>some <a href="#foo">link<em>text</em></a> span</p>""") 
  }
  
  it should "render a paragraph containing a internal link with a fragment part" in {
    val elem = testPar(SpanLink.internal("/bar#foo")(Text("link"), Emphasized("text")))
    render (elem) should be ("""<p>some <a href="bar.html#foo">link<em>text</em></a> span</p>""") 
  }
  
  it should "render a paragraph containing a internal link without a fragment part" in {
    val elem = testPar(SpanLink.internal("/bar")(Text("link"), Emphasized("text")))
    render (elem) should be ("""<p>some <a href="bar.html">link<em>text</em></a> span</p>""") 
  }
  
  it should "render a paragraph containing a internal link with an input filename without suffix" in {
    val elem = testPar(SpanLink.internal("/bar")(Text("link"), Emphasized("text")))
    render (elem) should be ("""<p>some <a href="bar.html">link<em>text</em></a> span</p>""") 
  }

  it should "render a paragraph containing an internal link while ignoring the restricted type parameter" in {
    val target = ResolvedInternalTarget(Path.parse("/doc#foo"), RelativePath.parse("#foo"), TargetFormats.Selected("html"))
    val elem = testPar(SpanLink(target)("link"))
    render (elem) should be ("""<p>some <a href="#foo">link</a> span</p>""")
  }
  
  it should "render a paragraph containing a citation link" in {
    val elem = testPar(CitationLink("ref","label"))
    render (elem) should be ("""<p>some <a class="citation" href="#ref">[label]</a> span</p>""") 
  }
  
  it should "render a paragraph containing a footnote link" in {
    val elem = testPar(FootnoteLink("id","label"))
    render (elem) should be ("""<p>some <a class="footnote" href="#id">[label]</a> span</p>""") 
  }

  it should "render a raw internal link" in {
    val elem = testPar(RawLink.internal("/doc#foo"))
    render (elem) should be ("""<p>some #foo span</p>""")
  }

  it should "render a raw external link" in {
    val elem = testPar(RawLink.external("/foo"))
    render (elem) should be ("""<p>some /foo span</p>""")
  }
  
  it should "render a paragraph containing an image without title" in {
    val elem = testPar(Image(imageTarget, alt = Some("img")))
    render (elem) should be ("""<p>some <img src="foo.jpg" alt="img"> span</p>""") 
  }
  
  it should "render a paragraph containing an image with title" in {
    val elem = testPar(Image(imageTarget, alt = Some("img"), title = Some("title")))
    render (elem) should be ("""<p>some <img src="foo.jpg" alt="img" title="title"> span</p>""") 
  }

  it should "render a paragraph containing an image with width and height in pixels" in {
    val image = Image(imageTarget, alt = Some("img"), width = Some(LengthUnit.px(200)), height = Some(LengthUnit.px(120)))
    val elem = testPar(image)
    render (elem) should be ("""<p>some <img src="foo.jpg" alt="img" width="200" height="120"> span</p>""")
  }

  it should "render a paragraph containing an image with width and height in a unit other than pixels" in {
    val image = Image(imageTarget, alt = Some("img"), width = Some(LengthUnit.in(12.4)), height = Some(LengthUnit.in(6.8)))
    val elem = testPar(image)
    render (elem) should be ("""<p>some <img src="foo.jpg" alt="img" style="width:12.4in;height:6.8in"> span</p>""")
  }

  it should "render a paragraph containing an image with just width in a unit other than pixels" in {
    val image = Image(imageTarget, alt = Some("img"), width = Some(LengthUnit.in(12.4)))
    val elem = testPar(image)
    render (elem) should be ("""<p>some <img src="foo.jpg" alt="img" style="width:12.4in"> span</p>""")
  }

  it should "render a paragraph containing a link with an icon glyph" in {
    val elem = p(
      Text("some "), 
      SpanLink.external("/foo")(IconGlyph('\uefa2', options = Styles("icofont-laika"))), 
      Text(" span")
    )
    render (elem) should be ("""<p>some <a href="/foo"><i class="icofont-laika">&#xefa2;</i></a> span</p>""")
  }

  it should "render a paragraph containing a link with an icon style" in {
    val elem = p(
      Text("some "),
      SpanLink.external("/foo")(IconStyle("open", options = Styles("icofont-laika"))),
      Text(" span")
    )
    render (elem) should be ("""<p>some <a href="/foo"><i class="open icofont-laika"></i></a> span</p>""")
  }

  it should "render a paragraph containing a link with an inline SVG icon" in {
    val svg = """<svg class="svg-icon" width="100%" height="100%" viewBox="0 0 100 100" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" xml:space="preserve" xmlns:serif="http://www.serif.com/" style="fill-rule:evenodd;clip-rule:evenodd;stroke-linejoin:round;stroke-miterlimit:2;">
                |  <g class="svg-shape">
                |    <path d="M75,47.5c13.246,0 24,10.754 24,24c0,13.246 -10.754,24"/>
                |  </g>
                |</svg>""".stripMargin
    val elem = p(
      Text("some "),
      SpanLink.external("/foo")(InlineSVGIcon(svg)),
      Text(" span")
    )
    render (elem) should be (s"""<p>some <a href="/foo"><span>$svg</span></a> span</p>""")
  }
  
  it should "render a paragraph containing an unresolved link reference" in {
    val elem = testPar(LinkIdReference("id", generatedSource("[link] [id]"))("link"))
    render (elem) should be ("""<p>some [link] [id] span</p>""")
  }
  
  it should "render a paragraph containing an unresolved image reference" in {
    val elem = testPar(ImageIdReference("img","id", source("![img] [id]", "![img] [id]")))
    render (elem) should be ("""<p>some ![img] [id] span</p>""") 
  }
  
  it should "render a paragraph containing an internal link target" in {
    val elem = testPar(InternalLinkTarget(Id("target")))
    render (elem) should be ("""<p>some <a id="target"></a> span</p>""") 
  }
  
  it should "render a template root containing string elements" in {
    val elem = TemplateRoot(TemplateString("aa"),TemplateString("bb"),TemplateString("cc"))
    render (elem) should be ("aabbcc")
  }
  
  it should "render a template span sequence containing string elements" in {
    val elem = TemplateSpanSequence(TemplateString("aa"),TemplateString("bb"),TemplateString("cc"))
    render (elem) should be ("aabbcc")
  }
  
  it should "render a template string without creating html entities" in {
    val elem = TemplateRoot(TemplateString("aa & bb"))
    render (elem) should be ("aa & bb")
  }
  
  it should "render a template root containing a TemplateElement" in {
    val elem = TemplateRoot(
      TemplateString("aa"),
      TemplateElement(BlockSequence(List(p("aaa"), p("bbb")),Styles("foo"))),
      TemplateString("cc")
    )
    val html = """aa<div class="foo">
      |  <p>aaa</p>
      |  <p>bbb</p>
      |</div>cc""".stripMargin
    render (elem) should be (html)
  }

  it should "render a runtime message" in {
    val html = """<span class="runtime-message warning">some message</span>"""
    render(RuntimeMessage(MessageLevel.Warning, "some message"), MessageFilter.Warning) should be (html)
  }
  
  it should "render a comment" in {
    render(Comment("foo")) should be ("<!-- foo -->")
  }
  
  it should "render an invalid block without the runtime message in default mode" in {
    val elem = InvalidBlock(RuntimeMessage(MessageLevel.Warning, "some message"), GeneratedSource, p("fallback"))
    val html = "<p>fallback</p>"
    render(elem) should be (html)
  }
  
  it should "render an invalid block without the runtime message if the configured message level is higher" in {
    val elem = InvalidBlock(RuntimeMessage(MessageLevel.Warning, "some message"), GeneratedSource, p("fallback"))
    val html = "<p>fallback</p>"
    render(elem, MessageFilter.Error) should be (html)
  }
  
  it should "render an invalid block with the runtime message if the configured message level is lower or equal" in {
    val elem = InvalidBlock(RuntimeMessage(MessageLevel.Warning, "some message"), GeneratedSource, p("fallback"))
    val html = """<p><span class="runtime-message warning">some message</span></p><p>fallback</p>"""
    render(elem, MessageFilter.Info) should be (html)
  }
  
  it should "render an invalid span without the runtime message in default mode" in {
    val elem = InvalidSpan(RuntimeMessage(MessageLevel.Warning, "some message"), GeneratedSource, Text("fallback"))
    render(elem) should be ("fallback")
  }
  
  it should "render an invalid span without the runtime message if the configured message level is higher" in {
    val elem = InvalidSpan(RuntimeMessage(MessageLevel.Warning, "some message"), GeneratedSource, Text("fallback"))
    render(elem, MessageFilter.Error) should be ("fallback")
  }
  
  it should "render an invalid span with the runtime message if the configured message level is lower or equal" in {
    val elem = InvalidSpan(RuntimeMessage(MessageLevel.Warning, "some message"), GeneratedSource, Text("fallback"))
    val html = """<span class="runtime-message warning">some message</span> fallback"""
    render(elem, MessageFilter.Info) should be (html)
  }
  
  it should "render a literal block" in {
    val code = """line 1
      |
      |    <line 2
      |
      |line 3""".stripMargin
    val elem = LiteralBlock(code)
    render(elem) should be ("<pre><code>" + code.replace("<", "&lt;") + "</code></pre>") 
  }
  
  it should "render a parsed literal block" in {
    val code = """line 1
      |
      |    #<line 2
      |
      |line 3""".stripMargin.split("#")
    val elem = ParsedLiteralBlock(List(Text(code(0)), Emphasized("em"), Text(code(1))))
    val html = "<pre><code>" + code(0) + "<em>em</em>" + code(1).replace("<", "&lt;") + "</code></pre>"
    render(elem) should be (html) 
  }
  
  it should "render a code block" in {
    val code = """line 1
      |
      |    <line 2
      |
      |line 3""".stripMargin
    val elem = CodeBlock("banana-script", List(Text(code)))
    render(elem) should be ("<pre><code class=\"banana-script\">" + code.replace("<", "&lt;") + "</code></pre>") 
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
    val elem = RawContent(NonEmptySet.of("html", "spooky"), raw)
    render(elem) should be (raw) 
  }
  
  it should "ignore raw content if the html format is not specified" in {
    val raw = "<span>foo</span>"
    val elem = RawContent(NonEmptySet.of("dodgy", "spooky"), raw)
    render(elem) should be ("") 
  }
  
  it should "render an embedded root with correct indentation" in {
    val elem = RootElement(TemplateRoot(
      TemplateString("<div>\n  "),
      EmbeddedRoot(List(p("aaa"),p("bbb")), 2),
      TemplateString("\n</div>")
    ))
    val html = """<div>
      |  <p>aaa</p>
      |  <p>bbb</p>
      |</div>""".stripMargin
    render(elem) should be (html) 
  }
  
  it should "render an embedded root without indentation" in {
    val elem = RootElement(TemplateRoot(
      TemplateString("<div>\n"),
      EmbeddedRoot(p("aaa"),p("bbb")),
      TemplateString("\n</div>")
    ))
    val html = """<div>
      |<p>aaa</p>
      |<p>bbb</p>
      |</div>""".stripMargin
    render(elem) should be (html) 
  }
  
  
}
