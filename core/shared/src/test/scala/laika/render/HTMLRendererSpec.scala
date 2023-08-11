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
import laika.ast.sample.{ ParagraphCompanionShortcuts, TestSourceBuilders }
import laika.format.HTML
import laika.parse.GeneratedSource
import laika.parse.code.CodeCategory
import laika.rewrite.{ OutputContext, Version, Versions }
import laika.rewrite.nav.{
  ConfigurablePathTranslator,
  PathAttributes,
  PathTranslator,
  TargetFormats,
  TranslatorConfig
}
import munit.FunSuite

class HTMLRendererSpec extends FunSuite with ParagraphCompanionShortcuts with TestSourceBuilders {

  private val versions = Versions.forCurrentVersion(Version("0.42", "0.42"))

  private val defaultRenderer = Renderer.of(HTML).build

  private def pathTranslator(
      specs: Map[Path, PathAttributes],
      versioned: Boolean
  ): PathTranslator = ConfigurablePathTranslator(
    TranslatorConfig(if (versioned) Some(versions) else None, "title", "index", None),
    OutputContext(HTML),
    Root / "doc",
    path => specs.get(path.withoutFragment)
  )

  def run(elem: Element, expected: String): Unit =
    assertEquals(defaultRenderer.render(elem), Right(expected))

  def runWithFilter(elem: Element, filter: MessageFilter, expected: String): Unit = {
    val renderer = Renderer.of(HTML).renderMessages(filter).build
    assertEquals(renderer.render(elem), Right(expected))
  }

  def runWithStaticDocs(
      elem: Element,
      specs: Map[Path, PathAttributes],
      expected: String,
      versioned: Boolean = false
  ): Unit = {
    val res = defaultRenderer.render(
      elem,
      Root / "doc",
      pathTranslator(specs, versioned),
      StyleDeclarationSet.empty
    )
    assertEquals(res, Right(expected))
  }

  def runUnformatted(elem: Element, expected: String): Unit = {
    val res = Renderer.of(HTML).unformatted.build.render(elem)
    assertEquals(res, Right(expected))
  }

  private val staticUnversionedSpec = PathAttributes(isStatic = true, isVersioned = false)
  private val imagePath             = Root / "foo.jpg"
  private val imageTarget           = InternalTarget(imagePath)
  private val imageTestSpecs        = Map(imagePath -> staticUnversionedSpec)

  def testPar(span: Span): Paragraph = p(Text("some "), span, Text(" span"))

  test("paragraph with plain text") {
    val elem = p("some text")
    run(elem, "<p>some text</p>")
  }

  test("render a document with two paragraphs with plain text") {
    val elem = RootElement(p("aaa"), p("bbb"))
    val html = """<p>aaa</p>
                 |<p>bbb</p>""".stripMargin
    run(elem, html)
  }

  test("render a blockquote with two paragraphs with plain text") {
    val elem = QuotedBlock(p("aaa"), p("bbb"))
    val html = """<blockquote>
                 |  <p>aaa</p>
                 |  <p>bbb</p>
                 |</blockquote>""".stripMargin
    run(elem, html)
  }

  test("render a block sequence with a style") {
    val elem = RootElement(BlockSequence(List(p("aaa"), p("bbb")), Styles("foo")))
    val html = """<div class="foo">
                 |  <p>aaa</p>
                 |  <p>bbb</p>
                 |</div>""".stripMargin
    run(elem, html)
  }

  test("render a block sequence without a style") {
    val elem = RootElement(p("aaa"), BlockSequence(p("bbb"), p("ccc")))
    val html = """<p>aaa</p>
                 |<p>bbb</p>
                 |<p>ccc</p>""".stripMargin
    run(elem, html)
  }

  test("render a block sequence with a single element") {
    val elem = RootElement(p("aaa"), BlockSequence("bbb"), p("ccc"))
    val html = """<p>aaa</p>
                 |<p>bbb</p>
                 |<p>ccc</p>""".stripMargin
    run(elem, html)
  }

  test("render a blockquote with simple flow content") {
    val elem = QuotedBlock("aaa")
    val html = "<blockquote>aaa</blockquote>"
    run(elem, html)
  }

  test("render a blockquote with an attribution") {
    val elem = QuotedBlock(List(p("aaa")), List(Text("bbb")))
    val html = """<blockquote>
                 |  <p>aaa</p>
                 |  <p class="attribution">bbb</p>
                 |</blockquote>""".stripMargin
    run(elem, html)
  }

  test("render a bullet list with simple flow content") {
    val elem = RootElement(BulletList("aaa", "bbb"))
    val html = """<ul>
                 |  <li>aaa</li>
                 |  <li>bbb</li>
                 |</ul>""".stripMargin
    run(elem, html)
  }

  test("render an enumerated list with simple flow content") {
    val elem = EnumList("aaa", "bbb")
    val html = """<ol class="arabic">
                 |  <li>aaa</li>
                 |  <li>bbb</li>
                 |</ol>""".stripMargin
    run(elem, html)
  }

  test("render an enumerated list with lower roman enumeration style") {
    val elem = EnumList(EnumFormat(EnumType.LowerRoman))("aaa", "bbb")
    val html = """<ol class="lowerroman">
                 |  <li>aaa</li>
                 |  <li>bbb</li>
                 |</ol>""".stripMargin
    run(elem, html)
  }

  test("render an enumerated list with upper roman enumeration style") {
    val elem = EnumList(EnumFormat(EnumType.UpperRoman))("aaa", "bbb")
    val html = """<ol class="upperroman">
                 |  <li>aaa</li>
                 |  <li>bbb</li>
                 |</ol>""".stripMargin
    run(elem, html)
  }

  test("render an enumerated list with lower alpha enumeration style") {
    val elem = EnumList(EnumFormat(EnumType.LowerAlpha))("aaa", "bbb")
    val html = """<ol class="loweralpha">
                 |  <li>aaa</li>
                 |  <li>bbb</li>
                 |</ol>""".stripMargin
    run(elem, html)
  }

  test("render an enumerated list with upper alpha enumeration style") {
    val elem = EnumList(EnumFormat(EnumType.UpperAlpha))("aaa", "bbb")
    val html = """<ol class="upperalpha">
                 |  <li>aaa</li>
                 |  <li>bbb</li>
                 |</ol>""".stripMargin
    run(elem, html)
  }

  test("render an enumerated list with the start value if it is not 1") {
    val elem = EnumList(EnumFormat(EnumType.Arabic), 7)("aaa", "bbb")
    val html = """<ol class="arabic" start="7">
                 |  <li>aaa</li>
                 |  <li>bbb</li>
                 |</ol>""".stripMargin
    run(elem, html)
  }

  private def fp(content: String) = ForcedParagraph(List(Text(content)))

  test("render a bullet list with forced paragraphs as list items") {
    val elem = BulletList(fp("aaa"), fp("bbb"))
    val html = """<ul>
                 |  <li>
                 |    <p>aaa</p>
                 |  </li>
                 |  <li>
                 |    <p>bbb</p>
                 |  </li>
                 |</ul>""".stripMargin
    run(elem, html)
  }

  test("render an enumerated list with forced paragraphs as list items") {
    val elem = EnumList(fp("aaa"), fp("bbb"))
    val html = """<ol class="arabic">
                 |  <li>
                 |    <p>aaa</p>
                 |  </li>
                 |  <li>
                 |    <p>bbb</p>
                 |  </li>
                 |</ol>""".stripMargin
    run(elem, html)
  }

  test("render a definition list with paragraphs") {
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
    run(elem, html)
  }

  test("render a definition list with simple flow content") {
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
    run(elem, html)
  }

  test("render a navigation list") {
    val refPath = Root / "doc-99"
    val elem    = NavigationList(
      Seq(
        NavigationItem(
          SpanSequence("Link-1"),
          Seq(
            NavigationItem(
              SpanSequence("Link-2"),
              Nil,
              Some(NavigationLink(InternalTarget(Root / "doc-2.html").relativeTo(refPath))),
              options = Style.level(2)
            )
          ),
          Some(NavigationLink(InternalTarget(Root / "doc-1.html").relativeTo(refPath))),
          options = Style.level(1)
        ),
        NavigationItem(
          SpanSequence("Header-3"),
          Seq(
            NavigationItem(
              SpanSequence("Link-4"),
              Nil,
              Some(
                NavigationLink(
                  InternalTarget(Root / "doc-4.html").relativeTo(refPath),
                  selfLink = true
                )
              ),
              options = Style.level(2)
            )
          ),
          options = Style.level(1)
        ),
        NavigationItem(
          SpanSequence("Link-5"),
          Nil,
          Some(NavigationLink(InternalTarget(Root / "doc-5.html").relativeTo(refPath))),
          options = Style.level(1)
        )
      )
    )
    val html    =
      """<ul class="nav-list">
        |  <li class="level1 nav-node"><a href="doc-1.html">Link-1</a></li>
        |  <li class="level2 nav-leaf"><a href="doc-2.html">Link-2</a></li>
        |  <li class="level1 nav-header">Header-3</li>
        |  <li class="level2 active nav-leaf"><a href="doc-4.html">Link-4</a></li>
        |  <li class="level1 nav-leaf"><a href="doc-5.html">Link-5</a></li>
        |</ul>""".stripMargin
    run(elem, html)
  }

  test("render a footnote") {
    val elem = Footnote("label", List(p("a"), p("b")), Id("id"))
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
    run(elem, html)
  }

  test("render a citation") {
    val elem = Citation("ref", List(p("a"), p("b")), Id("ref"))
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
    run(elem, html)
  }

  test("render a table without header cells") {
    val elem = Table(Row(BodyCell("a"), BodyCell("b")), Row(BodyCell("c"), BodyCell("d")))
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
    run(elem, html)
  }

  test("render a table with header cells") {
    val elem = Table(
      TableHead(List(Row(BodyCell("a"), BodyCell("b")))),
      TableBody(List(Row(BodyCell("c"), BodyCell("d"))))
    )
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
    run(elem, html)
  }

  test("render a table with a caption") {
    val caption = Caption(List(Text("caption")))
    val elem    =
      Table(Row(BodyCell("a"), BodyCell("b")), Row(BodyCell("c"), BodyCell("d"))).copy(caption =
        caption
      )
    val html    = """<table>
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
    run(elem, html)
  }

  test("render a cell using colspan and rowspan attributes") {
    val elem = Cell(BodyCell, List(p("a")), 3, 2)
    val html = """<td colspan="3" rowspan="2">a</td>"""
    run(elem, html)
  }

  test("render a cell with two paragraphs") {
    val elem = BodyCell(p("a"), p("b"))
    val html = """<td>
                 |  <p>a</p>
                 |  <p>b</p>
                 |</td>""".stripMargin
    run(elem, html)
  }

  test("render a titled block") {
    val elem = TitledBlock(
      List(Text("some "), Emphasized("em"), Text(" text")),
      List(p("aaa"), Rule(), p("bbb"))
    )
    val html = """<div>
                 |  <p class="title">some <em>em</em> text</p>
                 |  <p>aaa</p>
                 |  <hr>
                 |  <p>bbb</p>
                 |</div>""".stripMargin
    run(elem, html)
  }

  test("render a figure") {
    val elem = Figure(
      Image(InternalTarget(Root / "image.jpg"), alt = Some("alt")),
      List(Text("some "), Emphasized("caption"), Text(" text")),
      List(p("aaa"), Rule(), p("bbb"))
    )
    val html = """<div class="figure">
                 |  <img src="image.jpg" alt="alt">
                 |  <p class="caption">some <em>caption</em> text</p>
                 |  <div class="legend">
                 |    <p>aaa</p>
                 |    <hr>
                 |    <p>bbb</p>
                 |  </div>
                 |</div>""".stripMargin
    runWithStaticDocs(elem, imageTestSpecs, html)
  }

  test("render a document with two paragraphs separated by a horizontal rule") {
    val elem = RootElement(p("aaa"), Rule(), p("bbb"))
    val html = """<p>aaa</p>
                 |<hr>
                 |<p>bbb</p>""".stripMargin
    run(elem, html)
  }

  test("render a document with two nested sections") {
    val nested = Section(Header(2, Text("Title 2")), List(p("Line 1"), p("Line 2")))
    val elem   =
      RootElement(Section(Header(1, Text("Title 1")), List(p("Line 1"), p("Line 2"))), nested)
    val html   = """
                 |<h1>Title 1</h1>
                 |<p>Line 1</p>
                 |<p>Line 2</p>
                 |
                 |<h2>Title 2</h2>
                 |<p>Line 1</p>
                 |<p>Line 2</p>""".stripMargin
    run(elem, html)
  }

  test("render a selection") {
    val group = Selection(
      "config",
      Seq(
        Choice("name-a", "label-a", List(p("common"), p("11\n22"))),
        Choice("name-b", "label-b", List(p("common"), p("33\n44")))
      )
    )
    val elem  = RootElement(p("aaa"), group, p("bbb"))
    val html  = """<p>aaa</p>
                 |<p><strong>label-a</strong></p>
                 |<p>common</p>
                 |<p>11
                 |22</p>
                 |<p><strong>label-b</strong></p>
                 |<p>common</p>
                 |<p>33
                 |44</p>
                 |<p>bbb</p>""".stripMargin
    run(elem, html)
  }

  test("render a choice group".ignore) {
    val group = Selection(
      "config",
      Seq(
        Choice("name-a", "label-a", List(p("common"), p("11\n22"))),
        Choice("name-b", "label-b", List(p("common"), p("33\n44")))
      )
    )
    val elem  = RootElement(p("aaa"), group, p("bbb"))
    val html  = """<p>aaa</p>
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
    run(elem, html)
  }

  test("render a title containing emphasized text") {
    val elem = Title(Text("some "), Emphasized("em"), Text(" text"))
    run(elem, "<h1>some <em>em</em> text</h1>")
  }

  test("render a title containing a section number") {
    val elem = Title(SectionNumber(Seq(1, 2, 3)), Text(" Title"))
    run(elem, """<h1><span class="section-number">1.2.3 </span> Title</h1>""")
  }

  test("render a paragraph containing emphasized text") {
    val elem = testPar(Emphasized("em"))
    run(elem, "<p>some <em>em</em> span</p>")
  }

  test("render a paragraph containing strong text") {
    val elem = testPar(Strong("strong"))
    run(elem, "<p>some <strong>strong</strong> span</p>")
  }

  test("render a paragraph containing a deleted span") {
    val elem = testPar(Deleted("deleted"))
    run(elem, "<p>some <del>deleted</del> span</p>")
  }

  test("render a paragraph containing an inserted span") {
    val elem = testPar(Inserted("inserted"))
    run(elem, "<p>some <ins>inserted</ins> span</p>")
  }

  test("render a paragraph containing a literal span") {
    val elem = testPar(Literal("code"))
    run(elem, "<p>some <code>code</code> span</p>")
  }

  test("render a paragraph containing a code span") {
    val elem = testPar(InlineCode("banana-script", List(Text("code"))))
    run(elem, "<p>some <code class=\"banana-script\">code</code> span</p>")
  }

  test("render a paragraph containing a link without title") {
    val elem = testPar(SpanLink.external("/foo")("link"))
    run(elem, """<p>some <a href="/foo">link</a> span</p>""")
  }

  test("render a paragraph containing a link with title") {
    val elem = testPar(SpanLink.external("/foo")("link").copy(title = Some("title")))
    run(elem, """<p>some <a href="/foo" title="title">link</a> span</p>""")
  }

  test("render a paragraph containing a link with emphasized text") {
    val elem = testPar(SpanLink.external("/foo")(Text("link"), Emphasized("text")))
    run(elem, """<p>some <a href="/foo">link<em>text</em></a> span</p>""")
  }

  test("render a paragraph containing an internal link with emphasized text") {
    val elem = testPar(SpanLink.internal("#foo")(Text("link"), Emphasized("text")))
    run(elem, """<p>some <a href="#foo">link<em>text</em></a> span</p>""")
  }

  test("render a paragraph containing a internal link with a fragment part") {
    val elem = testPar(SpanLink.internal("/bar.html#foo")(Text("link"), Emphasized("text")))
    run(elem, """<p>some <a href="bar.html#foo">link<em>text</em></a> span</p>""")
  }

  test("render a paragraph containing a internal link without a fragment part") {
    val elem = testPar(SpanLink.internal("/bar.html")(Text("link"), Emphasized("text")))
    run(elem, """<p>some <a href="bar.html">link<em>text</em></a> span</p>""")
  }

  test(
    "render a paragraph containing an internal link while ignoring the restricted type parameter"
  ) {
    val target = ResolvedInternalTarget(
      Path.parse("/doc.html#foo"),
      RelativePath.parse("#foo"),
      TargetFormats.Selected("html")
    )
    val elem   = testPar(SpanLink(target)("link"))
    run(elem, """<p>some <a href="#foo">link</a> span</p>""")
  }

  test("render a paragraph containing a citation link") {
    val elem = testPar(CitationLink("ref", "label"))
    run(elem, """<p>some <a class="citation" href="#ref">[label]</a> span</p>""")
  }

  test("render a paragraph containing a footnote link") {
    val elem = testPar(FootnoteLink("id", "label"))
    run(elem, """<p>some <a class="footnote" href="#id">[label]</a> span</p>""")
  }

  test("render a raw internal link") {
    val elem = testPar(RawLink.internal("/doc.html#foo"))
    run(elem, """<p>some #foo span</p>""")
  }

  test("render a raw external link") {
    val elem = testPar(RawLink.external("/foo"))
    run(elem, """<p>some /foo span</p>""")
  }

  test("render a paragraph containing an image without title") {
    val elem     = testPar(Image(imageTarget, alt = Some("img")))
    val expected = """<p>some <img src="foo.jpg" alt="img"> span</p>"""
    runWithStaticDocs(elem, imageTestSpecs, expected)
  }

  test("render a paragraph containing a versioned image") {
    val elem     = testPar(Image(imageTarget, alt = Some("img")))
    val spec     = PathAttributes(isStatic = true, isVersioned = true)
    val expected = """<p>some <img src="0.42/foo.jpg" alt="img"> span</p>"""
    runWithStaticDocs(elem, Map(imagePath -> spec), expected, versioned = true)
  }

  test("render a paragraph containing an image with title") {
    val elem     = testPar(Image(imageTarget, alt = Some("img"), title = Some("title")))
    val expected = """<p>some <img src="foo.jpg" alt="img" title="title"> span</p>"""
    runWithStaticDocs(elem, imageTestSpecs, expected)
  }

  test("render a paragraph containing an image with width and height in pixels") {
    val image    = Image(
      imageTarget,
      alt = Some("img"),
      width = Some(LengthUnit.px(200)),
      height = Some(LengthUnit.px(120))
    )
    val elem     = testPar(image)
    val expected = """<p>some <img src="foo.jpg" alt="img" width="200" height="120"> span</p>"""
    runWithStaticDocs(elem, imageTestSpecs, expected)
  }

  test("render a paragraph containing an image with width and height in a unit other than pixels") {
    val image    = Image(
      imageTarget,
      alt = Some("img"),
      width = Some(LengthUnit.in(12.4)),
      height = Some(LengthUnit.in(6.8))
    )
    val elem     = testPar(image)
    val expected =
      """<p>some <img src="foo.jpg" alt="img" style="width:12.4in;height:6.8in"> span</p>"""
    runWithStaticDocs(elem, imageTestSpecs, expected)
  }

  test("render a paragraph containing an image with just width in a unit other than pixels") {
    val image    = Image(imageTarget, alt = Some("img"), width = Some(LengthUnit.in(12.4)))
    val elem     = testPar(image)
    val expected = """<p>some <img src="foo.jpg" alt="img" style="width:12.4in"> span</p>"""
    runWithStaticDocs(elem, imageTestSpecs, expected)
  }

  test("render a paragraph containing a link with an icon glyph") {
    val elem = p(
      Text("some "),
      SpanLink.external("/foo")(IconGlyph('\uefa2', options = Styles("icofont-laika"))),
      Text(" span")
    )
    run(elem, """<p>some <a href="/foo"><i class="icofont-laika">&#xefa2;</i></a> span</p>""")
  }

  test("render a paragraph containing a link with an icon style") {
    val elem = p(
      Text("some "),
      SpanLink.external("/foo")(IconStyle("open", options = Styles("icofont-laika"))),
      Text(" span")
    )
    run(elem, """<p>some <a href="/foo"><i class="open icofont-laika"></i></a> span</p>""")
  }

  test("render a paragraph containing a link with an inline SVG icon") {
    val svg  =
      """<svg class="svg-icon" width="100%" height="100%" viewBox="0 0 100 100" version="1.1" xmlns="http://www.w3.org/2000/svg" xml:space="preserve" style="fill-rule:evenodd;clip-rule:evenodd;stroke-linejoin:round;stroke-miterlimit:2;">
        |  <g class="svg-shape">
        |    <path d="M75,47.5c13.246,0 24,10.754 24,24c0,13.246 -10.754,24"/>
        |  </g>
        |</svg>""".stripMargin
    val elem = p(
      Text("some "),
      SpanLink.external("/foo")(InlineSVGIcon(svg)),
      Text(" span")
    )
    run(elem, s"""<p>some <a href="/foo"><span>$svg</span></a> span</p>""")
  }

  test("render a paragraph containing a link with a SVG symbol icon") {
    val svgDoc = Root / "icons" / "all.svg"
    val svg  = """<svg class="svg-icon"><use class="svg-shape" href="icons/all.svg#open"/></svg>"""
    val elem = p(
      Text("some "),
      SpanLink.external("/foo")(SVGSymbolIcon.internal(svgDoc.withFragment("open"))),
      Text(" span")
    )
    val expected = s"""<p>some <a href="/foo"><span>$svg</span></a> span</p>"""
    runWithStaticDocs(elem, Map(svgDoc -> staticUnversionedSpec), expected)
  }

  test("render a paragraph containing an unresolved link reference") {
    val elem = testPar(LinkIdReference("id", generatedSource("[link] [id]"))("link"))
    run(elem, """<p>some <code>[link] [id]</code> span</p>""")
  }

  test("render a paragraph containing an unresolved image reference") {
    val elem = testPar(ImageIdReference("img", "id", source("![img] [id]", "![img] [id]")))
    run(elem, """<p>some <code>![img] [id]</code> span</p>""")
  }

  test("render a paragraph containing an internal link target") {
    val elem = testPar(InternalLinkTarget(Id("target")))
    run(elem, """<p>some <a id="target"></a> span</p>""")
  }

  test("render a template root containing string elements") {
    val elem = TemplateRoot(TemplateString("aa"), TemplateString("bb"), TemplateString("cc"))
    run(elem, "aabbcc")
  }

  test("render a template span sequence containing string elements") {
    val elem =
      TemplateSpanSequence(TemplateString("aa"), TemplateString("bb"), TemplateString("cc"))
    run(elem, "aabbcc")
  }

  test("render a template string without creating html entities") {
    val elem = TemplateRoot(TemplateString("aa & bb"))
    run(elem, "aa & bb")
  }

  test("render a template root containing a TemplateElement") {
    val elem = TemplateRoot(
      TemplateString("aa"),
      TemplateElement(BlockSequence(List(p("aaa"), p("bbb")), Styles("foo"))),
      TemplateString("cc")
    )
    val html = """aa<div class="foo">
                 |  <p>aaa</p>
                 |  <p>bbb</p>
                 |</div>cc""".stripMargin
    run(elem, html)
  }

  test("render a runtime message") {
    val html = """<span class="runtime-message warning">some message</span>"""
    runWithFilter(RuntimeMessage(MessageLevel.Warning, "some message"), MessageFilter.Warning, html)
  }

  test("render a comment") {
    run(Comment("foo"), "<!-- foo -->")
  }

  test("render an invalid block without the runtime message in default mode") {
    val elem = InvalidBlock(
      RuntimeMessage(MessageLevel.Warning, "some message"),
      GeneratedSource,
      p("fallback")
    )
    val html = "<p>fallback</p>"
    run(elem, html)
  }

  test(
    "render an invalid block without the runtime message if the configured message level is higher"
  ) {
    val elem = InvalidBlock(
      RuntimeMessage(MessageLevel.Warning, "some message"),
      GeneratedSource,
      p("fallback")
    )
    val html = "<p>fallback</p>"
    runWithFilter(elem, MessageFilter.Error, html)
  }

  test(
    "render an invalid block with the runtime message if the configured message level is lower or equal"
  ) {
    val elem = InvalidBlock(
      RuntimeMessage(MessageLevel.Warning, "some message"),
      GeneratedSource,
      p("fallback")
    )
    val html = """<p><span class="runtime-message warning">some message</span></p><p>fallback</p>"""
    runWithFilter(elem, MessageFilter.Info, html)
  }

  test("render an invalid span without the runtime message in default mode") {
    val elem = InvalidSpan(
      RuntimeMessage(MessageLevel.Warning, "some message"),
      GeneratedSource,
      Text("fallback")
    )
    run(elem, "fallback")
  }

  test(
    "render an invalid span without the runtime message if the configured message level is higher"
  ) {
    val elem = InvalidSpan(
      RuntimeMessage(MessageLevel.Warning, "some message"),
      GeneratedSource,
      Text("fallback")
    )
    runWithFilter(elem, MessageFilter.Error, "fallback")
  }

  test(
    "render an invalid span with the runtime message if the configured message level is lower or equal"
  ) {
    val elem = InvalidSpan(
      RuntimeMessage(MessageLevel.Warning, "some message"),
      GeneratedSource,
      Text("fallback")
    )
    val html = """<span class="runtime-message warning">some message</span> fallback"""
    runWithFilter(elem, MessageFilter.Info, html)
  }

  test("render a literal block") {
    val code = """line 1
                 |
                 |    <line 2
                 |
                 |line 3""".stripMargin
    val elem = LiteralBlock(code)
    run(elem, "<pre><code>" + code.replace("<", "&lt;") + "</code></pre>")
  }

  test("render a parsed literal block") {
    val code = """line 1
                 |
                 |    #<line 2
                 |
                 |line 3""".stripMargin.split("#")
    val elem = ParsedLiteralBlock(List(Text(code(0)), Emphasized("em"), Text(code(1))))
    val html =
      "<pre><code>" + code(0) + "<em>em</em>" + code(1).replace("<", "&lt;") + "</code></pre>"
    run(elem, html)
  }

  test("render a code block") {
    val code = """line 1
                 |
                 |    <line 2
                 |
                 |line 3""".stripMargin
    val elem = CodeBlock("banana-script", List(Text(code)))
    run(elem, "<pre><code class=\"banana-script\">" + code.replace("<", "&lt;") + "</code></pre>")
  }

  test("render a code block with syntax highlighting") {
    val code         = List(
      CodeSpan("{{", CodeCategory.Keyword),
      CodeSpan("foo"),
      CodeSpan("}}", Set(CodeCategory.Tag.Punctuation, CodeCategory.Identifier))
    )
    val elem         = CodeBlock("banana-script", code)
    val renderedCode =
      """<span class="keyword">{{</span><span>foo</span><span class="tag-punctuation identifier">}}</span>"""
    run(elem, s"""<pre><code class="nohighlight">$renderedCode</code></pre>""")
  }

  test("render a literal block inside a blockquote without indentation") {
    val code = """line 1
                 |
                 |    line 2
                 |
                 |line 3""".stripMargin
    val html = """<blockquote>
                 |  <pre><code>%s</code></pre>
                 |</blockquote>""".stripMargin.format(code)
    val elem = QuotedBlock(LiteralBlock(code))
    run(elem, html)
  }

  test("render a parsed literal block inside a blockquote without indentation") {
    val code = """line 1
                 |
                 |    line 2
                 |
                 |line 3""".stripMargin
    val html = """<blockquote>
                 |  <pre><code>:<em>%s</em>:</code></pre>
                 |</blockquote>""".stripMargin.format(code)
    val elem = QuotedBlock(ParsedLiteralBlock(List(Text(":"), Emphasized(code), Text(":"))))
    run(elem, html)
  }

  test("render a code block inside a blockquote without indentation") {
    val code = """line 1
                 |
                 |    line 2
                 |
                 |line 3""".stripMargin
    val html = """<blockquote>
                 |  <pre><code class="nohighlight">:<em>%s</em>:</code></pre>
                 |</blockquote>""".stripMargin.format(code)
    val elem = QuotedBlock(CodeBlock("banana-script", List(Text(":"), Emphasized(code), Text(":"))))
    run(elem, html)
  }

  test("render a table cell unformatted") {
    val elem = BodyCell(p("a"), p("b"))
    val html = """<td>
                 |<p>a</p>
                 |<p>b</p>
                 |</td>""".stripMargin
    runUnformatted(elem, html)
  }

  test("render raw content unchanged if the html format is specified") {
    val raw  = "<span>foo</span>"
    val elem = RawContent(NonEmptySet.of("html", "spooky"), raw)
    run(elem, raw)
  }

  test("ignore raw content if the html format is not specified") {
    val raw  = "<span>foo</span>"
    val elem = RawContent(NonEmptySet.of("dodgy", "spooky"), raw)
    run(elem, "")
  }

  test("render an embedded root with correct indentation") {
    val elem = RootElement(
      TemplateRoot(
        TemplateString("<div>\n  "),
        EmbeddedRoot(List(p("aaa"), p("bbb")), 2),
        TemplateString("\n</div>")
      )
    )
    val html = """<div>
                 |  <p>aaa</p>
                 |  <p>bbb</p>
                 |</div>""".stripMargin
    run(elem, html)
  }

  test("render an embedded root without indentation") {
    val elem = RootElement(
      TemplateRoot(
        TemplateString("<div>\n"),
        EmbeddedRoot(p("aaa"), p("bbb")),
        TemplateString("\n</div>")
      )
    )
    val html = """<div>
                 |<p>aaa</p>
                 |<p>bbb</p>
                 |</div>""".stripMargin
    run(elem, html)
  }

}
