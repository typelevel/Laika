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

package laika.render.fo

import cats.data.NonEmptySet
import laika.api.Renderer
import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentTree
import laika.ast._
import laika.ast.sample.{ ParagraphCompanionShortcuts, TestSourceBuilders }
import laika.config.{ ConfigBuilder, LaikaKeys }
import laika.format.XSLFO
import laika.parse.GeneratedSource
import laika.parse.code.CodeCategory
import laika.parse.markup.DocumentParser.RendererError
import laika.rewrite.OutputContext
import laika.rewrite.nav.{
  ConfigurablePathTranslator,
  NoOpPathTranslator,
  PathAttributes,
  TargetFormats,
  TranslatorConfig
}
import munit.FunSuite

class XSLFORendererSpec extends FunSuite with ParagraphCompanionShortcuts with TestSourceBuilders {

  private val defaultRenderer = Renderer.of(XSLFO).build

  private val defaultParagraphStyles =
    """font-family="serif" font-size="10pt" line-height="1.5" space-after="3mm" text-align="justify""""

  private val ruleBlock =
    """<fo:block space-after="3mm"><fo:leader leader-length="100%" leader-pattern="rule" rule-style="solid" rule-thickness="2pt"></fo:leader></fo:block>"""

  def render(elem: Element, style: StyleDeclarationSet): Either[RendererError, String] =
    defaultRenderer.render(elem, Root / "doc", NoOpPathTranslator, style)

  def run(input: Element, expectedFO: String)(implicit loc: munit.Location): Unit =
    assertEquals(render(input, TestTheme.foStyles), Right(expectedFO))

  def run(input: Element, style: StyleDeclaration, expectedFO: String)(implicit
      loc: munit.Location
  ): Unit =
    run(input, TestTheme.foStyles ++ StyleDeclarationSet(Path.Root, style), expectedFO)

  def run(input: Element, style: StyleDeclarationSet, expectedFO: String)(implicit
      loc: munit.Location
  ): Unit =
    assertEquals(
      defaultRenderer.render(input, Root / "doc", NoOpPathTranslator, style),
      Right(expectedFO)
    )

  def run(elem: Element, messageFilter: MessageFilter, expectedFO: String)(implicit
      loc: munit.Location
  ): Unit = {
    val res = Renderer.of(XSLFO).renderMessages(messageFilter).build.render(
      elem,
      Root / "doc",
      NoOpPathTranslator,
      TestTheme.foStyles
    )
    assertEquals(res, Right(expectedFO))
  }

  def runUnformatted(input: Element, expectedFO: String): Unit = {
    val res = Renderer.of(XSLFO).unformatted.build.render(
      input,
      Root / "doc",
      NoOpPathTranslator,
      TestTheme.foStyles
    )
    assertEquals(res, Right(expectedFO))
  }

  private val imageTarget = InternalTarget(CurrentTree / "foo.jpg")

  test("render a paragraph with plain text") {
    val elem = p("some text")
    val fo   = s"""<fo:block $defaultParagraphStyles>some text</fo:block>"""
    run(elem, fo)
  }

  test("render a document with two paragraphs with plain text") {
    val elem = RootElement(p("aaa"), p("bbb"))
    val fo   = s"""<fo:block $defaultParagraphStyles>aaa</fo:block>
                |<fo:block $defaultParagraphStyles>bbb</fo:block>""".stripMargin
    run(elem, fo)
  }

  test("render a block sequence with a custom style") {
    val elem  = RootElement(BlockSequence(List(p("aaa"), p("bbb")), Styles("foo")))
    val fo    = s"""<fo:block font-weight="bold">
                |  <fo:block $defaultParagraphStyles>aaa</fo:block>
                |  <fo:block $defaultParagraphStyles>bbb</fo:block>
                |</fo:block>""".stripMargin
    val style = StyleDeclaration(
      StyleSelector(Set(StylePredicate.StyleName("foo"))),
      Map("font-weight" -> "bold")
    )
    run(elem, style, fo)
  }

  test("render a block sequence without a style") {
    val elem = RootElement(p("aaa"), BlockSequence(p("bbb"), p("ccc")))
    val fo   = s"""<fo:block $defaultParagraphStyles>aaa</fo:block>
                |<fo:block $defaultParagraphStyles>bbb</fo:block>
                |<fo:block $defaultParagraphStyles>ccc</fo:block>""".stripMargin
    run(elem, fo)
  }

  test("render a block sequence with a single element") {
    val elem = RootElement(p("aaa"), BlockSequence(p("bbb")), p("ccc"))
    val fo   = s"""<fo:block $defaultParagraphStyles>aaa</fo:block>
                |<fo:block $defaultParagraphStyles>bbb</fo:block>
                |<fo:block $defaultParagraphStyles>ccc</fo:block>""".stripMargin
    run(elem, fo)
  }

  test("render a blockquote with two paragraphs with plain text") {
    val elem = QuotedBlock(p("aaa"), p("bbb"))
    val fo   =
      s"""<fo:block font-style="italic" margin-left="8mm" margin-right="8mm" space-after="3mm">
         |  <fo:block $defaultParagraphStyles>aaa</fo:block>
         |  <fo:block $defaultParagraphStyles>bbb</fo:block>
         |</fo:block>""".stripMargin
    run(elem, fo)
  }

  test("render a blockquote with one paragraph with plain text") {
    val elem = QuotedBlock(p("aaa"))
    val fo   =
      s"""<fo:block font-style="italic" margin-left="8mm" margin-right="8mm" space-after="3mm">
         |  <fo:block $defaultParagraphStyles>aaa</fo:block>
         |</fo:block>""".stripMargin
    run(elem, fo)
  }

  test("render a blockquote with an attribution") {
    val elem = QuotedBlock(List(p("aaa")), List(Text("bbb")))
    val fo   =
      s"""<fo:block font-style="italic" margin-left="8mm" margin-right="8mm" space-after="3mm">
         |  <fo:block $defaultParagraphStyles>aaa</fo:block>
         |  <fo:block font-family="serif" font-size="10pt" line-height="1.5" text-align="right">bbb</fo:block>
         |</fo:block>""".stripMargin
    run(elem, fo)
  }

  test("render a bullet list with simple flow content") {
    val elem = BulletList("aaa", "bbb")
    val fo   = s"""<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>&#x2022;</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>aaa</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>&#x2022;</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>bbb</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |</fo:list-block>""".stripMargin
    run(elem, fo)
  }

  test("render a bullet list with a nested list") {
    val elem = BulletList(Seq(SpanSequence(Text("aaa")), BulletList("bbb")))
    val fo   = s"""<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>&#x2022;</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>aaa</fo:block>
                |      <fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
                |        <fo:list-item space-after="3mm">
                |          <fo:list-item-label end-indent="label-end()">
                |            <fo:block $defaultParagraphStyles>&#x2022;</fo:block>
                |          </fo:list-item-label>
                |          <fo:list-item-body start-indent="body-start()">
                |            <fo:block $defaultParagraphStyles>bbb</fo:block>
                |          </fo:list-item-body>
                |        </fo:list-item>
                |      </fo:list-block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |</fo:list-block>""".stripMargin
    run(elem, fo)
  }

  test("render an enumerated list with simple flow content") {
    val elem = EnumList("aaa", "bbb")
    val fo   = s"""<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>1.</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>aaa</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>2.</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>bbb</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |</fo:list-block>""".stripMargin
    run(elem, fo)
  }

  test("render an enumerated list with lower roman enumeration style") {
    val elem = EnumList(EnumFormat(EnumType.LowerRoman))("aaa", "bbb")
    val fo   = s"""<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>i.</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>aaa</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>ii.</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>bbb</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |</fo:list-block>""".stripMargin
    run(elem, fo)
  }

  test("render an enumerated list with upper roman enumeration style") {
    val elem = EnumList(EnumFormat(EnumType.UpperRoman))("aaa", "bbb")
    val fo   = s"""<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>I.</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>aaa</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>II.</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>bbb</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |</fo:list-block>""".stripMargin
    run(elem, fo)
  }

  test("render an enumerated list with lower alpha enumeration style") {
    val elem = EnumList(EnumFormat(EnumType.LowerAlpha))("aaa", "bbb")
    val fo   = s"""<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>a.</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>aaa</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>b.</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>bbb</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |</fo:list-block>""".stripMargin
    run(elem, fo)
  }

  test("render an enumerated list with upper alpha enumeration style") {
    val elem = EnumList(EnumFormat(EnumType.UpperAlpha))("aaa", "bbb")
    val fo   = s"""<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>A.</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>aaa</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>B.</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>bbb</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |</fo:list-block>""".stripMargin
    run(elem, fo)
  }

  test("render an enumerated list with the start value other than 1") {
    val elem = EnumList(EnumFormat(EnumType.Arabic), 7)("aaa", "bbb")
    val fo   = s"""<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>7.</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>aaa</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>8.</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>bbb</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |</fo:list-block>""".stripMargin
    run(elem, fo)
  }

  private def fp(content: String) = ForcedParagraph(List(Text(content)))

  test(
    "render a bullet list with forced paragraphs as list items the same way as normal paragraphs"
  ) {
    val elem = BulletList(fp("aaa"), fp("bbb"))
    val fo   = s"""<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>&#x2022;</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>aaa</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>&#x2022;</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>bbb</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |</fo:list-block>""".stripMargin
    run(elem, fo)
  }

  test(
    "render an enumerated list with forced paragraphs as list items the same way as normal paragraphs"
  ) {
    val elem = EnumList(fp("aaa"), fp("bbb"))
    val fo   = s"""<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>1.</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>aaa</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>2.</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>bbb</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |</fo:list-block>""".stripMargin
    run(elem, fo)
  }

  test("render a definition list with paragraphs") {
    val elem = DefinitionList(
      DefinitionListItem("term 1", p("1"), p("1")),
      DefinitionListItem("term 2", p("2"), p("2"))
    )
    val fo   = s"""<fo:list-block provisional-distance-between-starts="20mm" space-after="6mm">
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>term 1</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>1</fo:block>
                |      <fo:block $defaultParagraphStyles>1</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>term 2</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>2</fo:block>
                |      <fo:block $defaultParagraphStyles>2</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |</fo:list-block>""".stripMargin
    run(elem, fo)
  }

  test("render a definition list with simple flow content") {
    val elem = DefinitionList(
      DefinitionListItem("term 1", p("1")),
      DefinitionListItem("term 2", p("2"))
    )
    val fo   = s"""<fo:list-block provisional-distance-between-starts="20mm" space-after="6mm">
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>term 1</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>1</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |  <fo:list-item space-after="3mm">
                |    <fo:list-item-label end-indent="label-end()">
                |      <fo:block $defaultParagraphStyles>term 2</fo:block>
                |    </fo:list-item-label>
                |    <fo:list-item-body start-indent="body-start()">
                |      <fo:block $defaultParagraphStyles>2</fo:block>
                |    </fo:list-item-body>
                |  </fo:list-item>
                |</fo:list-block>""".stripMargin
    run(elem, fo)
  }

  test("render a paragraph containing a citation link") {
    val elem = BlockSequence(
      p(Text("some "), CitationLink("ref", "label"), Text(" span")),
      Citation("ref", List(p("a"), p("b")), Id("ref"))
    )
    val fo   =
      s"""<fo:block $defaultParagraphStyles>some <fo:footnote>
         |  <fo:inline font-size="8pt" vertical-align="super">[label]</fo:inline>
         |  <fo:footnote-body id="_doc_ref">
         |    <fo:block $defaultParagraphStyles><fo:inline font-size="8pt" vertical-align="super">[label]</fo:inline> a</fo:block>
         |    <fo:block $defaultParagraphStyles>b</fo:block>
         |  </fo:footnote-body>
         |</fo:footnote> span</fo:block>
         |""".stripMargin
    run(elem, fo)
  }

  test("render a paragraph containing a footnote link") {
    val elem = BlockSequence(
      p(Text("some "), FootnoteLink("id", "label"), Text(" span")),
      Footnote("label", List(p("a"), p("b")), Id("id"))
    )
    val fo   =
      s"""<fo:block $defaultParagraphStyles>some <fo:footnote>
         |  <fo:inline font-size="8pt" vertical-align="super">[label]</fo:inline>
         |  <fo:footnote-body id="_doc_id">
         |    <fo:block $defaultParagraphStyles><fo:inline font-size="8pt" vertical-align="super">[label]</fo:inline> a</fo:block>
         |    <fo:block $defaultParagraphStyles>b</fo:block>
         |  </fo:footnote-body>
         |</fo:footnote> span</fo:block>
         |""".stripMargin
    run(elem, fo)
  }

  private val defaultTableProps =
    """border="1pt solid #cccccc" border-collapse="separate" space-after="6mm""""

  private val oddRowBg = """background-color="#f2f2f2""""

  test("render a table without header cells") {
    val elem = Table(Row(BodyCell("a"), BodyCell("b")), Row(BodyCell("c"), BodyCell("d")))
    val fo   = s"""<fo:table $defaultTableProps>
                |  <fo:table-body>
                |    <fo:table-row $oddRowBg>
                |      <fo:table-cell padding="2mm">
                |        <fo:block $defaultParagraphStyles>a</fo:block>
                |      </fo:table-cell>
                |      <fo:table-cell padding="2mm">
                |        <fo:block $defaultParagraphStyles>b</fo:block>
                |      </fo:table-cell>
                |    </fo:table-row>
                |    <fo:table-row>
                |      <fo:table-cell padding="2mm">
                |        <fo:block $defaultParagraphStyles>c</fo:block>
                |      </fo:table-cell>
                |      <fo:table-cell padding="2mm">
                |        <fo:block $defaultParagraphStyles>d</fo:block>
                |      </fo:table-cell>
                |    </fo:table-row>
                |  </fo:table-body>
                |</fo:table>""".stripMargin
    run(elem, fo)
  }

  test("render a table with header cells") {
    val elem = Table(
      TableHead(List(Row(BodyCell("a"), BodyCell("b")))),
      TableBody(List(Row(BodyCell("c"), BodyCell("d"))))
    )
    val fo   =
      s"""<fo:table $defaultTableProps>
         |  <fo:table-header>
         |    <fo:table-row>
         |      <fo:table-cell border-bottom="1pt solid #cccccc" font-weight="bold" padding="2mm">
         |        <fo:block $defaultParagraphStyles>a</fo:block>
         |      </fo:table-cell>
         |      <fo:table-cell border-bottom="1pt solid #cccccc" font-weight="bold" padding="2mm">
         |        <fo:block $defaultParagraphStyles>b</fo:block>
         |      </fo:table-cell>
         |    </fo:table-row>
         |  </fo:table-header>
         |  <fo:table-body>
         |    <fo:table-row $oddRowBg>
         |      <fo:table-cell padding="2mm">
         |        <fo:block $defaultParagraphStyles>c</fo:block>
         |      </fo:table-cell>
         |      <fo:table-cell padding="2mm">
         |        <fo:block $defaultParagraphStyles>d</fo:block>
         |      </fo:table-cell>
         |    </fo:table-row>
         |  </fo:table-body>
         |</fo:table>""".stripMargin
    run(elem, fo)
  }

  test("render a table with a caption") {
    val caption = Caption(List(Text("caption")))
    val elem    =
      Table(Row(BodyCell("a"), BodyCell("b")), Row(BodyCell("c"), BodyCell("d"))).copy(caption =
        caption
      )
    val fo      =
      s"""<fo:block padding-left="20mm" padding-right="20mm" space-after="6mm">
         |  <fo:block font-family="sans-serif" font-size="11pt" font-weight="bold" space-after="3mm">caption</fo:block>
         |  <fo:table $defaultTableProps>
         |    <fo:table-body>
         |      <fo:table-row $oddRowBg>
         |        <fo:table-cell padding="2mm">
         |          <fo:block $defaultParagraphStyles>a</fo:block>
         |        </fo:table-cell>
         |        <fo:table-cell padding="2mm">
         |          <fo:block $defaultParagraphStyles>b</fo:block>
         |        </fo:table-cell>
         |      </fo:table-row>
         |      <fo:table-row>
         |        <fo:table-cell padding="2mm">
         |          <fo:block $defaultParagraphStyles>c</fo:block>
         |        </fo:table-cell>
         |        <fo:table-cell padding="2mm">
         |          <fo:block $defaultParagraphStyles>d</fo:block>
         |        </fo:table-cell>
         |      </fo:table-row>
         |    </fo:table-body>
         |  </fo:table>
         |</fo:block>""".stripMargin
    run(elem, fo)
  }

  test("render a cell using colspan and rowspan attributes") {
    val elem = Cell(BodyCell, List(p("a")), 3, 2)
    val fo   = s"""<fo:table-cell number-columns-spanned="3" number-rows-spanned="2" padding="2mm">
                |  <fo:block $defaultParagraphStyles>a</fo:block>
                |</fo:table-cell>""".stripMargin
    run(elem, fo)
  }

  test("render a cell with two paragraphs") {
    val elem = BodyCell(p("a"), p("b"))
    val fo   = s"""<fo:table-cell padding="2mm">
                |  <fo:block $defaultParagraphStyles>a</fo:block>
                |  <fo:block $defaultParagraphStyles>b</fo:block>
                |</fo:table-cell>""".stripMargin
    run(elem, fo)
  }

  test("render a titled block") {
    val elem = TitledBlock(
      List(Text("some "), Emphasized("em"), Text(" text")),
      List(p("aaa"), Rule(), p("bbb"))
    )
    val fo   =
      s"""<fo:block padding-left="20mm" padding-right="20mm" space-after="6mm">
         |  <fo:block font-family="sans-serif" font-size="11pt" font-weight="bold" space-after="3mm">some <fo:inline font-style="italic">em</fo:inline> text</fo:block>
         |  <fo:block $defaultParagraphStyles>aaa</fo:block>
         |  $ruleBlock
         |  <fo:block $defaultParagraphStyles>bbb</fo:block>
         |</fo:block>""".stripMargin
    run(elem, fo)
  }

  test("render a figure") {
    val elem = Figure(
      Image(InternalTarget(CurrentTree / "image.jpg"), alt = Some("alt")),
      List(Text("some "), Emphasized("caption"), Text(" text")),
      List(p("aaa"), Rule(), p("bbb"))
    )
    val fo   =
      s"""<fo:block space-after="6mm">
         |  <fo:block space-after="3mm" text-align="center"><fo:external-graphic content-width="scale-down-to-fit" height="auto" scaling="uniform" src="/image.jpg" width="85%"/></fo:block>
         |  <fo:block font-family="serif" font-size="0.9em" font-style="italic" space-after="3mm">some <fo:inline font-style="italic">caption</fo:inline> text</fo:block>
         |  <fo:block font-size="0.9em" font-style="italic">
         |    <fo:block $defaultParagraphStyles>aaa</fo:block>
         |    $ruleBlock
         |    <fo:block $defaultParagraphStyles>bbb</fo:block>
         |  </fo:block>
         |</fo:block>""".stripMargin
    run(elem, fo)
  }

  test("render a choice group without selections") {
    val elem = Selection(
      "config",
      Seq(
        Choice("name-a", "label-a", List(p("common"), p("11\n22"))),
        Choice("name-b", "label-b", List(p("common"), p("33\n44")))
      )
    )
    val fo   =
      s"""<fo:block $defaultParagraphStyles><fo:inline font-weight="bold">label-a</fo:inline></fo:block>
         |<fo:block $defaultParagraphStyles>common</fo:block>
         |<fo:block $defaultParagraphStyles>11
         |22</fo:block>
         |<fo:block $defaultParagraphStyles><fo:inline font-weight="bold">label-b</fo:inline></fo:block>
         |<fo:block $defaultParagraphStyles>common</fo:block>
         |<fo:block $defaultParagraphStyles>33
         |44</fo:block>""".stripMargin
    run(elem, fo)
  }

  test("render a document with two paragraphs separated by a horizontal rule") {
    val elem = RootElement(p("aaa"), Rule(), p("bbb"))
    val fo   = s"""<fo:block $defaultParagraphStyles>aaa</fo:block>
                |$ruleBlock
                |<fo:block $defaultParagraphStyles>bbb</fo:block>""".stripMargin
    run(elem, fo)
  }

  test("render a document with two nested sections") {
    val nested   = Section(Header(2, Text("Title 2")), List(p("Line 1"), p("Line 2")))
    val rootElem =
      RootElement(Section(Header(1, Text("Title 1")), List(p("Line 1"), p("Line 2"))), nested)
    val fo       =
      s"""<fo:block font-family="sans-serif" font-size="14pt" font-weight="bold" keep-with-next="always" space-after="6mm" space-before="12mm">Title 1</fo:block>
         |<fo:block $defaultParagraphStyles>Line 1</fo:block>
         |<fo:block $defaultParagraphStyles>Line 2</fo:block>
         |<fo:block font-family="sans-serif" font-size="14pt" font-weight="bold" keep-with-next="always" space-after="3mm" space-before="7mm">Title 2</fo:block>
         |<fo:block $defaultParagraphStyles>Line 1</fo:block>
         |<fo:block $defaultParagraphStyles>Line 2</fo:block>""".stripMargin
    run(rootElem, fo)
  }

  test("render a navigation list with two levels") {
    def link(level: Int, titleNum: Int, children: Seq[NavigationItem] = Nil): NavigationItem = {
      val target =
        Some(NavigationLink(InternalTarget(Root / s"doc#title-$titleNum").relativeTo(Root / "doc")))
      val title  = SpanSequence("Title " + titleNum)
      NavigationItem(title, children, target, options = Style.level(level))
    }
    val navList     = NavigationList(
      Seq(
        link(1, 2, Seq(link(2, 3))),
        link(1, 4, Seq(link(2, 5)))
      )
    )
    val level1Props =
      """color="#931813" font-family="serif" font-size="22pt" font-weight="bold" keep-with-next="always" line-height="1.5" margin-left="0mm" space-after="0mm" space-before="15mm" text-align="justify" text-align-last="center" text-transform="uppercase""""
    val level2Props =
      """color="#931813" font-family="serif" font-size="17pt" keep-with-previous="always" line-height="1.5" margin-left="4mm" space-after="0mm" space-before="7mm" text-align="justify" text-align-last="justify""""
    val leader      =
      """<fo:leader leader-pattern="dots" padding-left="2mm" padding-right="2mm"></fo:leader>"""

    val fo =
      s"""<fo:block $level1Props><fo:basic-link color="#007c99" font-weight="bold" internal-destination="_doc_title-2">Title 2$leader<fo:page-number-citation ref-id="_doc_title-2" /></fo:basic-link></fo:block>
         |<fo:block $level2Props><fo:basic-link color="#931813" font-weight="bold" internal-destination="_doc_title-3">Title 3$leader<fo:page-number-citation ref-id="_doc_title-3" /></fo:basic-link></fo:block>
         |<fo:block $level1Props><fo:basic-link color="#007c99" font-weight="bold" internal-destination="_doc_title-4">Title 4$leader<fo:page-number-citation ref-id="_doc_title-4" /></fo:basic-link></fo:block>
         |<fo:block $level2Props><fo:basic-link color="#931813" font-weight="bold" internal-destination="_doc_title-5">Title 5$leader<fo:page-number-citation ref-id="_doc_title-5" /></fo:basic-link></fo:block>""".stripMargin
    run(RootElement(navList), fo)
  }

  test("render a title containing emphasized text") {
    val elem = Title(Text("some "), Emphasized("em"), Text(" text"))
    val fo   =
      """<fo:block color="#007c99" font-family="sans-serif" font-size="24pt" font-weight="bold" keep-with-next="always" space-after="6mm" space-before="0mm">some <fo:inline font-style="italic">em</fo:inline> text</fo:block>"""
    run(elem, fo)
  }

  test("render a title containing a section number") {
    val elem = Title(SectionNumber(Seq(1, 2, 3)), Text("Title"))
    val fo   =
      """<fo:block color="#007c99" font-family="sans-serif" font-size="24pt" font-weight="bold" keep-with-next="always" space-after="6mm" space-before="0mm">1.2.3 Title</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing emphasized text") {
    val elem = p(Text("some "), Emphasized("em"), Text(" text"))
    val fo   =
      s"""<fo:block $defaultParagraphStyles>some <fo:inline font-style="italic">em</fo:inline> text</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing strong text") {
    val elem = p(Text("some "), Strong("strong"), Text(" text"))
    val fo   =
      s"""<fo:block $defaultParagraphStyles>some <fo:inline font-weight="bold">strong</fo:inline> text</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing a deleted span") {
    val elem = p(Text("some "), Deleted(Seq(Text("deleted"))), Text(" text"))
    val fo   =
      s"""<fo:block $defaultParagraphStyles>some <fo:inline text-decoration="line-through">deleted</fo:inline> text</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing an inserted span") {
    val elem = p(Text("some "), Inserted("inserted"), Text(" text"))
    val fo   =
      s"""<fo:block $defaultParagraphStyles>some <fo:inline text-decoration="underline">inserted</fo:inline> text</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing a literal span") {
    val elem = p(Text("some "), Literal("code"), Text(" span"))
    val fo   =
      s"""<fo:block $defaultParagraphStyles>some <fo:inline font-family="monospaced" font-size="0.9em">code</fo:inline> span</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing a code span") {
    val elem = p(Text("some "), InlineCode("banana-script", List(Text("code"))), Text(" span"))
    val fo   =
      s"""<fo:block $defaultParagraphStyles>some <fo:inline font-family="monospaced" font-size="0.9em">code</fo:inline> span</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing a code span with syntax highlighting") {
    val elem  = p(Text("some "), CodeSpan("code", CodeCategory.Keyword), Text(" span"))
    val fo    =
      s"""<fo:block $defaultParagraphStyles>some <fo:inline font-weight="bold">code</fo:inline> span</fo:block>"""
    val style = StyleDeclaration(
      StyleSelector(Set(StylePredicate.StyleName("keyword"))),
      Map("font-weight" -> "bold")
    )
    run(elem, style, fo)
  }

  test("render a paragraph containing a link without title") {
    val elem = p(Text("some "), SpanLink.external("/foo")("link"), Text(" span"))
    val fo   =
      s"""<fo:block $defaultParagraphStyles>some <fo:basic-link color="#931813" external-destination="/foo" font-weight="bold">link</fo:basic-link> span</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing a link with title") {
    val elem =
      p(Text("some "), SpanLink.external("/foo")("link").copy(title = Some("title")), Text(" span"))
    val fo   =
      s"""<fo:block $defaultParagraphStyles>some <fo:basic-link color="#931813" external-destination="/foo" font-weight="bold">link</fo:basic-link> span</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing a link with emphasized text") {
    val elem =
      p(Text("some "), SpanLink.external("/foo")(Text("link"), Emphasized("text")), Text(" span"))
    val fo   = s"""<fo:block $defaultParagraphStyles>some """ +
      """<fo:basic-link color="#931813" external-destination="/foo" font-weight="bold">link<fo:inline font-style="italic">text</fo:inline></fo:basic-link> span</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing an internal link with emphasized text") {
    val elem =
      p(Text("some "), SpanLink.internal("#foo")(Text("link"), Emphasized("text")), Text(" span"))
    val fo   = s"""<fo:block $defaultParagraphStyles>some """ +
      """<fo:basic-link color="#931813" font-weight="bold" internal-destination="_doc_foo">link<fo:inline font-style="italic">text</fo:inline></fo:basic-link> span</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing an internal link with a fragment part") {
    val elem = p(
      Text("some "),
      SpanLink.internal("../bar#foo")(Text("link"), Emphasized("text")),
      Text(" span")
    )
    val fo   = s"""<fo:block $defaultParagraphStyles>some """ +
      """<fo:basic-link color="#931813" font-weight="bold" internal-destination="_bar_foo">link<fo:inline font-style="italic">text</fo:inline></fo:basic-link> span</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing an internal link without a fragment part") {
    val elem =
      p(Text("some "), SpanLink.internal("../bar")(Text("link"), Emphasized("text")), Text(" span"))
    val fo   = s"""<fo:block $defaultParagraphStyles>some """ +
      """<fo:basic-link color="#931813" font-weight="bold" internal-destination="_bar">link<fo:inline font-style="italic">text</fo:inline></fo:basic-link> span</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing an internal link with a filename without suffix") {
    val elem =
      p(Text("some "), SpanLink.internal("../bar")(Text("link"), Emphasized("text")), Text(" span"))
    val fo   = s"""<fo:block $defaultParagraphStyles>some """ +
      """<fo:basic-link color="#931813" font-weight="bold" internal-destination="_bar">link<fo:inline font-style="italic">text</fo:inline></fo:basic-link> span</fo:block>"""
    run(elem, fo)
  }

  test("translate to external URL when an internal link is not defined for PDF as a target") {
    val target     = ResolvedInternalTarget(
      Path.parse("/foo#ref"),
      RelativePath.parse("foo#ref"),
      TargetFormats.Selected("html")
    )
    val elem       = p(Text("some "), SpanLink(target)("link"), Text(" span"))
    val fo         = s"""<fo:block $defaultParagraphStyles>some """ +
      """<fo:basic-link color="#931813" external-destination="http://external/foo.html#ref" font-weight="bold">link</fo:basic-link> span</fo:block>"""
    val translator = {
      val config  = ConfigBuilder.empty.withValue(LaikaKeys.siteBaseURL, "http://external/").build
      val tConfig = TranslatorConfig.readFrom(config).getOrElse(TranslatorConfig.empty)
      val lookup: Path => Option[PathAttributes] = path =>
        if (path == Root / "doc") Some(PathAttributes(isStatic = false, isVersioned = false))
        else None
      ConfigurablePathTranslator(tConfig, OutputContext("fo", "pdf"), Root / "doc", lookup)
    }
    assertEquals(
      defaultRenderer.render(elem, Root / "doc", translator, TestTheme.foStyles),
      Right(fo)
    )
  }

  test("render a paragraph containing a raw internal link") {
    val elem = p(Text("some "), RawLink.internal("#foo"), Text(" span"))
    val fo   = s"""<fo:block $defaultParagraphStyles>some _doc_foo span</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing a raw external link") {
    val elem = p(Text("some "), RawLink.external("/foo"), Text(" span"))
    val fo   = s"""<fo:block $defaultParagraphStyles>some /foo span</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing only an image centered") {
    val elem = p(Image(imageTarget, alt = Some("img")))
    val fo   =
      s"""<fo:block space-after="3mm" text-align="center"><fo:external-graphic content-width="scale-down-to-fit" height="auto" scaling="uniform" src="/foo.jpg" width="85%"/></fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing an image without title") {
    val elem = p(Text("some "), Image(imageTarget, alt = Some("img")), Text(" span"))
    val fo   =
      s"""<fo:block $defaultParagraphStyles>some <fo:external-graphic content-width="scale-down-to-fit" height="auto" scaling="uniform" src="/foo.jpg" width="85%"/> span</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing an image with title") {
    val elem =
      p(Text("some "), Image(imageTarget, alt = Some("img"), title = Some("title")), Text(" span"))
    val fo   =
      s"""<fo:block $defaultParagraphStyles>some <fo:external-graphic content-width="scale-down-to-fit" height="auto" scaling="uniform" src="/foo.jpg" width="85%"/> span</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing an image, ignoring intrinsic width and height attributes") {
    val elem = p(
      Text("some "),
      Image(
        imageTarget,
        alt = Some("img"),
        width = Some(LengthUnit.px(120)),
        height = Some(LengthUnit.px(80))
      ),
      Text(" span")
    )
    val fo   =
      s"""<fo:block $defaultParagraphStyles>some <fo:external-graphic content-width="scale-down-to-fit" height="auto" scaling="uniform" src="/foo.jpg" width="85%"/> span</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing an image with vertical align style") {
    val elem =
      p(Text("some "), Image(imageTarget, alt = Some("img")).withStyle("align-top"), Text(" span"))
    val fo   =
      s"""<fo:block $defaultParagraphStyles>some <fo:external-graphic content-width="scale-down-to-fit" height="auto" scaling="uniform" src="/foo.jpg" vertical-align="top" width="85%"/> span</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing a link with an icon glyph") {
    val elem = p(
      Text("some "),
      SpanLink.external("/foo")(IconGlyph('\uefa2', options = Styles("icofont-laika"))),
      Text(" span")
    )
    val fo   =
      s"""<fo:block $defaultParagraphStyles>some <fo:basic-link color="#931813" external-destination="/foo" font-weight="bold"><fo:inline font-family="IcoFont" font-size="16pt">&#xefa2;</fo:inline></fo:basic-link> span</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing a link with an inline SVG icon") {
    val openTag  =
      """<svg class="svg-icon" width="100%" height="100%" viewBox="0 0 100 100" version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" xml:space="preserve" style="fill-rule:evenodd;clip-rule:evenodd;stroke-linejoin:round;stroke-miterlimit:2;">"""
    val svgRest  = s"""  <g class="svg-shape">
                     |    <path d="M75,47.5c13.246,0 24,10.754 24,24c0,13.246 -10.754,24"/>
                     |  </g>
                     |</svg>""".stripMargin
    val svg      = s"""$openTag
                 |$svgRest""".stripMargin
    val expected = s"""$openTag
                      |  <style>.svg-shape { fill: #007c99; }</style>
                      |$svgRest""".stripMargin
    val elem     = p(Text("some "), SpanLink.external("/foo")(InlineSVGIcon(svg)), Text(" span"))
    val fo       =
      s"""<fo:block $defaultParagraphStyles>some <fo:basic-link color="#931813" external-destination="/foo" font-weight="bold"><fo:instream-foreign-object content-height="1.5em" content-width="1.5em">$expected</fo:instream-foreign-object></fo:basic-link> span</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing an unresolved link reference") {
    val elem =
      p(Text("some "), LinkIdReference("id", generatedSource("[link] [id]"))("link"), Text(" span"))
    val fo   =
      s"""<fo:block $defaultParagraphStyles>some <fo:inline font-family="monospaced" font-size="0.9em">[link] [id]</fo:inline> span</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing an unresolved image reference") {
    val elem = p(
      Text("some "),
      ImageIdReference("img", "id", source("![img] [id]", "![img] [id]")),
      Text(" span")
    )
    val fo   =
      s"""<fo:block $defaultParagraphStyles>some <fo:inline font-family="monospaced" font-size="0.9em">![img] [id]</fo:inline> span</fo:block>"""
    run(elem, fo)
  }

  test("render a paragraph containing an internal link target") {
    val elem = p(Text("some "), InternalLinkTarget(Id("target")), Text(" span"))
    val fo   =
      s"""<fo:block $defaultParagraphStyles>some <fo:inline id="_doc_target"></fo:inline> span</fo:block>"""
    run(elem, fo)
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

  test("render a template string without creating XML entities") {
    val elem = TemplateRoot(TemplateString("aa & bb"))
    run(elem, "aa & bb")
  }

  test("render a template root containing a TemplateElement") {
    val elem = TemplateRoot(
      TemplateString("aa"),
      TemplateElement(BlockSequence(List(p("aaa"), p("bbb")), Styles("foo"))),
      TemplateString("cc")
    )
    val fo   = s"""aa<fo:block>
                |  <fo:block $defaultParagraphStyles>aaa</fo:block>
                |  <fo:block $defaultParagraphStyles>bbb</fo:block>
                |</fo:block>cc""".stripMargin
    run(elem, fo)
  }

  private val warningProps =
    """background-color="#fcfacd" border="1pt solid #b1a400" color="#b1a400" padding="1pt 2pt""""

  test("render a runtime message") {
    val fo = s"""<fo:inline $warningProps>some message</fo:inline>"""
    run(RuntimeMessage(MessageLevel.Warning, "some message"), MessageFilter.Warning, fo)
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
    val fo   = s"""<fo:block $defaultParagraphStyles>fallback</fo:block>"""
    run(elem, fo)
  }

  test(
    "render an invalid block without the runtime message if the configured message level is higher"
  ) {
    val elem = InvalidBlock(
      RuntimeMessage(MessageLevel.Warning, "some message"),
      GeneratedSource,
      p("fallback")
    )
    val fo   = s"""<fo:block $defaultParagraphStyles>fallback</fo:block>"""
    run(elem, MessageFilter.Error, fo)
  }

  test(
    "render an invalid block with the runtime message if the configured message level is lower or equal"
  ) {
    val elem = InvalidBlock(
      RuntimeMessage(MessageLevel.Warning, "some message"),
      GeneratedSource,
      p("fallback")
    )
    val fo   = s"""<fo:block $defaultParagraphStyles>""" +
      s"""<fo:inline $warningProps>some message</fo:inline>""" +
      s"""</fo:block><fo:block $defaultParagraphStyles>fallback</fo:block>"""
    run(elem, MessageFilter.Info, fo)
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
    run(elem, MessageFilter.Error, "fallback")
  }

  test(
    "render an invalid span with the runtime message if the configured message level is lower or equal"
  ) {
    val elem = InvalidSpan(
      RuntimeMessage(MessageLevel.Warning, "some message"),
      GeneratedSource,
      Text("fallback")
    )
    val fo   = s"""<fo:inline $warningProps>some message</fo:inline> fallback"""
    run(elem, MessageFilter.Info, fo)
  }

  val defaultCodeBlockStyles =
    """background-color="#F6F1EF" color="#362E21" font-family="monospaced" font-size="0.9em" fox:border-radius="2mm" line-height="1.4" linefeed-treatment="preserve" margin-left="2mm" margin-right="2mm" padding="2mm" space-after="6mm" white-space-collapse="false" white-space-treatment="preserve""""

  val monoBlock = s"""<fo:block $defaultCodeBlockStyles>"""

  test("render a literal block") {
    val code = """line 1
                 |
                 |    <line 2
                 |
                 |line 3""".stripMargin
    val elem = LiteralBlock(code)
    val fo   = monoBlock + code.replace("<", "&lt;") + "</fo:block>"
    run(elem, fo)
  }

  test("render a parsed literal block") {
    val code = """line 1
                 |
                 |    #<line 2
                 |
                 |line 3""".stripMargin.split("#")
    val elem = ParsedLiteralBlock(List(Text(code(0)), Emphasized("em"), Text(code(1))))
    val fo   = monoBlock + code(0) + """<fo:inline font-style="italic">em</fo:inline>""" + code(
      1
    ).replace("<", "&lt;") + "</fo:block>"
    run(elem, fo)
  }

  test("render a code block") {
    val code = """line 1
                 |
                 |    <line 2
                 |
                 |line 3""".stripMargin
    val elem = CodeBlock("banana-script", List(Text(code)))
    val fo   = monoBlock + code.replace("<", "&lt;") + "</fo:block>"
    run(elem, fo)
  }

  test("render a literal block inside a blockquote without indentation") {
    val code = """line 1
                 |
                 |    line 2
                 |
                 |line 3""".stripMargin
    val fo   =
      s"""<fo:block font-style="italic" margin-left="8mm" margin-right="8mm" space-after="3mm">
         |  %s</fo:block>
         |</fo:block>""".stripMargin.format(monoBlock + code)
    val elem = QuotedBlock(LiteralBlock(code))
    run(elem, fo)
  }

  test("render a parsed literal block inside a blockquote without indentation") {
    val code = """line 1
                 |
                 |    line 2
                 |
                 |line 3""".stripMargin
    val fo   =
      s"""<fo:block font-style="italic" margin-left="8mm" margin-right="8mm" space-after="3mm">
         |  %s:<fo:inline font-style="italic">%s</fo:inline>:</fo:block>
         |</fo:block>""".stripMargin.format(monoBlock, code)
    val elem = QuotedBlock(ParsedLiteralBlock(List(Text(":"), Emphasized(code), Text(":"))))
    run(elem, fo)
  }

  test("render a code block inside a blockquote without indentation") {
    val code = """line 1
                 |
                 |    line 2
                 |
                 |line 3""".stripMargin
    val fo   =
      s"""<fo:block font-style="italic" margin-left="8mm" margin-right="8mm" space-after="3mm">
         |  %s:<fo:inline font-style="italic">%s</fo:inline>:</fo:block>
         |</fo:block>""".stripMargin.format(monoBlock, code)
    val elem = QuotedBlock(CodeBlock("banana-script", List(Text(":"), Emphasized(code), Text(":"))))
    run(elem, fo)
  }

  test("render a table cell unformatted") {
    val elem = BodyCell(p("a"), p("b"))
    val fo   = s"""<fo:table-cell padding="2mm">
                |<fo:block $defaultParagraphStyles>a</fo:block>
                |<fo:block $defaultParagraphStyles>b</fo:block>
                |</fo:table-cell>""".stripMargin
    runUnformatted(elem, fo)
  }

  test("render raw content unchanged if the xsl-fo format is specified") {
    val raw  = "<fo:block>some text</fo:block>"
    val elem = RawContent(NonEmptySet.of("fo", "spooky"), raw)
    run(elem, raw)
  }

  test("ignore raw content if the xsl-fo format is not specified") {
    val raw  = "<fo:block>some text</fo:block>"
    val elem = RawContent(NonEmptySet.of("dodgy", "spooky"), raw)
    run(elem, "")
  }

  test("render an embedded root with correct indentation") {
    val elem = RootElement(
      TemplateRoot(
        TemplateString("<fo:block>\n  "),
        EmbeddedRoot(List(p("aaa"), p("bbb")), 2),
        TemplateString("\n</fo:block>")
      )
    )
    val fo   = s"""<fo:block>
                |  <fo:block $defaultParagraphStyles>aaa</fo:block>
                |  <fo:block $defaultParagraphStyles>bbb</fo:block>
                |</fo:block>""".stripMargin
    run(elem, fo)
  }

  test("render an embedded root without indentation") {
    val elem = RootElement(
      TemplateRoot(
        TemplateString("<fo:block>\n"),
        EmbeddedRoot(p("aaa"), p("bbb")),
        TemplateString("\n</fo:block>")
      )
    )
    val fo   = s"""<fo:block>
                |<fo:block $defaultParagraphStyles>aaa</fo:block>
                |<fo:block $defaultParagraphStyles>bbb</fo:block>
                |</fo:block>""".stripMargin
    run(elem, fo)
  }

}
