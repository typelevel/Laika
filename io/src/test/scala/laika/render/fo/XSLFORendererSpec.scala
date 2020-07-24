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
import cats.implicits._
import laika.api.Renderer
import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentTree
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.format.XSLFO
import laika.parse.code.CodeCategory
import laika.rewrite.nav.BasicPathTranslator
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class XSLFORendererSpec extends AnyFlatSpec
  with Matchers
  with ModelBuilder {

  private val pathTranslator = BasicPathTranslator(XSLFO.fileSuffix)
  
  private val defaultRenderer = Renderer.of(XSLFO).build

  private val defaultParagraphStyles = """font-family="serif" font-size="10pt" line-height="1.5" space-after="3mm" text-align="justify""""
  private val ruleBlock = """<fo:block space-after="3mm"><fo:leader leader-length="100%" leader-pattern="rule" rule-style="solid" rule-thickness="2pt"></fo:leader></fo:block>""" 
  
  def render (elem: Element): String = render(elem, TestTheme.foStyles)

  def render (elem: Element, style: StyleDeclaration): String = 
    render(elem, TestTheme.foStyles ++ StyleDeclarationSet(Path.Root, style))

  def render (elem: Element, style: StyleDeclarationSet): String = 
    defaultRenderer.render(elem, Root, pathTranslator, style)
  
  def render (elem: Element, messageFilter: MessageFilter): String =
    Renderer.of(XSLFO).renderMessages(messageFilter).build.render(elem, Root, pathTranslator, TestTheme.foStyles)

  def renderUnformatted (elem: Element): String = 
    Renderer.of(XSLFO).unformatted.build.render(elem, Root, pathTranslator, TestTheme.foStyles)

  val imageTarget = InternalTarget(Root / "foo.jpg", CurrentTree / "foo.jpg")
  

  "The XSLFO renderer" should "render a paragraph with plain text" in {
    val elem = p("some text")
    render (elem) should be (s"""<fo:block $defaultParagraphStyles>some text</fo:block>""")
  }

  it should "render a document with two paragraphs with plain text" in {
    val elem = root( p("aaa"), p("bbb"))
    val fo = s"""<fo:block $defaultParagraphStyles>aaa</fo:block>
               |<fo:block $defaultParagraphStyles>bbb</fo:block>""".stripMargin
    render (elem) should be (fo)
  }

  it should "render a block sequence with a custom style" in {
    val elem = root(BlockSequence(List(p("aaa"), p("bbb")), Styles("foo")))
    val fo = s"""<fo:block font-weight="bold">
               |  <fo:block $defaultParagraphStyles>aaa</fo:block>
               |  <fo:block $defaultParagraphStyles>bbb</fo:block>
               |</fo:block>""".stripMargin
    val style = StyleDeclaration(StyleSelector(Set(StylePredicate.StyleName("foo"))), Map("font-weight" -> "bold"))
    render (elem, style) should be (fo)
  }

  it should "render a block sequence without a style" in {
    val elem = root(p("aaa"), BlockSequence(p("bbb"), p("ccc")))
    val fo = s"""<fo:block $defaultParagraphStyles>aaa</fo:block>
               |<fo:block $defaultParagraphStyles>bbb</fo:block>
               |<fo:block $defaultParagraphStyles>ccc</fo:block>""".stripMargin
    render (elem) should be (fo)
  }

  it should "render a block sequence with a single element" in {
    val elem = root(p("aaa"), BlockSequence(p("bbb")), p("ccc"))
    val fo = s"""<fo:block $defaultParagraphStyles>aaa</fo:block>
               |<fo:block $defaultParagraphStyles>bbb</fo:block>
               |<fo:block $defaultParagraphStyles>ccc</fo:block>""".stripMargin
    render (elem) should be (fo)
  }

  it should "render a blockquote with two paragraphs with plain text" in {
    val elem = QuotedBlock( p("aaa"), p("bbb"))
    val fo = s"""<fo:block font-style="italic" margin-left="8mm" margin-right="8mm" space-after="3mm">
               |  <fo:block $defaultParagraphStyles>aaa</fo:block>
               |  <fo:block $defaultParagraphStyles>bbb</fo:block>
               |</fo:block>""".stripMargin
    render (elem) should be (fo)
  }

  it should "render a blockquote with one paragraph with plain text" in {
    val elem = QuotedBlock(p("aaa"))
    val fo = s"""<fo:block font-style="italic" margin-left="8mm" margin-right="8mm" space-after="3mm">
               |  <fo:block $defaultParagraphStyles>aaa</fo:block>
               |</fo:block>""".stripMargin
    render (elem) should be (fo)
  }

  it should "render a blockquote with an attribution" in {
    val elem = quote("aaa","bbb")
    val fo = s"""<fo:block font-style="italic" margin-left="8mm" margin-right="8mm" space-after="3mm">
               |  <fo:block $defaultParagraphStyles>aaa</fo:block>
               |  <fo:block font-family="serif" font-size="10pt" line-height="1.5" text-align="right">bbb</fo:block>
               |</fo:block>""".stripMargin
    render (elem) should be (fo)
  }

  it should "render a bullet list with simple flow content" in {
    val elem = bulletList("aaa","bbb")
    val fo = s"""<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
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
    render (elem) should be (fo)
  }

  it should "render a bullet list with a nested list" in {
    val elem = BulletList(Seq(BulletListItem(Seq(SpanSequence(Text("aaa")), bulletList("bbb")), StringBullet("*"))), StringBullet("*"))
    val fo = s"""<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
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
    render (elem) should be (fo)
  }

  it should "render an enumerated list with simple flow content" in {
    val elem = enumList("aaa", "bbb")
    val fo = s"""<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
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
    render (elem) should be (fo)
  }

  it should "render an enumerated list with lower roman enumeration style" in {
    val elem = enumList(EnumFormat(EnumType.LowerRoman))("aaa", "bbb")
    val fo = s"""<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
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
    render (elem) should be (fo)
  }

  it should "render an enumerated list with upper roman enumeration style" in {
    val elem = enumList(EnumFormat(EnumType.UpperRoman))("aaa", "bbb")
    val fo = s"""<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
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
    render (elem) should be (fo)
  }

  it should "render an enumerated list with lower alpha enumeration style" in {
    val elem = enumList(EnumFormat(EnumType.LowerAlpha))("aaa", "bbb")
    val fo = s"""<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
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
    render (elem) should be (fo)
  }

  it should "render an enumerated list with upper alpha enumeration style" in {
    val elem = enumList(EnumFormat(EnumType.UpperAlpha))("aaa", "bbb")
    val fo = s"""<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
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
    render (elem) should be (fo)
  }

  it should "render an enumerated list with the start value other than 1" in {
    val elem = enumList(EnumFormat(EnumType.Arabic), 7)("aaa", "bbb")
    val fo = s"""<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
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
    render (elem) should be (fo)
  }

  private def fp (content: String) = ForcedParagraph(List(Text(content)))

  it should "render a bullet list with forced paragraphs as list items the same way as normal paragraphs" in {
    val elem = bulletList(fp("aaa"), fp("bbb"))
    val fo = s"""<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
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
    render (elem) should be (fo)
  }

  it should "render an enumerated list with forced paragraphs as list items the same way as normal paragraphs" in {
    val elem = enumList(fp("aaa"), fp("bbb"))
    val fo = s"""<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
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
    render (elem) should be (fo)
  }

  it should "render a definition list with paragraphs" in {
    val elem = DefinitionList(Seq(
      defListItem("term 1", p("1"), p("1")),
      defListItem("term 2", p("2"), p("2"))
    ))
    val fo = s"""<fo:list-block provisional-distance-between-starts="20mm" space-after="6mm">
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
    render (elem) should be (fo)
  }

  it should "render a definition list with simple flow content" in {
    val elem = DefinitionList(Seq(
      defListItem("term 1", p("1")),
      defListItem("term 2", p("2"))
    ))
    val fo = s"""<fo:list-block provisional-distance-between-starts="20mm" space-after="6mm">
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
    render (elem) should be (fo)
  }

  it should "render a paragraph containing a citation link" in {
    val elem = BlockSequence(p(Text("some "), CitationLink("ref","label"), Text(" span")), Citation("ref", List(p("a"),p("b")), Id("ref")))
    val fo = s"""<fo:block $defaultParagraphStyles>some <fo:footnote>
               |  <fo:inline font-size="8pt" vertical-align="super">[label]</fo:inline>
               |  <fo:footnote-body id="__ref">
               |    <fo:block $defaultParagraphStyles><fo:inline font-size="8pt" vertical-align="super">[label]</fo:inline> a</fo:block>
               |    <fo:block $defaultParagraphStyles>b</fo:block>
               |  </fo:footnote-body>
               |</fo:footnote> span</fo:block>
               |""".stripMargin
    render (elem) should be (fo)
  }

  it should "render a paragraph containing a footnote link" in {
    val elem = BlockSequence(p(Text("some "), FootnoteLink("id","label"), Text(" span")), Footnote("label", List(p("a"),p("b")), Id("id")))
    val fo = s"""<fo:block $defaultParagraphStyles>some <fo:footnote>
               |  <fo:inline font-size="8pt" vertical-align="super">[label]</fo:inline>
               |  <fo:footnote-body id="__id">
               |    <fo:block $defaultParagraphStyles><fo:inline font-size="8pt" vertical-align="super">[label]</fo:inline> a</fo:block>
               |    <fo:block $defaultParagraphStyles>b</fo:block>
               |  </fo:footnote-body>
               |</fo:footnote> span</fo:block>
               |""".stripMargin
    render (elem) should be (fo)
  }
  
  private val defaultTableProps = """border="1pt solid #cccccc" border-collapse="separate" space-after="6mm""""
  private val oddRowBg = """background-color="#f2f2f2""""

  it should "render a table without header cells" in {
    val elem = Table(Row(BodyCell("a"),BodyCell("b")),Row(BodyCell("c"),BodyCell("d")))
    val fo = s"""<fo:table $defaultTableProps>
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
    render(elem) should be (fo)
  }

  it should "render a table with header cells" in {
    val elem = Table(TableHead(List(Row(BodyCell("a"), BodyCell("b")))),
      TableBody(List(Row(BodyCell("c"), BodyCell("d")))))
    val fo = s"""<fo:table $defaultTableProps>
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
    render(elem) should be (fo)
  }

  it should "render a table with a caption" in {
    val caption = Caption(List(Text("caption")))
    val elem = Table(Row(BodyCell("a"),BodyCell("b")),Row(BodyCell("c"),BodyCell("d"))).copy(caption = caption)
    val fo = s"""<fo:block padding-left="20mm" padding-right="20mm" space-after="6mm">
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
    render(elem) should be (fo)
  }

  it should "render a cell using colspan and rowspan attributes" in {
    val elem = cell("a",3,2)
    val fo = s"""<fo:table-cell number-columns-spanned="3" number-rows-spanned="2" padding="2mm">
               |  <fo:block $defaultParagraphStyles>a</fo:block>
               |</fo:table-cell>""".stripMargin
    render(elem) should be (fo)
  }

  it should "render a cell with two paragraphs" in {
    val elem = BodyCell(p("a"),p("b"))
    val fo = s"""<fo:table-cell padding="2mm">
               |  <fo:block $defaultParagraphStyles>a</fo:block>
               |  <fo:block $defaultParagraphStyles>b</fo:block>
               |</fo:table-cell>""".stripMargin
    render(elem) should be (fo)
  }

  it should "render a titled block" in {
    val elem = TitledBlock(List(Text("some "), Emphasized("em"), Text(" text")), List(p("aaa"), Rule(), p("bbb")))
    val fo = s"""<fo:block padding-left="20mm" padding-right="20mm" space-after="6mm">
               |  <fo:block font-family="sans-serif" font-size="11pt" font-weight="bold" space-after="3mm">some <fo:inline font-style="italic">em</fo:inline> text</fo:block>
               |  <fo:block $defaultParagraphStyles>aaa</fo:block>
               |  $ruleBlock
               |  <fo:block $defaultParagraphStyles>bbb</fo:block>
               |</fo:block>""".stripMargin
    render(elem) should be (fo)
  }

  it should "render a figure" in {
    val elem = Figure(Image("alt", InternalTarget(Root / "image.jpg", CurrentTree / "image.jpg")), List(Text("some "), Emphasized("caption"), Text(" text")), List(p("aaa"), Rule(), p("bbb")))
    val fo = s"""<fo:block space-after="6mm">
               |  <fo:block space-after="3mm" text-align="center"><fo:external-graphic content-width="scale-down-to-fit" scaling="uniform" src="/image.jpg" width="85%"/></fo:block>
               |  <fo:block font-family="serif" font-size="9pt" font-style="italic" space-after="3mm">some <fo:inline font-style="italic">caption</fo:inline> text</fo:block>
               |  <fo:block font-size="9pt" font-style="italic">
               |    <fo:block $defaultParagraphStyles>aaa</fo:block>
               |    $ruleBlock
               |    <fo:block $defaultParagraphStyles>bbb</fo:block>
               |  </fo:block>
               |</fo:block>""".stripMargin
    render(elem) should be (fo)
  }
  
  it should "render a choice group without selections" in {
    val elem = ChoiceGroup("config", Seq(
      Choice("name-a","label-a", List(p("common"), p("11\n22"))),
      Choice("name-b","label-b", List(p("common"), p("33\n44")))
    ))
    val fo = s"""<fo:block $defaultParagraphStyles><fo:inline font-weight="bold">label-a</fo:inline></fo:block>
     |<fo:block $defaultParagraphStyles>common</fo:block>
     |<fo:block $defaultParagraphStyles>11
     |22</fo:block>
     |<fo:block $defaultParagraphStyles><fo:inline font-weight="bold">label-b</fo:inline></fo:block>
     |<fo:block $defaultParagraphStyles>common</fo:block>
     |<fo:block $defaultParagraphStyles>33
     |44</fo:block>""".stripMargin
    render(elem) should be (fo)
  }

  it should "render a document with two paragraphs separated by a horizontal rule" in {
    val elem = root( p("aaa"), Rule(), p("bbb"))
    val fo = s"""<fo:block $defaultParagraphStyles>aaa</fo:block>
               |$ruleBlock
               |<fo:block $defaultParagraphStyles>bbb</fo:block>""".stripMargin
    render (elem) should be (fo)
  }

  it should "render a document with two nested sections" in {
    val nested = Section(Header(2, Text("Title 2")), List(p("Line 1"), p("Line 2")))
    val rootElem = root(Section(Header(1, Text("Title 1")), List(p("Line 1"), p("Line 2"))), nested)
    val fo = s"""<fo:block font-family="sans-serif" font-size="14pt" font-weight="bold" keep-with-next="always" space-after="6mm" space-before="12mm">Title 1</fo:block>
               |<fo:block $defaultParagraphStyles>Line 1</fo:block>
               |<fo:block $defaultParagraphStyles>Line 2</fo:block>
               |<fo:block font-family="sans-serif" font-size="14pt" font-weight="bold" keep-with-next="always" space-after="3mm" space-before="7mm">Title 2</fo:block>
               |<fo:block $defaultParagraphStyles>Line 1</fo:block>
               |<fo:block $defaultParagraphStyles>Line 2</fo:block>""".stripMargin
    render (rootElem) should be (fo)
  }

  it should "render a title containing emphasized text" in {
    val elem = Title(Text("some "), Emphasized("em"), Text(" text"))
    render (elem) should be ("""<fo:block color="#007c99" font-family="sans-serif" font-size="24pt" font-weight="bold" keep-with-next="always" space-after="6mm" space-before="0mm">some <fo:inline font-style="italic">em</fo:inline> text</fo:block>""")
  }

  it should "render a title containing a section number" in {
    val elem = Title(SectionNumber(Seq(1,2,3)), Text("Title"))
    render (elem) should be ("""<fo:block color="#007c99" font-family="sans-serif" font-size="24pt" font-weight="bold" keep-with-next="always" space-after="6mm" space-before="0mm">1.2.3 Title</fo:block>""")
  }

  it should "render a paragraph containing emphasized text" in {
    val elem = p(Text("some "), Emphasized("em"), Text(" text"))
    render (elem) should be (s"""<fo:block $defaultParagraphStyles>some <fo:inline font-style="italic">em</fo:inline> text</fo:block>""")
  }

  it should "render a paragraph containing strong text" in {
    val elem = p(Text("some "), Strong("strong"), Text(" text"))
    render (elem) should be (s"""<fo:block $defaultParagraphStyles>some <fo:inline font-weight="bold">strong</fo:inline> text</fo:block>""")
  }

  it should "render a paragraph containing a deleted span" in {
    val elem = p(Text("some "), Deleted(Seq(Text("deleted"))), Text(" text"))
    render (elem) should be (s"""<fo:block $defaultParagraphStyles>some <fo:inline text-decoration="line-through">deleted</fo:inline> text</fo:block>""")
  }

  it should "render a paragraph containing an inserted span" in {
    val elem = p(Text("some "), Inserted("inserted"), Text(" text"))
    render (elem) should be (s"""<fo:block $defaultParagraphStyles>some <fo:inline text-decoration="underline">inserted</fo:inline> text</fo:block>""")
  }

  it should "render a paragraph containing a literal span" in {
    val elem = p(Text("some "), Literal("code"), Text(" span"))
    val fo = s"""<fo:block $defaultParagraphStyles>some <fo:inline font-family="monospaced" font-size="9pt">code</fo:inline> span</fo:block>"""
    render (elem) should be (fo)
  }

  it should "render a paragraph containing a code span" in {
    val elem = p(Text("some "), InlineCode("banana-script", List(Text("code"))), Text(" span"))
    val fo = s"""<fo:block $defaultParagraphStyles>some <fo:inline font-family="monospaced" font-size="9pt">code</fo:inline> span</fo:block>"""
    render (elem) should be (fo)
  }

  it should "render a paragraph containing a code span with syntax highlighting" in {
    val elem = p(Text("some "), CodeSpan("code", CodeCategory.Keyword), Text(" span"))
    val fo = s"""<fo:block $defaultParagraphStyles>some <fo:inline font-weight="bold">code</fo:inline> span</fo:block>"""
    val style = StyleDeclaration(StyleSelector(Set(StylePredicate.StyleName("keyword"))), Map("font-weight" -> "bold"))
    render (elem, style) should be (fo)
  }

  it should "render a paragraph containing a link without title" in {
    val elem = p(Text("some "), link(Text("link")).url("/foo"), Text(" span"))
    val fo = s"""<fo:block $defaultParagraphStyles>some <fo:basic-link color="#931813" external-destination="/foo" font-weight="bold">link</fo:basic-link> span</fo:block>"""
    render (elem) should be (fo)
  }

  it should "render a paragraph containing a link with title" in {
    val elem = p(Text("some "), link(Text("link")).url("/foo").title("title"), Text(" span"))
    val fo = s"""<fo:block $defaultParagraphStyles>some <fo:basic-link color="#931813" external-destination="/foo" font-weight="bold">link</fo:basic-link> span</fo:block>"""
    render (elem) should be (fo)
  }

  it should "render a paragraph containing a link with emphasized text" in {
    val elem = p(Text("some "), link(Text("link"),Emphasized("text")).url("/foo"), Text(" span"))
    val fo = s"""<fo:block $defaultParagraphStyles>some """ +
      """<fo:basic-link color="#931813" external-destination="/foo" font-weight="bold">link<fo:inline font-style="italic">text</fo:inline></fo:basic-link> span</fo:block>"""
    render (elem) should be (fo)
  }

  it should "render a paragraph containing an internal link with emphasized text" in {
    val elem = p(Text("some "), SpanLink(List(Text("link"),Emphasized("text")), InternalTarget(Path.parse("/#foo"),RelativePath.parse("#foo"))), Text(" span"))
    val fo = s"""<fo:block $defaultParagraphStyles>some """ +
      """<fo:basic-link color="#931813" font-weight="bold" internal-destination="__foo">link<fo:inline font-style="italic">text</fo:inline></fo:basic-link> span</fo:block>"""
    render (elem) should be (fo)
  }

  it should "render a paragraph containing a cross link with a fragment part" in {
    val elem = p(Text("some "), SpanLink(List(Text("link"),Emphasized("text")), InternalTarget(Path.parse("/bar#foo"),RelativePath.parse("../bar#foo"))), Text(" span"))
    val fo = s"""<fo:block $defaultParagraphStyles>some """ +
      """<fo:basic-link color="#931813" font-weight="bold" internal-destination="_bar_foo">link<fo:inline font-style="italic">text</fo:inline></fo:basic-link> span</fo:block>"""
    render (elem) should be (fo)
  }

  it should "render a paragraph containing a cross link without a fragment part" in {
    val elem = p(Text("some "), SpanLink(List(Text("link"),Emphasized("text")), InternalTarget(Path.parse("/bar"),RelativePath.parse("../bar"))), Text(" span"))
    val fo = s"""<fo:block $defaultParagraphStyles>some """ +
      """<fo:basic-link color="#931813" font-weight="bold" internal-destination="_bar">link<fo:inline font-style="italic">text</fo:inline></fo:basic-link> span</fo:block>"""
    render (elem) should be (fo)
  }

  it should "render a paragraph containing a cross link with a filename without suffix" in {
    val elem = p(Text("some "), SpanLink(List(Text("link"),Emphasized("text")), InternalTarget(Path.parse("/bar"),RelativePath.parse("../bar"))), Text(" span"))
    val fo = s"""<fo:block $defaultParagraphStyles>some """ +
      """<fo:basic-link color="#931813" font-weight="bold" internal-destination="_bar">link<fo:inline font-style="italic">text</fo:inline></fo:basic-link> span</fo:block>"""
    render (elem) should be (fo)
  }

  it should "render a paragraph containing only an image centered" in {
    val elem = p(Image("img", imageTarget))
    val fo = s"""<fo:block space-after="3mm" text-align="center"><fo:external-graphic content-width="scale-down-to-fit" scaling="uniform" src="/foo.jpg" width="85%"/></fo:block>"""
    render (elem) should be (fo)
  }
  
  it should "render a paragraph containing an image without title" in {
    val elem = p(Text("some "), Image("img", imageTarget), Text(" span"))
    val fo = s"""<fo:block $defaultParagraphStyles>some <fo:external-graphic content-width="scale-down-to-fit" scaling="uniform" src="/foo.jpg" width="85%"/> span</fo:block>"""
    render (elem) should be (fo)
  }

  it should "render a paragraph containing an image with title" in {
    val elem = p(Text("some "), Image("img", imageTarget, title = Some("title")), Text(" span"))
    val fo = s"""<fo:block $defaultParagraphStyles>some <fo:external-graphic content-width="scale-down-to-fit" scaling="uniform" src="/foo.jpg" width="85%"/> span</fo:block>"""
    render (elem) should be (fo)
  }

  it should "render a paragraph containing an image with width and height attributes" in {
    val elem = p(Text("some "), Image("img", imageTarget, width = Some(LengthUnit.px(120)), height = Some(LengthUnit.px(80))), Text(" span"))
    val fo = s"""<fo:block $defaultParagraphStyles>some <fo:external-graphic content-width="scale-down-to-fit" height="80px" scaling="uniform" src="/foo.jpg" width="120px"/> span</fo:block>"""
    render (elem) should be (fo)
  }

  it should "render a paragraph containing an image with vertical align style" in {
    val elem = p(Text("some "), Image("img", imageTarget).copy(options = Styles("align-top")), Text(" span"))
    val fo = s"""<fo:block $defaultParagraphStyles>some <fo:external-graphic content-width="scale-down-to-fit" scaling="uniform" src="/foo.jpg" vertical-align="top" width="85%"/> span</fo:block>"""
    render (elem) should be (fo)
  }

  it should "render a paragraph containing an unresolved link reference" in {
    val elem = p(Text("some "), linkRef(Text("link")).id("id").source("[link] [id]"), Text(" span"))
    render (elem) should be (s"""<fo:block $defaultParagraphStyles>some [link] [id] span</fo:block>""")
  }

  it should "render a paragraph containing an unresolved image reference" in {
    val elem = p(Text("some "), imgRef("img","id","![img] [id]"), Text(" span"))
    render (elem) should be (s"""<fo:block $defaultParagraphStyles>some ![img] [id] span</fo:block>""")
  }

  it should "render a paragraph containing an internal link target" in {
    val elem = p(Text("some "), InternalLinkTarget(Id("target")), Text(" span"))
    val fo = s"""<fo:block $defaultParagraphStyles>some <fo:inline id="__target"></fo:inline> span</fo:block>"""
    render (elem) should be (fo)
  }

  it should "render a template root containing string elements" in {
    val elem = TemplateRoot(t("aa"),t("bb"),t("cc"))
    render (elem) should be ("aabbcc")
  }

  it should "render a template span sequence containing string elements" in {
    val elem = TemplateSpanSequence(t("aa"),t("bb"),t("cc"))
    render (elem) should be ("aabbcc")
  }

  it should "render a template string without creating XML entities" in {
    val elem = TemplateRoot(t("aa & bb"))
    render (elem) should be ("aa & bb")
  }

  it should "render a template root containing a TemplateElement" in {
    val elem = TemplateRoot(t("aa"),TemplateElement(BlockSequence(List(p("aaa"), p("bbb")),Styles("foo"))),t("cc"))
    val fo = s"""aa<fo:block>
               |  <fo:block $defaultParagraphStyles>aaa</fo:block>
               |  <fo:block $defaultParagraphStyles>bbb</fo:block>
               |</fo:block>cc""".stripMargin
    render (elem) should be (fo)
  }

  private val warningProps = """background-color="#fcfacd" border="1pt solid #b1a400" color="#b1a400" padding="1pt 2pt""""
  
  it should "render a runtime message" in {
    val fo = s"""<fo:inline $warningProps>some message</fo:inline>"""
    render (RuntimeMessage(MessageLevel.Warning, "some message"), MessageFilter.Warning) should be (fo)
  }

  it should "render a comment" in {
    render (Comment("foo")) should be ("<!-- foo -->")
  }

  it should "render an invalid block without the runtime message in default mode" in {
    val elem = InvalidBlock(RuntimeMessage(MessageLevel.Warning, "some message"), p("fallback"))
    val fo = s"""<fo:block $defaultParagraphStyles>fallback</fo:block>"""
    render (elem) should be (fo)
  }

  it should "render an invalid block without the runtime message if the configured message level is higher" in {
    val elem = InvalidBlock(RuntimeMessage(MessageLevel.Warning, "some message"), p("fallback"))
    val fo = s"""<fo:block $defaultParagraphStyles>fallback</fo:block>"""
    render (elem, MessageFilter.Error) should be (fo)
  }

  it should "render an invalid block with the runtime message if the configured message level is lower or equal" in {
    val elem = InvalidBlock(RuntimeMessage(MessageLevel.Warning, "some message"), p("fallback"))
    val fo = s"""<fo:block $defaultParagraphStyles>""" +
      s"""<fo:inline $warningProps>some message</fo:inline>""" +
      s"""</fo:block><fo:block $defaultParagraphStyles>fallback</fo:block>"""
    render (elem, MessageFilter.Info) should be (fo)
  }

  it should "render an invalid span without the runtime message in default mode" in {
    val elem = InvalidSpan(RuntimeMessage(MessageLevel.Warning, "some message"), Text("fallback"))
    render (elem) should be ("fallback")
  }

  it should "render an invalid span without the runtime message if the configured message level is higher" in {
    val elem = InvalidSpan(RuntimeMessage(MessageLevel.Warning, "some message"), Text("fallback"))
    render (elem, MessageFilter.Error) should be ("fallback")
  }

  it should "render an invalid span with the runtime message if the configured message level is lower or equal" in {
    val elem = InvalidSpan(RuntimeMessage(MessageLevel.Warning, "some message"), Text("fallback"))
    val fo = s"""<fo:inline $warningProps>some message</fo:inline> fallback"""
    render (elem, MessageFilter.Info) should be (fo)
  }

  val defaultCodeBlockStyles = """background-color="#F6F1EF" color="#362E21" font-family="monospaced" font-size="9pt" fox:border-radius="2mm" line-height="1.4" linefeed-treatment="preserve" margin-left="2mm" margin-right="2mm" padding="2mm" space-after="6mm" white-space-collapse="false" white-space-treatment="preserve""""
  val monoBlock = s"""<fo:block $defaultCodeBlockStyles>"""

  it should "render a literal block" in {
    val code = """line 1
                 |
                 |    <line 2
                 |
                 |line 3""".stripMargin
    val elem = LiteralBlock(code)
    render (elem) should be (monoBlock + code.replace("<", "&lt;") + "</fo:block>")
  }

  it should "render a parsed literal block" in {
    val code = """line 1
                 |
                 |    #<line 2
                 |
                 |line 3""".stripMargin.split("#")
    val elem = ParsedLiteralBlock(List(Text(code(0)), Emphasized("em"), Text(code(1))))
    val fo = monoBlock + code(0) + """<fo:inline font-style="italic">em</fo:inline>""" + code(1).replace("<", "&lt;") + "</fo:block>"
    render (elem) should be (fo)
  }

  it should "render a code block" in {
    val code = """line 1
                 |
                 |    <line 2
                 |
                 |line 3""".stripMargin
    val elem = CodeBlock("banana-script", List(Text(code)))
    render (elem) should be (monoBlock + code.replace("<", "&lt;") + "</fo:block>")
  }

  it should "render a literal block inside a blockquote without indentation" in {
    val code = """line 1
                 |
                 |    line 2
                 |
                 |line 3""".stripMargin
    val fo = s"""<fo:block font-style="italic" margin-left="8mm" margin-right="8mm" space-after="3mm">
               |  %s</fo:block>
               |</fo:block>""".stripMargin.format(monoBlock+code)
    val elem = QuotedBlock(LiteralBlock(code))
    render (elem) should be (fo)
  }

  it should "render a parsed literal block inside a blockquote without indentation" in {
    val code = """line 1
                 |
                 |    line 2
                 |
                 |line 3""".stripMargin
    val fo = s"""<fo:block font-style="italic" margin-left="8mm" margin-right="8mm" space-after="3mm">
               |  %s:<fo:inline font-style="italic">%s</fo:inline>:</fo:block>
               |</fo:block>""".stripMargin.format(monoBlock, code)
    val elem = QuotedBlock(ParsedLiteralBlock(List(Text(":"),Emphasized(code),Text(":"))))
    render (elem) should be (fo)
  }

  it should "render a code block inside a blockquote without indentation" in {
    val code = """line 1
                 |
                 |    line 2
                 |
                 |line 3""".stripMargin
    val fo = s"""<fo:block font-style="italic" margin-left="8mm" margin-right="8mm" space-after="3mm">
               |  %s:<fo:inline font-style="italic">%s</fo:inline>:</fo:block>
               |</fo:block>""".stripMargin.format(monoBlock, code)
    val elem = QuotedBlock(CodeBlock("banana-script", List(Text(":"),Emphasized(code),Text(":"))))
    render (elem) should be (fo)
  }

  it should "render a table cell unformatted" in {
    val elem = BodyCell(p("a"),p("b"))
    val fo = s"""<fo:table-cell padding="2mm">
               |<fo:block $defaultParagraphStyles>a</fo:block>
               |<fo:block $defaultParagraphStyles>b</fo:block>
               |</fo:table-cell>""".stripMargin
    renderUnformatted(elem) should be (fo)
  }

  it should "render raw content unchanged if the xsl-fo format is specified" in {
    val raw = "<fo:block>some text</fo:block>"
    val elem = RawContent(NonEmptySet.of("fo", "spooky"), raw)
    render (elem) should be (raw)
  }

  it should "ignore raw content if the xsl-fo format is not specified" in {
    val raw = "<fo:block>some text</fo:block>"
    val elem = RawContent(NonEmptySet.of("dodgy", "spooky"), raw)
    render (elem) should be ("")
  }

  it should "render an embedded root with correct indentation" in {
    val elem = root(TemplateRoot(
      t("<fo:block>\n  "),
      EmbeddedRoot(List(p("aaa"),p("bbb")), 2),
      t("\n</fo:block>")
    ))
    val fo = s"""<fo:block>
               |  <fo:block $defaultParagraphStyles>aaa</fo:block>
               |  <fo:block $defaultParagraphStyles>bbb</fo:block>
               |</fo:block>""".stripMargin
    render (elem) should be (fo)
  }

  it should "render an embedded root without indentation" in {
    val elem = root(TemplateRoot(
      t("<fo:block>\n"),
      EmbeddedRoot(p("aaa"),p("bbb")),
      t("\n</fo:block>")
    ))
    val fo = s"""<fo:block>
               |<fo:block $defaultParagraphStyles>aaa</fo:block>
               |<fo:block $defaultParagraphStyles>bbb</fo:block>
               |</fo:block>""".stripMargin
    render (elem) should be (fo)
  }


}
