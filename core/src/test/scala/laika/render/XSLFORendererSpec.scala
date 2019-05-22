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

package laika.render

import laika.api.Render
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.bundle.BundleProvider
import laika.format.XSLFO
import org.scalatest.{FlatSpec, Matchers}

class XSLFORendererSpec extends FlatSpec
                        with Matchers
                        with ModelBuilder {
 
  
  def render (elem: Element): String = Render.as(XSLFO).build.render(elem)

  def render (elem: Element, messageLevel: MessageLevel): String =
    Render.as(XSLFO).withMessageLevel(messageLevel).build.render(elem)
    
  def render (elem: Element, style: StyleDeclaration): String = 
    Render.as(XSLFO).using(BundleProvider
      .forTheme(XSLFO.Theme(defaultStyles = StyleDeclarationSet(Path.Root, style)))).build.render(elem)
    
  def renderUnformatted (elem: Element): String = (Render as XSLFO).unformatted.build.render(elem)
  
  
  "The XSLFO renderer" should "render a paragraph with plain text" in {
    val elem = p("some text")
    render (elem) should be ("""<fo:block font-family="serif" font-size="10pt" space-after="3mm">some text</fo:block>""") 
  }
  
  it should "render a document with two paragraphs with plain text" in {
    val elem = root( p("aaa"), p("bbb"))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
      |<fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a block sequence with a custom style" in {
    val elem = root(BlockSequence(List(p("aaa"), p("bbb")), Styles("foo")))
    val html = """<fo:block font-weight="bold">
      |  <fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
      |  <fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>
      |</fo:block>""".stripMargin
    val style = StyleDeclaration(StyleSelector(Set(StylePredicate.StyleName("foo"))), Map("font-weight" -> "bold"))
    render (elem, style) should be (html) 
  }
  
  it should "render a block sequence without a style" in {
    val elem = root(p("aaa"), BlockSequence(List(p("bbb"), p("ccc"))))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
      |<fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>
      |<fo:block font-family="serif" font-size="10pt" space-after="3mm">ccc</fo:block>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a block sequence with a single element" in {
    val elem = root(p("aaa"), BlockSequence(List(p("bbb"))), p("ccc"))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
      |<fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>
      |<fo:block font-family="serif" font-size="10pt" space-after="3mm">ccc</fo:block>""".stripMargin
    render (elem) should be (html) 
  }

  it should "render a blockquote with two paragraphs with plain text" in {
    val elem = quote( p("aaa"), p("bbb"))
    val html = """<fo:block font-style="italic" margin-left="8mm" margin-right="8mm" space-after="3mm">
      |  <fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
      |  <fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>
      |</fo:block>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a blockquote with one paragraph with plain text" in {
    val elem = quote(p("aaa"))
    val html = """<fo:block font-style="italic" margin-left="8mm" margin-right="8mm" space-after="3mm">
      |  <fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
      |</fo:block>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a blockquote with an attribution" in {
    val elem = quote("aaa","bbb")
    val html = """<fo:block font-style="italic" margin-left="8mm" margin-right="8mm" space-after="3mm">
      |  <fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
      |  <fo:block font-family="serif" font-size="10pt" space-after="3mm" text-align="right">bbb</fo:block>
      |</fo:block>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a bullet list with simple flow content" in {
    val elem = bulletList() + "aaa" + "bbb" toList
    val html = """<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">&#x2022;</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">&#x2022;</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |</fo:list-block>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a bullet list with a nested list" in {
    val elem = bulletList() + (SpanSequence(Seq(Text("aaa"))), bulletList() + "bbb") toList
    val html = """<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">&#x2022;</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
      |      <fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
      |        <fo:list-item space-after="3mm">
      |          <fo:list-item-label end-indent="label-end()">
      |            <fo:block font-family="serif" font-size="10pt" space-after="3mm">&#x2022;</fo:block>
      |          </fo:list-item-label>
      |          <fo:list-item-body start-indent="body-start()">
      |            <fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>
      |          </fo:list-item-body>
      |        </fo:list-item>
      |      </fo:list-block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |</fo:list-block>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with simple flow content" in {
    val elem = enumList() + "aaa" + "bbb"
    val html = """<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">1.</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">2.</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |</fo:list-block>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with lower roman enumeration style" in {
    val elem = enumList(EnumFormat(EnumType.LowerRoman)) + "aaa" + "bbb"
    val html = """<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">i.</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">ii.</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |</fo:list-block>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with upper roman enumeration style" in {
    val elem = enumList(EnumFormat(EnumType.UpperRoman)) + "aaa" + "bbb"
    val html = """<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">I.</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">II.</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |</fo:list-block>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with lower alpha enumeration style" in {
    val elem = enumList(EnumFormat(EnumType.LowerAlpha)) + "aaa" + "bbb"
    val html = """<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">a.</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">b.</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |</fo:list-block>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with upper alpha enumeration style" in {
    val elem = enumList(EnumFormat(EnumType.UpperAlpha)) + "aaa" + "bbb"
    val html = """<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">A.</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">B.</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |</fo:list-block>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with the start value other than 1" in {
    val elem = enumList(EnumFormat(EnumType.Arabic), 7) + "aaa" + "bbb"
    val html = """<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">7.</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">8.</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |</fo:list-block>""".stripMargin
    render (elem) should be (html) 
  }
  
  private def fp (content: String) = ForcedParagraph(List(Text(content)))
  
  it should "render a bullet list with forced paragraphs as list items the same way as normal paragraphs" in {
    val elem = bulletList() + fp("aaa") + fp("bbb") toList
    val html = """<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">&#x2022;</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">&#x2022;</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |</fo:list-block>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an enumerated list with forced paragraphs as list items the same way as normal paragraphs" in {
    val elem = enumList() + fp("aaa") + fp("bbb")
    val html = """<fo:list-block provisional-distance-between-starts="5mm" space-after="6mm">
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">1.</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">2.</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |</fo:list-block>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a definition list with paragraphs" in {
    val elem = defList + ("term 1", p("1"), p("1")) + ("term 2", p("2"), p("2"))
    val html = """<fo:list-block provisional-distance-between-starts="20mm" space-after="6mm">
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">term 1</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">1</fo:block>
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">1</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">term 2</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">2</fo:block>
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">2</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |</fo:list-block>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a definition list with simple flow content" in {
    val elem = defList + ("term 1", p("1")) + ("term 2", p("2"))
    val html = """<fo:list-block provisional-distance-between-starts="20mm" space-after="6mm">
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">term 1</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">1</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |  <fo:list-item space-after="3mm">
      |    <fo:list-item-label end-indent="label-end()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">term 2</fo:block>
      |    </fo:list-item-label>
      |    <fo:list-item-body start-indent="body-start()">
      |      <fo:block font-family="serif" font-size="10pt" space-after="3mm">2</fo:block>
      |    </fo:list-item-body>
      |  </fo:list-item>
      |</fo:list-block>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a paragraph containing a citation link" in {
    val elem = BlockSequence(List(p(txt("some "), CitationLink("ref","label"), txt(" span")), Citation("ref", List(p("a"),p("b")), Id("ref"))))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">some <fo:footnote>
      |  <fo:inline font-size="8pt" vertical-align="super">[label]</fo:inline>
      |  <fo:footnote-body id="_ref">
      |    <fo:block font-family="serif" font-size="10pt" space-after="3mm"><fo:inline font-size="8pt" vertical-align="super">[label]</fo:inline> a</fo:block>
      |    <fo:block font-family="serif" font-size="10pt" space-after="3mm">b</fo:block>
      |  </fo:footnote-body>
      |</fo:footnote> span</fo:block>
      |""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a paragraph containing a footnote link" in {
    val elem = BlockSequence(List(p(txt("some "), FootnoteLink("id","label"), txt(" span")), Footnote("label", List(p("a"),p("b")), Id("id"))))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">some <fo:footnote>
      |  <fo:inline font-size="8pt" vertical-align="super">[label]</fo:inline>
      |  <fo:footnote-body id="_id">
      |    <fo:block font-family="serif" font-size="10pt" space-after="3mm"><fo:inline font-size="8pt" vertical-align="super">[label]</fo:inline> a</fo:block>
      |    <fo:block font-family="serif" font-size="10pt" space-after="3mm">b</fo:block>
      |  </fo:footnote-body>
      |</fo:footnote> span</fo:block>
      |""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render a table without header cells" in {
    val elem = table(row(cell("a"),cell("b")),row(cell("c"),cell("d")))
    val html = """<fo:table space-after="6mm">
      |  <fo:table-body>
      |    <fo:table-row>
      |      <fo:table-cell padding-top="2mm">
      |        <fo:block font-family="serif" font-size="10pt" space-after="3mm">a</fo:block>
      |      </fo:table-cell>
      |      <fo:table-cell padding-top="2mm">
      |        <fo:block font-family="serif" font-size="10pt" space-after="3mm">b</fo:block>
      |      </fo:table-cell>
      |    </fo:table-row>
      |    <fo:table-row>
      |      <fo:table-cell padding-top="2mm">
      |        <fo:block font-family="serif" font-size="10pt" space-after="3mm">c</fo:block>
      |      </fo:table-cell>
      |      <fo:table-cell padding-top="2mm">
      |        <fo:block font-family="serif" font-size="10pt" space-after="3mm">d</fo:block>
      |      </fo:table-cell>
      |    </fo:table-row>
      |  </fo:table-body>
      |</fo:table>""".stripMargin
    render(elem) should be (html)
  }
  
  it should "render a table with header cells" in {
    val elem = Table(TableHead(List(row(cell("a"), cell("b")))),
                     TableBody(List(row(cell("c"), cell("d")))))
    val html = """<fo:table space-after="6mm">
      |  <fo:table-header border-bottom-style="solid" border-bottom-width="1pt">
      |    <fo:table-row>
      |      <fo:table-cell padding-top="2mm">
      |        <fo:block font-family="serif" font-size="10pt" space-after="3mm">a</fo:block>
      |      </fo:table-cell>
      |      <fo:table-cell padding-top="2mm">
      |        <fo:block font-family="serif" font-size="10pt" space-after="3mm">b</fo:block>
      |      </fo:table-cell>
      |    </fo:table-row>
      |  </fo:table-header>
      |  <fo:table-body>
      |    <fo:table-row>
      |      <fo:table-cell padding-top="2mm">
      |        <fo:block font-family="serif" font-size="10pt" space-after="3mm">c</fo:block>
      |      </fo:table-cell>
      |      <fo:table-cell padding-top="2mm">
      |        <fo:block font-family="serif" font-size="10pt" space-after="3mm">d</fo:block>
      |      </fo:table-cell>
      |    </fo:table-row>
      |  </fo:table-body>
      |</fo:table>""".stripMargin
    render(elem) should be (html)
  }
  
  it should "render a table with a caption" in {
    val caption = Caption(List(Text("caption")))
    val elem = table(row(cell("a"),cell("b")),row(cell("c"),cell("d"))).copy(caption = caption)
    val html = """<fo:block background-color="#cccccc" padding-left="2cm" padding-right="2cm" space-after="6mm">
      |  <fo:block font-family="sans-serif" font-size="12pt" font-weight="bold" space-after="3mm">caption</fo:block>
      |  <fo:table space-after="6mm">
      |    <fo:table-body>
      |      <fo:table-row>
      |        <fo:table-cell padding-top="2mm">
      |          <fo:block font-family="serif" font-size="10pt" space-after="3mm">a</fo:block>
      |        </fo:table-cell>
      |        <fo:table-cell padding-top="2mm">
      |          <fo:block font-family="serif" font-size="10pt" space-after="3mm">b</fo:block>
      |        </fo:table-cell>
      |      </fo:table-row>
      |      <fo:table-row>
      |        <fo:table-cell padding-top="2mm">
      |          <fo:block font-family="serif" font-size="10pt" space-after="3mm">c</fo:block>
      |        </fo:table-cell>
      |        <fo:table-cell padding-top="2mm">
      |          <fo:block font-family="serif" font-size="10pt" space-after="3mm">d</fo:block>
      |        </fo:table-cell>
      |      </fo:table-row>
      |    </fo:table-body>
      |  </fo:table>
      |</fo:block>""".stripMargin
    render(elem) should be (html)
  }
  
  it should "render a cell using colspan and rowspan attributes" in {
    val elem = cell("a",3,2)
    val html = """<fo:table-cell number-columns-spanned="3" number-rows-spanned="2" padding-top="2mm">
      |  <fo:block font-family="serif" font-size="10pt" space-after="3mm">a</fo:block>
      |</fo:table-cell>""".stripMargin
    render(elem) should be (html)
  } 
  
  it should "render a cell with two paragraphs" in {
    val elem = cell(p("a"),p("b"))
    val html = """<fo:table-cell padding-top="2mm">
      |  <fo:block font-family="serif" font-size="10pt" space-after="3mm">a</fo:block>
      |  <fo:block font-family="serif" font-size="10pt" space-after="3mm">b</fo:block>
      |</fo:table-cell>""".stripMargin
    render(elem) should be (html)
  } 
  
  it should "render a titled block" in {
    val elem = TitledBlock(List(txt("some "), em("em"), txt(" text")), List(p("aaa"), Rule(), p("bbb")))
    val html = """<fo:block background-color="#cccccc" padding-left="2cm" padding-right="2cm" space-after="6mm">
      |  <fo:block font-family="sans-serif" font-size="12pt" font-weight="bold" space-after="3mm">some <fo:inline font-style="italic">em</fo:inline> text</fo:block>
      |  <fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
      |  <fo:leader leader-pattern="rule"></fo:leader>
      |  <fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>
      |</fo:block>""".stripMargin
    render(elem) should be (html)
  }
  
  it should "render a figure" in {
    val elem = Figure(Image("alt",URI("image.jpg")), List(txt("some "), em("caption"), txt(" text")), List(p("aaa"), Rule(), p("bbb")))
    val html = """<fo:block space-after="6mm">
      |  <fo:block font-family="serif" font-size="10pt" space-after="3mm"><fo:external-graphic content-width="scale-down-to-fit" height="100%" scaling="uniform" src="image.jpg" width="100%"/></fo:block>
      |  <fo:block font-family="serif" font-size="9pt" font-style="italic" space-after="3mm">some <fo:inline font-style="italic">caption</fo:inline> text</fo:block>
      |  <fo:block font-size="9pt" font-style="italic">
      |    <fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
      |    <fo:leader leader-pattern="rule"></fo:leader>
      |    <fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>
      |  </fo:block>
      |</fo:block>""".stripMargin
    render(elem) should be (html)
  }
  
  it should "render nested line blocks" in {
    val elem = lb(lb(line("1"),line("2")), line("3"))
    val html = """<fo:block margin-left="2cm">
      |  <fo:block margin-left="2cm">
      |    <fo:block>1</fo:block>
      |    <fo:block>2</fo:block>
      |  </fo:block>
      |  <fo:block>3</fo:block>
      |</fo:block>""".stripMargin
    render(elem) should be (html)
  }
  
  it should "render a document with two paragraphs separated by a horizontal rule" in {
    val elem = root( p("aaa"), Rule(), p("bbb"))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
      |<fo:leader leader-pattern="rule"></fo:leader>
      |<fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>""".stripMargin
    render (elem) should be (html) 
  } 
  
  it should "render a document with two nested sections" in {
    val nested = Section(h(2, txt("Title 2")), List(p("Line 1"), p("Line 2")))
    val rootElem = root(Section(h(1, txt("Title 1")), List(p("Line 1"), p("Line 2"))), nested)
    val html = """<fo:block font-family="sans-serif" font-size="16pt" font-weight="bold" keep-with-next="always" space-after="7mm" space-before="12mm">Title 1</fo:block>
      |<fo:block font-family="serif" font-size="10pt" space-after="3mm">Line 1</fo:block>
      |<fo:block font-family="serif" font-size="10pt" space-after="3mm">Line 2</fo:block>
      |<fo:block font-family="sans-serif" font-size="14pt" font-weight="bold" keep-with-next="always" space-after="5mm" space-before="9mm">Title 2</fo:block>
      |<fo:block font-family="serif" font-size="10pt" space-after="3mm">Line 1</fo:block>
      |<fo:block font-family="serif" font-size="10pt" space-after="3mm">Line 2</fo:block>""".stripMargin
    render (rootElem) should be (html) 
  }
  
  it should "render a title containing emphasized text" in {
    val elem = Title(Seq(txt("some "), em("em"), txt(" text")))
    render (elem) should be ("""<fo:marker marker-class-name="chapter"><fo:block>some em text</fo:block></fo:marker>
      |<fo:block font-family="sans-serif" font-size="16pt" keep-with-next="always" space-after="7mm" space-before="12mm">some <fo:inline font-style="italic">em</fo:inline> text</fo:block>""".stripMargin) 
  }
  
  it should "render a title containing a section number" in {
    val elem = Title(Seq(SectionNumber(Seq(1,2,3)), txt("Title")))
    render (elem) should be ("""<fo:marker marker-class-name="chapter"><fo:block>1.2.3 Title</fo:block></fo:marker>
      |<fo:block font-family="sans-serif" font-size="16pt" keep-with-next="always" space-after="7mm" space-before="12mm">1.2.3 Title</fo:block>""".stripMargin) 
  }
  
  it should "render a paragraph containing emphasized text" in {
    val elem = p(txt("some "), em("em"), txt(" text"))
    render (elem) should be ("""<fo:block font-family="serif" font-size="10pt" space-after="3mm">some <fo:inline font-style="italic">em</fo:inline> text</fo:block>""") 
  }
  
  it should "render a paragraph containing strong text" in {
    val elem = p(txt("some "), str("strong"), txt(" text")) 
    render (elem) should be ("""<fo:block font-family="serif" font-size="10pt" space-after="3mm">some <fo:inline font-weight="bold">strong</fo:inline> text</fo:block>""") 
  }

  it should "render a paragraph containing a deleted span" in {
    val elem = p(txt("some "), Deleted(Seq(Text("deleted"))), txt(" text"))
    render (elem) should be ("""<fo:block font-family="serif" font-size="10pt" space-after="3mm">some <fo:inline text-decoration="line-through">deleted</fo:inline> text</fo:block>""")
  }

  it should "render a paragraph containing an inserted span" in {
    val elem = p(txt("some "), Inserted(Seq(Text("inserted"))), txt(" text"))
    render (elem) should be ("""<fo:block font-family="serif" font-size="10pt" space-after="3mm">some <fo:inline text-decoration="underline">inserted</fo:inline> text</fo:block>""")
  }
  
  it should "render a paragraph containing a literal span" in {
    val elem = p(txt("some "), lit("code"), txt(" span"))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">some <fo:inline font-family="monospace">code</fo:inline> span</fo:block>"""
    render (elem) should be (html) 
  }
  
  it should "render a paragraph containing a code span" in {
    val elem = p(txt("some "), Code("banana-script", List(Text("code"))), txt(" span"))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">some <fo:inline font-family="monospace">code</fo:inline> span</fo:block>"""
    render (elem) should be (html) 
  }
  
  it should "render a paragraph containing a link without title" in {
    val elem = p(txt("some "), link(txt("link")).url("/foo"), txt(" span"))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">some <fo:basic-link color="#3956ac" external-destination="/foo">link</fo:basic-link> span</fo:block>"""
    render (elem) should be (html) 
  }
  
  it should "render a paragraph containing a link with title" in {
    val elem = p(txt("some "), link(txt("link")).url("/foo").title("title"), txt(" span"))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">some <fo:basic-link color="#3956ac" external-destination="/foo">link</fo:basic-link> span</fo:block>"""
    render (elem) should be (html) 
  }
  
  it should "render a paragraph containing a link with emphasized text" in {
    val elem = p(txt("some "), link(txt("link"),em("text")).url("/foo"), txt(" span"))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">some """ + 
        """<fo:basic-link color="#3956ac" external-destination="/foo">link<fo:inline font-style="italic">text</fo:inline></fo:basic-link> span</fo:block>"""
    render (elem) should be (html) 
  }
  
  it should "render a paragraph containing an internal link with emphasized text" in {
    val elem = p(txt("some "), InternalLink(List(txt("link"),em("text")),"foo"), txt(" span"))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">some """ +
        """<fo:basic-link color="#3956ac" internal-destination="_foo">link<fo:inline font-style="italic">text</fo:inline></fo:basic-link> span</fo:block>"""
    render (elem) should be (html) 
  }
  
  it should "render a paragraph containing a cross link with a fragment part" in {
    val elem = p(txt("some "), CrossLink(List(txt("link"),em("text")),"foo", PathInfo(Path("/bar"),Path("../bar.md"))), txt(" span"))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">some """ +
        """<fo:basic-link color="#3956ac" internal-destination="_bar_foo">link<fo:inline font-style="italic">text</fo:inline></fo:basic-link> span</fo:block>"""
    render (elem) should be (html) 
  }
  
  it should "render a paragraph containing a cross link without a fragment part" in {
    val elem = p(txt("some "), CrossLink(List(txt("link"),em("text")),"", PathInfo(Path("/bar"),Path("../bar.md"))), txt(" span"))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">some """ +
        """<fo:basic-link color="#3956ac" internal-destination="_bar_">link<fo:inline font-style="italic">text</fo:inline></fo:basic-link> span</fo:block>"""
    render (elem) should be (html) 
  }
  
  it should "render a paragraph containing a cross link with a filename without suffix" in {
    val elem = p(txt("some "), CrossLink(List(txt("link"),em("text")),"", PathInfo(Path("/bar"),Path("../bar"))), txt(" span"))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">some """ +
        """<fo:basic-link color="#3956ac" internal-destination="_bar_">link<fo:inline font-style="italic">text</fo:inline></fo:basic-link> span</fo:block>"""
    render (elem) should be (html) 
  }
  
  it should "render a paragraph containing an image without title" in {
    val elem = p(txt("some "), img("img", "foo.jpg"), txt(" span"))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">some <fo:external-graphic content-width="scale-down-to-fit" height="100%" scaling="uniform" src="foo.jpg" width="100%"/> span</fo:block>"""
    render (elem) should be (html) 
  }
  
  it should "render a paragraph containing an image with title" in {
    val elem = p(txt("some "), img("img", "foo.jpg", title = Some("title")), txt(" span"))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">some <fo:external-graphic content-width="scale-down-to-fit" height="100%" scaling="uniform" src="foo.jpg" width="100%"/> span</fo:block>"""
    render (elem) should be (html) 
  }

  it should "render a paragraph containing an image with width and height attributes" in {
    val elem = p(txt("some "), img("img", "foo.jpg", width = Some(Size(120,"px")), height = Some(Size(80,"px"))), txt(" span"))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">some <fo:external-graphic content-width="scale-down-to-fit" height="80px" scaling="uniform" src="foo.jpg" width="120px"/> span</fo:block>"""
    render (elem) should be (html)
  }

  it should "render a paragraph containing an image with vertical align style" in {
    val elem = p(txt("some "), img("img", "foo.jpg").copy(options = Styles("align-top")), txt(" span"))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">some <fo:external-graphic content-width="scale-down-to-fit" height="100%" scaling="uniform" src="foo.jpg" vertical-align="top" width="100%"/> span</fo:block>"""
    render (elem) should be (html)
  }

  it should "render a paragraph containing an unresolved link reference" in {
    val elem = p(txt("some "), linkRef(txt("link")).id("id").source("[link] [id]"), txt(" span"))
    render (elem) should be ("""<fo:block font-family="serif" font-size="10pt" space-after="3mm">some [link] [id] span</fo:block>""") 
  }
  
  it should "render a paragraph containing an unresolved image reference" in {
    val elem = p(txt("some "), imgRef("img","id","![img] [id]"), txt(" span"))
    render (elem) should be ("""<fo:block font-family="serif" font-size="10pt" space-after="3mm">some ![img] [id] span</fo:block>""") 
  }
  
  it should "render a paragraph containing an internal link target" in {
    val elem = p(txt("some "), InternalLinkTarget(Id("target")), txt(" span"))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">some <fo:inline id="_target"></fo:inline> span</fo:block>"""
    render (elem) should be (html) 
  }
  
  it should "render a template root containing string elements" in {
    val elem = tRoot(tt("aa"),tt("bb"),tt("cc"))
    render (elem) should be ("aabbcc")
  }
  
  it should "render a template span sequence containing string elements" in {
    val elem = TemplateSpanSequence(List(tt("aa"),tt("bb"),tt("cc")))
    render (elem) should be ("aabbcc")
  }
  
  it should "render a template string without creating XML entities" in {
    val elem = tRoot(tt("aa & bb"))
    render (elem) should be ("aa & bb")
  }
  
  it should "render a template root containing a TemplateElement" in {
    val elem = tRoot(tt("aa"),tElem(BlockSequence(List(p("aaa"), p("bbb")),Styles("foo"))),tt("cc"))
    val html = """aa<fo:block>
      |  <fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
      |  <fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>
      |</fo:block>cc""".stripMargin
    render (elem) should be (html)
  }

  import MessageLevel._
  
  it should "render a system message" in {
    val html = """<fo:inline background-color="#ffff33" color="white">some message</fo:inline>"""
    render (SystemMessage(Warning, "some message"), Warning) should be (html)
  }
  
  it should "render a comment" in {
    render (Comment("foo")) should be ("<!-- foo -->")
  }
  
  it should "render an invalid block without the system message in default mode" in {
    val elem = InvalidBlock(SystemMessage(Warning, "some message"), p("fallback"))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">fallback</fo:block>"""
    render (elem) should be (html)
  }
  
  it should "render an invalid block without the system message if the configured message level is higher" in {
    val elem = InvalidBlock(SystemMessage(Warning, "some message"), p("fallback"))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">fallback</fo:block>"""
    render (elem, Error) should be (html)
  }
  
  it should "render an invalid block with the system message if the configured message level is lower or equal" in {
    val elem = InvalidBlock(SystemMessage(Warning, "some message"), p("fallback"))
    val html = """<fo:block font-family="serif" font-size="10pt" space-after="3mm">""" + 
        """<fo:inline background-color="#ffff33" color="white">some message</fo:inline>""" +
        """</fo:block><fo:block font-family="serif" font-size="10pt" space-after="3mm">fallback</fo:block>"""
    render (elem, Info) should be (html)
  }
  
  it should "render an invalid span without the system message in default mode" in {
    val elem = InvalidSpan(SystemMessage(Warning, "some message"), txt("fallback"))
    render (elem) should be ("fallback")
  }
  
  it should "render an invalid span without the system message if the configured message level is higher" in {
    val elem = InvalidSpan(SystemMessage(Warning, "some message"), txt("fallback"))
    render (elem, Error) should be ("fallback")
  }
  
  it should "render an invalid span with the system message if the configured message level is lower or equal" in {
    val elem = InvalidSpan(SystemMessage(Warning, "some message"), txt("fallback"))
    val html = """<fo:inline background-color="#ffff33" color="white">some message</fo:inline> fallback"""
    render (elem, Info) should be (html)
  }
  
  val monoBlock = """<fo:block font-family="monospace" font-size="10pt" linefeed-treatment="preserve" margin-left="6mm" margin-right="6mm" space-after="6mm">"""
  
  it should "render a literal block" in {
    val code = """line 1
      |
      |    <line 2
      |
      |line 3""".stripMargin
    val elem = litBlock(code)
    render (elem) should be (monoBlock + code.replaceAllLiterally("<", "&lt;") + "</fo:block>") 
  }
  
  it should "render a parsed literal block" in {
    val code = """line 1
      |
      |    #<line 2
      |
      |line 3""".stripMargin.split("#")
    val elem = ParsedLiteralBlock(List(txt(code(0)), em("em"), txt(code(1))))
    val html = monoBlock + code(0) + """<fo:inline font-style="italic">em</fo:inline>""" + code(1).replaceAllLiterally("<", "&lt;") + "</fo:block>"
    render (elem) should be (html) 
  }
  
  it should "render a code block" in {
    val code = """line 1
      |
      |    <line 2
      |
      |line 3""".stripMargin
    val elem = CodeBlock("banana-script", List(Text(code)))
    render (elem) should be (monoBlock + code.replaceAllLiterally("<", "&lt;") + "</fo:block>") 
  }
  
  it should "render a literal block inside a blockquote without indentation" in {
    val code = """line 1
      |
      |    line 2
      |
      |line 3""".stripMargin
    val html = """<fo:block font-style="italic" margin-left="8mm" margin-right="8mm" space-after="3mm">
      |  %s</fo:block>
      |</fo:block>""".stripMargin.format(monoBlock+code)
    val elem = quote(litBlock(code))
    render (elem) should be (html) 
  }
  
  it should "render a parsed literal block inside a blockquote without indentation" in {
    val code = """line 1
      |
      |    line 2
      |
      |line 3""".stripMargin
    val html = """<fo:block font-style="italic" margin-left="8mm" margin-right="8mm" space-after="3mm">
      |  %s:<fo:inline font-style="italic">%s</fo:inline>:</fo:block>
      |</fo:block>""".stripMargin.format(monoBlock, code)
    val elem = quote(ParsedLiteralBlock(List(txt(":"),em(code),txt(":"))))
    render (elem) should be (html) 
  }
  
  it should "render a code block inside a blockquote without indentation" in {
    val code = """line 1
      |
      |    line 2
      |
      |line 3""".stripMargin
    val html = """<fo:block font-style="italic" margin-left="8mm" margin-right="8mm" space-after="3mm">
      |  %s:<fo:inline font-style="italic">%s</fo:inline>:</fo:block>
      |</fo:block>""".stripMargin.format(monoBlock, code)
    val elem = quote(CodeBlock("banana-script", List(txt(":"),em(code),txt(":"))))
    render (elem) should be (html) 
  }
  
  it should "render a table cell unformatted" in {
    val elem = cell(p("a"),p("b"))
    val html = """<fo:table-cell padding-top="2mm">
      |<fo:block font-family="serif" font-size="10pt" space-after="3mm">a</fo:block>
      |<fo:block font-family="serif" font-size="10pt" space-after="3mm">b</fo:block>
      |</fo:table-cell>""".stripMargin
    renderUnformatted(elem) should be (html)
  } 
  
  it should "render raw content unchanged if the xsl-fo format is specified" in {
    val raw = "<fo:block>some text</fo:block>"
    val elem = RawContent(List("fo", "spooky"), raw)
    render (elem) should be (raw) 
  }
  
  it should "ignore raw content if the xsl-fo format is not specified" in {
    val raw = "<fo:block>some text</fo:block>"
    val elem = RawContent(List("dodgy", "spooky"), raw)
    render (elem) should be ("") 
  }
  
  it should "render an embedded root with correct indentation" in {
    val elem = root(tRoot(
      tt("<fo:block>\n  "),
      EmbeddedRoot(List(p("aaa"),p("bbb")), 2),
      tt("\n</fo:block>")
    ))
    val html = """<fo:block>
     |  <fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
     |  <fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>
     |</fo:block>""".stripMargin
    render (elem) should be (html) 
  }
  
  it should "render an embedded root without indentation" in {
    val elem = root(tRoot(
      tt("<fo:block>\n"),
      EmbeddedRoot(List(p("aaa"),p("bbb")), 0),
      tt("\n</fo:block>")
    ))
    val html = """<fo:block>
     |<fo:block font-family="serif" font-size="10pt" space-after="3mm">aaa</fo:block>
     |<fo:block font-family="serif" font-size="10pt" space-after="3mm">bbb</fo:block>
     |</fo:block>""".stripMargin
    render (elem) should be (html) 
  }
  
  
}
