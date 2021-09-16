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

package laika.helium

import cats.effect.{IO, Resource}
import laika.api.Transformer
import laika.ast.LengthUnit._
import laika.ast.Path
import laika.ast.Path.Root
import laika.format.{Markdown, XSLFO}
import laika.helium.config.ColorQuintet
import laika.io.api.TreeTransformer
import laika.io.helper.{InputBuilder, ResultExtractor, StringOps}
import laika.io.implicits._
import laika.io.model.StringTreeOutput
import laika.markdown.github.GitHubFlavor
import laika.parse.code.SyntaxHighlighting
import laika.rewrite.nav.CoverImage
import laika.theme.ThemeProvider
import laika.theme.config.Color
import munit.CatsEffectSuite

/**
  * @author Jens Halm
  */
class HeliumFORendererSpec extends CatsEffectSuite with InputBuilder with ResultExtractor with StringOps {

  def transformer (theme: ThemeProvider): Resource[IO, TreeTransformer[IO]] = Transformer
    .from(Markdown)
    .to(XSLFO)
    .using(
      GitHubFlavor,
      SyntaxHighlighting
    )
    .parallel[IO]
    .withTheme(theme)
    .build

  def transformAndExtract(inputs: Seq[(Path, String)], helium: Helium, start: String, end: String): IO[String] = transformer(helium.build).use { t =>
    for {
      resultTree <- t.fromInput(build(inputs)).toOutput(StringTreeOutput).transform
      res        <- IO.fromEither(resultTree.extractTidiedSubstring(Root / "doc.fo", start, end)
        .toRight(new RuntimeException("Missing document under test")))
    } yield res
  }
  
  test("defaults - full XSL-FO output") {
    val inputs = Seq(
      Root / "doc.md" -> "Text"
    )
    val expected = 
      """<fo:layout-master-set>
        |<fo:simple-page-master
        |master-name="default"
        |page-height="29.7cm"
        |page-width="21cm"
        |margin-top="1cm"
        |margin-bottom="1cm"
        |margin-left="2.5cm"
        |margin-right="2.5cm">
        |<fo:region-body margin-top="2cm" margin-bottom="2cm"/>
        |<fo:region-before extent="3cm"/>
        |<fo:region-after extent="1cm"/>
        |</fo:simple-page-master>
        |</fo:layout-master-set>
        |<fo:page-sequence master-reference="default">
        |<fo:static-content flow-name="xsl-region-before">
        |<fo:block border-bottom-width="1pt" border-bottom-style="solid"
        |font-family="Lato" font-weight="bold" font-size="9pt" text-align="center">
        |<fo:retrieve-marker
        |retrieve-class-name="chapter"
        |retrieve-position="first-including-carryover"
        |/>
        |</fo:block>
        |</fo:static-content>
        |<fo:static-content flow-name="xsl-region-after">
        |<fo:block height="100%" font-family="Lato" font-weight="bold" font-size="10pt" text-align="center">
        |<fo:page-number/>
        |</fo:block>
        |</fo:static-content>
        |<fo:flow flow-name="xsl-region-body">
        |<fo:block font-family="Lato" font-size="10pt" line-height="1.5" space-after="3mm" text-align="justify">Text</fo:block>
        |</fo:flow>
        |</fo:page-sequence>""".stripMargin
    val startTag = "<fo:root xmlns:fo=\"http://www.w3.org/1999/XSL/Format\" xmlns:fox=\"http://xmlgraphics.apache.org/fop/extensions\">"
    transformAndExtract(inputs, Helium.defaults, startTag, "</fo:root>").assertEquals(expected)
  }
  
  private val defaultLayout = Helium.defaults.pdfSettings.layout
  
  def withCustomLayout(helium: Helium): Helium = helium.pdf.layout(
    pageWidth = defaultLayout.pageWidth,
    pageHeight = defaultLayout.pageHeight,
    marginTop = defaultLayout.marginTop,
    marginRight = defaultLayout.marginRight,
    marginBottom = defaultLayout.marginBottom,
    marginLeft = defaultLayout.marginLeft,
    defaultBlockSpacing = mm(2),
    defaultLineHeight = 1.4,
    keepTogetherDecoratedLines = defaultLayout.keepTogetherDecoratedLines
  )
  
  test("custom master page layout") {
    val inputs = Seq(
      Root / "doc.md" -> "Text"
    )
    val helium = Helium.defaults.pdf.layout(
      pageWidth = cm(14.8), 
      pageHeight = cm(21), 
      marginTop = cm(0.7), 
      marginRight = cm(1.8), 
      marginBottom = cm(0.7), 
      marginLeft = cm(1.8), 
      defaultBlockSpacing = mm(2),
      defaultLineHeight = 1.4,
      keepTogetherDecoratedLines = 10)
    val expected =
      """master-name="default"
        |page-height="21cm"
        |page-width="14.8cm"
        |margin-top="0.7cm"
        |margin-bottom="0.7cm"
        |margin-left="1.8cm"
        |margin-right="1.8cm">""".stripMargin
    transformAndExtract(inputs, helium, "<fo:simple-page-master", "<fo:region-body margin-top=\"2cm\" margin-bottom=\"2cm\"/>")
      .assertEquals(expected)
  }
  
  test("paragraph with custom font families, font sizes and layout") {
    val inputs = Seq(
      Root / "doc.md" -> "Text"
    )
    val helium = withCustomLayout(Helium.defaults
      .all.fontFamilies(body = "Custom-Body", headlines = "Custom-Header", code = "Custom-Code")
      .all.fontSizes(body = pt(11),
        code = pt(10),
        title = pt(24),
        header2 = pt(14),
        header3 = pt(12),
        header4 = pt(11),
        small = pt(9))
      )
    val expected = 
      """<fo:block font-family="Custom-Body" font-size="11pt" line-height="1.4" space-after="2mm" text-align="justify">Text</fo:block>"""
    transformAndExtract(inputs, helium, "<fo:flow flow-name=\"xsl-region-body\">", "</fo:flow>")
      .assertEquals(expected)
  }

  test("custom font sizes and font families for titles and headers") {
    val markup =
      """
        |Title
        |=====
        |
        |## Header 2
        |
        |### Header 3
        |
        |#### Header 4
      """.stripMargin
    val inputs = Seq(
      Root / "doc.md" -> markup
    )
    val helium = withCustomLayout(Helium.defaults
      .all.fontFamilies(body = "Custom-Body", headlines = "Custom-Header", code = "Custom-Code")
      .all.fontSizes(body = pt(11),
      code = pt(10),
      title = pt(24),
      header2 = pt(14),
      header3 = pt(12),
      header4 = pt(11),
      small = pt(9))
    )
    val expected =
      """<fo:block id="_doc_title" color="#007c99" font-family="Custom-Header" font-size="24pt" font-weight="bold" keep-with-next="always" space-after="6mm" space-before="0mm">Title</fo:block>
        |<fo:block id="_doc_header-2" font-family="Custom-Header" font-size="14pt" font-weight="bold" keep-with-next="always" space-after="2mm" space-before="7mm">Header 2</fo:block>
        |<fo:block id="_doc_header-3" font-family="Custom-Header" font-size="12pt" font-weight="bold" keep-with-next="always" space-after="2mm" space-before="7mm">Header 3</fo:block>
        |<fo:block id="_doc_header-4" font-family="Custom-Header" font-size="11pt" font-weight="bold" keep-with-next="always" space-after="2mm" space-before="7mm">Header 4</fo:block>""".stripMargin
    transformAndExtract(inputs, helium, "<fo:flow flow-name=\"xsl-region-body\">", "</fo:flow>")
      .assertEquals(expected)
  }

  test("custom link colors") {
    val markup = "Text with a [link](http://foo.bar)."
    val inputs = Seq(
      Root / "doc.md" -> markup
    )
    val helium = Helium.defaults.pdf.themeColors(
      primary = Color.hex("000011"),
      primaryMedium = Color.hex("000011"),
      primaryLight = Color.hex("000022"),
      secondary = Color.hex("330033"),
      text = Color.hex("990099"),
      background = Color.hex("eeeeee"),
      bgGradient = (Color.hex("000099"), Color.hex("000011"))
    )
    val expected = """<fo:block font-family="Lato" font-size="10pt" line-height="1.5" space-after="3mm" text-align="justify">Text with a <fo:basic-link color="#330033" external-destination="http://foo.bar" font-weight="bold">link</fo:basic-link>.</fo:block>"""
    transformAndExtract(inputs, helium, "<fo:flow flow-name=\"xsl-region-body\">", "</fo:flow>")
      .assertEquals(expected)
  }
  
  test("custom color scheme for syntax highlighting") {
    import Color._
    val markup =
      """
        |```scala
        |val stuff = Seq(
        |  "text", 
        |  7, 
        |  new Foo
        |)
        |```
      """.stripMargin
    val inputs = Seq(
      Root / "doc.md" -> markup
    )
    val helium = Helium.defaults.pdf.syntaxHighlightingColors(
      base = ColorQuintet(hex("000011"), hex("000022"), hex("000033"), hex("000044"), hex("000055")),
      wheel = ColorQuintet(hex("990011"), hex("990022"), hex("990033"), hex("990044"), hex("990055"))
    )
    val expected = 
      """<fo:block background-color="#000011" color="#000055" font-family="Fira Code" font-size="9pt" fox:border-radius="2mm" line-height="1.4" linefeed-treatment="preserve" margin-left="2mm" margin-right="2mm" padding="2mm" page-break-inside="avoid" space-after="6mm" white-space-collapse="false" white-space-treatment="preserve"><fo:inline color="#990022">val</fo:inline> <fo:inline color="#000044">stuff</fo:inline> = <fo:inline color="#990055">Seq</fo:inline>(
        |<fo:inline color="#990044">&quot;text&quot;</fo:inline>,
        |<fo:inline color="#990044">7</fo:inline>,
        |<fo:inline color="#990022">new</fo:inline> <fo:inline color="#990055">Foo</fo:inline>
        |)</fo:block>""".stripMargin
    transformAndExtract(inputs, helium, "<fo:flow flow-name=\"xsl-region-body\">", "</fo:flow>")
      .assertEquals(expected)
  }
  
  test("callouts with icons") {
    val markup =
      """
        |@:callout(warning)
        |
        |You really should not do this.
        |
        |@:@
      """.stripMargin
    val inputs = Seq(
      Root / "doc.md" -> markup
    )
    val expected = 
      """<fo:block background-color="#fcfacd" border-left="3pt solid #b1a400" font-family="Lato" font-size="10pt" fox:border-after-end-radius="2mm" fox:border-before-end-radius="2mm" line-height="1.5" margin-left="2mm" margin-right="2mm" padding="3mm 3mm 0.1mm 3mm" space-after="6mm">
        |<fo:block padding-top="-2mm"><fo:inline color="#b1a400" font-family="IcoFont" font-size="16pt">&#xf026;</fo:inline></fo:block>
        |<fo:block font-family="Lato" font-size="10pt" line-height="1.5" space-after="3mm" text-align="justify">You really should not do this.</fo:block>
        |</fo:block>""".stripMargin
    transformAndExtract(inputs, Helium.defaults, "<fo:flow flow-name=\"xsl-region-body\">", "</fo:flow>")
      .assertEquals(expected)
  }
  
  test("cover image") {
    val inputs = Seq(
      Root / "doc.md" -> "Text",
      Root / "cover.png" -> "",
    )
    val helium = Helium.defaults.pdf.coverImages(
      CoverImage(Root / "cover.png")
    )
    val expected = 
      """<fox:external-document src="/cover.png" width="21cm" height="29.7cm" content-width="21cm"/>"""
    transformAndExtract(inputs, helium, "</fo:layout-master-set>", "<fo:page-sequence master-reference=\"default\">")
      .assertEquals(expected)
  }
  
}
