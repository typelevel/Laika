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

import cats.effect.{ IO, Resource }
import laika.api.Transformer
import laika.ast.LengthUnit.px
import laika.ast.Path
import laika.ast.Path.Root
import laika.format.{ HTML, Markdown }
import laika.helium.config.{ AnchorPlacement, ColorQuintet }
import laika.io.api.TreeTransformer
import laika.io.helper.{ InputBuilder, ResultExtractor, StringOps }
import laika.io.implicits._
import laika.io.model.StringTreeOutput
import laika.rewrite.link.LinkConfig
import laika.theme.ThemeProvider
import munit.CatsEffectSuite

class HeliumSiteCSSSpec extends CatsEffectSuite with InputBuilder with ResultExtractor
    with StringOps {

  def transformer(theme: ThemeProvider): Resource[IO, TreeTransformer[IO]] = Transformer
    .from(Markdown)
    .to(HTML)
    .withConfigValue(LinkConfig(excludeFromValidation = Seq(Root)))
    .parallel[IO]
    .withTheme(theme)
    .build

  val singleDoc = Seq(
    Root / "name.md" -> "text"
  )

  def transformAndExtract(
      inputs: Seq[(Path, String)],
      helium: Helium,
      start: String,
      end: String
  ): IO[String] = transformer(helium.build).use { t =>
    for {
      resultTree <- t.fromInput(build(inputs)).toOutput(StringTreeOutput).transform
      res <- resultTree.extractStaticContent(Root / "helium" / "laika-helium.css", start, end)
    } yield res
  }

  private val defaultColors = """--primary-color: #007c99;
                                |--primary-light: #ebf6f7;
                                |--primary-medium: #a7d4de;
                                |--secondary-color: #931813;
                                |--text-color: #5f5f5f;
                                |--bg-color: #ffffff;
                                |--gradient-top: #095269;
                                |--gradient-bottom: #007c99;
                                |--top-color: var(--primary-color);
                                |--top-bg: var(--primary-light);
                                |--top-hover: var(--secondary-color);
                                |--top-border: var(--primary-medium);
                                |--messages-info: #007c99;
                                |--messages-info-light: #ebf6f7;
                                |--messages-warning: #b1a400;
                                |--messages-warning-light: #fcfacd;
                                |--messages-error: #d83030;
                                |--messages-error-light: #ffe9e3;
                                |--syntax-base1: #2a3236;
                                |--syntax-base2: #8c878e;
                                |--syntax-base3: #b2adb4;
                                |--syntax-base4: #bddcee;
                                |--syntax-base5: #e8e8e8;
                                |--syntax-wheel1: #e28e93;
                                |--syntax-wheel2: #ef9725;
                                |--syntax-wheel3: #ffc66d;
                                |--syntax-wheel4: #7fb971;
                                |--syntax-wheel5: #4dbed4;""".stripMargin

  private val defaultFonts = """--body-font: "Lato", sans-serif;
                               |--header-font: "Lato", sans-serif;
                               |--code-font: "Fira Code", monospace;
                               |--body-font-size: 15px;
                               |--code-font-size: 14px;
                               |--small-font-size: 12px;
                               |--title-font-size: 34px;
                               |--header2-font-size: 28px;
                               |--header3-font-size: 20px;
                               |--header4-font-size: 15px;""".stripMargin

  private val defaultLayout = s"""--block-spacing: 10px;
                                 |--line-height: 1.5;
                                 |--content-width: 860px;
                                 |--nav-width: 275px;
                                 |--top-bar-height: 35px;""".stripMargin

  private val landingPage = """--landing-subtitle-font-size: 32px;
                              |--teaser-title-font-size: 28px;
                              |--teaser-body-font-size: 17px;""".stripMargin

  private val colorScheme = "color-scheme: light dark;"

  private val heliumBase = Helium.defaults.site.landingPage()

  test("defaults") {
    val expected = s"""$defaultColors
                      |$defaultFonts
                      |$defaultLayout
                      |$landingPage
                      |$colorScheme""".stripMargin
    transformAndExtract(singleDoc, heliumBase, ":root {", "}").assertEquals(expected)
  }

  private val customFonts = s"""$defaultColors
                               |--body-font: "Custom-Body", sans-serif;
                               |--header-font: "Custom-Header", sans-serif;
                               |--code-font: "Custom-Code", monospace;
                               |--body-font-size: 14px;
                               |--code-font-size: 13px;
                               |--small-font-size: 11px;
                               |--title-font-size: 33px;
                               |--header2-font-size: 27px;
                               |--header3-font-size: 19px;
                               |--header4-font-size: 14px;
                               |$defaultLayout
                               |$landingPage
                               |$colorScheme""".stripMargin

  test("custom font families and font sizes - via 'site' selector") {
    val helium = heliumBase
      .site.fontFamilies(body = "Custom-Body", headlines = "Custom-Header", code = "Custom-Code")
      .site.fontSizes(
        body = px(14),
        code = px(13),
        title = px(33),
        header2 = px(27),
        header3 = px(19),
        header4 = px(14),
        small = px(11)
      )
    transformAndExtract(singleDoc, helium, ":root {", "}").assertEquals(customFonts)
  }

  test("custom font families and font sizes - via 'all' selector") {
    val helium = heliumBase
      .all.fontFamilies(body = "Custom-Body", headlines = "Custom-Header", code = "Custom-Code")
      .all.fontSizes(
        body = px(14),
        code = px(13),
        title = px(33),
        header2 = px(27),
        header3 = px(19),
        header4 = px(14),
        small = px(11)
      )
    transformAndExtract(singleDoc, helium, ":root {", "}").assertEquals(customFonts)
  }

  private val customColors = s"""--primary-color: rgb(1,1,1);
                                |--primary-light: rgb(2,2,2);
                                |--primary-medium: rgb(4,4,4);
                                |--secondary-color: rgb(212,212,212);
                                |--text-color: rgb(10,10,10);
                                |--bg-color: rgb(11,11,11);
                                |--gradient-top: rgb(0,0,0);
                                |--gradient-bottom: rgb(9,9,9);
                                |--top-color: var(--primary-color);
                                |--top-bg: var(--primary-light);
                                |--top-hover: var(--secondary-color);
                                |--top-border: var(--primary-medium);
                                |--messages-info: #aaaaaa;
                                |--messages-info-light: #aaaaab;
                                |--messages-warning: #aaaaac;
                                |--messages-warning-light: #aaaaad;
                                |--messages-error: #aaaaae;
                                |--messages-error-light: #aaaaaf;
                                |--syntax-base1: #000011;
                                |--syntax-base2: #000022;
                                |--syntax-base3: #000033;
                                |--syntax-base4: #000044;
                                |--syntax-base5: #000055;
                                |--syntax-wheel1: #110011;
                                |--syntax-wheel2: #110022;
                                |--syntax-wheel3: #110033;
                                |--syntax-wheel4: #110044;
                                |--syntax-wheel5: #110055;
                                |$defaultFonts
                                |$defaultLayout
                                |$landingPage
                                |$colorScheme""".stripMargin

  private val darkModeColors = """}
                                 |@media (prefers-color-scheme: dark) {
                                 |:root {
                                 |--primary-color: rgb(11,11,11);
                                 |--primary-light: rgb(12,12,12);
                                 |--primary-medium: rgb(14,14,14);
                                 |--secondary-color: rgb(112,112,112);
                                 |--text-color: rgb(110,110,110);
                                 |--bg-color: rgb(111,111,111);
                                 |--gradient-top: rgb(10,10,10);
                                 |--gradient-bottom: rgb(19,19,19);
                                 |--top-color: var(--primary-color);
                                 |--top-bg: var(--primary-light);
                                 |--top-hover: var(--secondary-color);
                                 |--top-border: var(--primary-medium);
                                 |--messages-info: #00aaaa;
                                 |--messages-info-light: #00aaab;
                                 |--messages-warning: #00aaac;
                                 |--messages-warning-light: #00aaad;
                                 |--messages-error: #00aaae;
                                 |--messages-error-light: #00aaaf;
                                 |--syntax-base1: #200011;
                                 |--syntax-base2: #200022;
                                 |--syntax-base3: #200033;
                                 |--syntax-base4: #200044;
                                 |--syntax-base5: #200055;
                                 |--syntax-wheel1: #210011;
                                 |--syntax-wheel2: #210022;
                                 |--syntax-wheel3: #210033;
                                 |--syntax-wheel4: #210044;
                                 |--syntax-wheel5: #210055;""".stripMargin

  test("custom colors - via 'site' selector") {
    import laika.theme.config.Color._
    val helium = heliumBase
      .site.themeColors(
        primary = rgb(1, 1, 1),
        primaryLight = rgb(2, 2, 2),
        primaryMedium = rgb(4, 4, 4),
        secondary = rgb(212, 212, 212),
        text = rgb(10, 10, 10),
        background = rgb(11, 11, 11),
        bgGradient = (rgb(0, 0, 0), rgb(9, 9, 9))
      )
      .site.messageColors(
        info = hex("aaaaaa"),
        infoLight = hex("aaaaab"),
        warning = hex("aaaaac"),
        warningLight = hex("aaaaad"),
        error = hex("aaaaae"),
        errorLight = hex("aaaaaf")
      )
      .site.syntaxHighlightingColors(
        base =
          ColorQuintet(hex("000011"), hex("000022"), hex("000033"), hex("000044"), hex("000055")),
        wheel =
          ColorQuintet(hex("110011"), hex("110022"), hex("110033"), hex("110044"), hex("110055"))
      )
    transformAndExtract(singleDoc, helium, ":root {", "}").assertEquals(customColors)
  }

  test("custom colors - via 'all' selector") {
    import laika.theme.config.Color._
    val helium = heliumBase
      .all.themeColors(
        primary = rgb(1, 1, 1),
        primaryLight = rgb(2, 2, 2),
        primaryMedium = rgb(4, 4, 4),
        secondary = rgb(212, 212, 212),
        text = rgb(10, 10, 10),
        background = rgb(11, 11, 11),
        bgGradient = (rgb(0, 0, 0), rgb(9, 9, 9))
      )
      .all.messageColors(
        info = hex("aaaaaa"),
        infoLight = hex("aaaaab"),
        warning = hex("aaaaac"),
        warningLight = hex("aaaaad"),
        error = hex("aaaaae"),
        errorLight = hex("aaaaaf")
      )
      .all.syntaxHighlightingColors(
        base =
          ColorQuintet(hex("000011"), hex("000022"), hex("000033"), hex("000044"), hex("000055")),
        wheel =
          ColorQuintet(hex("110011"), hex("110022"), hex("110033"), hex("110044"), hex("110055"))
      )
    transformAndExtract(singleDoc, helium, ":root {", "}").assertEquals(customColors)
  }

  test("custom colors in dark mode") {
    import laika.theme.config.Color._
    val helium = heliumBase
      .site.themeColors(
        primary = rgb(1, 1, 1),
        primaryLight = rgb(2, 2, 2),
        primaryMedium = rgb(4, 4, 4),
        secondary = rgb(212, 212, 212),
        text = rgb(10, 10, 10),
        background = rgb(11, 11, 11),
        bgGradient = (rgb(0, 0, 0), rgb(9, 9, 9))
      )
      .site.messageColors(
        info = hex("aaaaaa"),
        infoLight = hex("aaaaab"),
        warning = hex("aaaaac"),
        warningLight = hex("aaaaad"),
        error = hex("aaaaae"),
        errorLight = hex("aaaaaf")
      )
      .site.syntaxHighlightingColors(
        base =
          ColorQuintet(hex("000011"), hex("000022"), hex("000033"), hex("000044"), hex("000055")),
        wheel =
          ColorQuintet(hex("110011"), hex("110022"), hex("110033"), hex("110044"), hex("110055"))
      )
      .site.darkMode.themeColors(
        primary = rgb(11, 11, 11),
        primaryLight = rgb(12, 12, 12),
        primaryMedium = rgb(14, 14, 14),
        secondary = rgb(112, 112, 112),
        text = rgb(110, 110, 110),
        background = rgb(111, 111, 111),
        bgGradient = (rgb(10, 10, 10), rgb(19, 19, 19))
      )
      .site.darkMode.messageColors(
        info = hex("00aaaa"),
        infoLight = hex("00aaab"),
        warning = hex("00aaac"),
        warningLight = hex("00aaad"),
        error = hex("00aaae"),
        errorLight = hex("00aaaf")
      )
      .site.darkMode.syntaxHighlightingColors(
        base =
          ColorQuintet(hex("200011"), hex("200022"), hex("200033"), hex("200044"), hex("200055")),
        wheel =
          ColorQuintet(hex("210011"), hex("210022"), hex("210033"), hex("210044"), hex("210055"))
      )
    transformAndExtract(singleDoc, helium, ":root {", "}\n}").assertEquals(
      customColors + "\n" + darkModeColors
    )
  }

  test("dark mode disabled") {
    val helium = heliumBase.site.darkMode.disabled
    transformAndExtract(singleDoc, helium, ":root {", "}").assertEquals(
      defaultColors + "\n" + defaultFonts + "\n" + defaultLayout + "\n" + landingPage
    )
  }

  private val customLayout = s"""$defaultColors
                                |$defaultFonts
                                |--block-spacing: 9px;
                                |--line-height: 1.2;
                                |--content-width: 1000px;
                                |--nav-width: 300px;
                                |--top-bar-height: 55px;
                                |$landingPage
                                |$colorScheme""".stripMargin

  test("layout") {
    val helium = heliumBase
      .site.layout(
        contentWidth = px(1000),
        navigationWidth = px(300),
        topBarHeight = px(55),
        defaultBlockSpacing = px(9),
        defaultLineHeight = 1.2,
        anchorPlacement = AnchorPlacement.None
      )
    transformAndExtract(singleDoc, helium, ":root {", "}").assertEquals(customLayout)
  }

}
