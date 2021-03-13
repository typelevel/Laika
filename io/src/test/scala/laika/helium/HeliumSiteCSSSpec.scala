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
import laika.ast.LengthUnit.px
import laika.ast.Path
import laika.ast.Path.Root
import laika.format.{HTML, Markdown}
import laika.helium.config.{AnchorPlacement, ColorQuintet}
import laika.io.IOFunSuite
import laika.io.api.TreeTransformer
import laika.io.helper.{InputBuilder, ResultExtractor, StringOps}
import laika.io.implicits._
import laika.io.model.StringTreeOutput
import laika.theme.ThemeProvider

class HeliumSiteCSSSpec extends IOFunSuite with InputBuilder with ResultExtractor with StringOps {

  def transformer (theme: ThemeProvider): Resource[IO, TreeTransformer[IO]] = Transformer
    .from(Markdown)
    .to(HTML)
    .parallel[IO]
    .withTheme(theme)
    .build

  val singleDoc = Seq(
    Root / "name.md" -> "text"
  )
  
  def transformAndExtract(inputs: Seq[(Path, String)], helium: Helium, start: String, end: String): IO[String] = transformer(helium.build).use { t =>
    for {
      resultTree <- t.fromInput(build(inputs)).toOutput(StringTreeOutput).transform
      res        <- resultTree.extractStaticContent(Root / "helium" / "laika-helium.css", start, end)
    } yield res
  }
    
  test("defaults") {
    val expected = """--primary-color: #007c99;
                     |--primary-light: #ebf6f7;
                     |--primary-medium: #a7d4de;
                     |--primary-dark: #095269;
                     |--secondary-color: #931813;
                     |--text-color: #5f5f5f;
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
                     |--syntax-wheel5: #4dbed4;
                     |--body-font: "Lato";
                     |--header-font: "Lato";
                     |--code-font: "Fira Code";
                     |--body-font-size: 15px;
                     |--code-font-size: 14px;
                     |--small-font-size: 12px;
                     |--title-font-size: 34px;
                     |--header2-font-size: 28px;
                     |--header3-font-size: 20px;
                     |--header4-font-size: 15px;
                     |--block-spacing: 10px;
                     |--line-height: 1.5;
                     |--content-width: 860px;
                     |--nav-width: 275px;""".stripMargin
    transformAndExtract(singleDoc, Helium.defaults, ":root {", "}").assertEquals(expected)
  }
  
  private val customFonts = """--primary-color: #007c99;
                              |--primary-light: #ebf6f7;
                              |--primary-medium: #a7d4de;
                              |--primary-dark: #095269;
                              |--secondary-color: #931813;
                              |--text-color: #5f5f5f;
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
                              |--syntax-wheel5: #4dbed4;
                              |--body-font: "Custom-Body";
                              |--header-font: "Custom-Header";
                              |--code-font: "Custom-Code";
                              |--body-font-size: 14px;
                              |--code-font-size: 13px;
                              |--small-font-size: 11px;
                              |--title-font-size: 33px;
                              |--header2-font-size: 27px;
                              |--header3-font-size: 19px;
                              |--header4-font-size: 14px;
                              |--block-spacing: 10px;
                              |--line-height: 1.5;
                              |--content-width: 860px;
                              |--nav-width: 275px;""".stripMargin

  test("custom font families and font sizes - via 'site' selector") {
    val helium = Helium.defaults
      .site.fontFamilies(body = "Custom-Body", headlines = "Custom-Header", code = "Custom-Code")
      .site.fontSizes(body = px(14), code = px(13), title = px(33), 
        header2 = px(27), header3 = px(19), header4 = px(14), small = px(11))
    transformAndExtract(singleDoc, helium, ":root {", "}").assertEquals(customFonts)
  }

  test("custom font families and font sizes - via 'all' selector") {
    val helium = Helium.defaults
      .all.fontFamilies(body = "Custom-Body", headlines = "Custom-Header", code = "Custom-Code")
      .all.fontSizes(body = px(14), code = px(13), title = px(33),
      header2 = px(27), header3 = px(19), header4 = px(14), small = px(11))
    transformAndExtract(singleDoc, helium, ":root {", "}").assertEquals(customFonts)
  }

  private val customColors = """--primary-color: rgb(1,1,1);
                              |--primary-light: rgb(2,2,2);
                              |--primary-medium: rgb(4,4,4);
                              |--primary-dark: rgb(0,0,0);
                              |--secondary-color: rgb(212,212,212);
                              |--text-color: rgb(10,10,10);
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
                              |--body-font: "Lato";
                              |--header-font: "Lato";
                              |--code-font: "Fira Code";
                              |--body-font-size: 15px;
                              |--code-font-size: 14px;
                              |--small-font-size: 12px;
                              |--title-font-size: 34px;
                              |--header2-font-size: 28px;
                              |--header3-font-size: 20px;
                              |--header4-font-size: 15px;
                              |--block-spacing: 10px;
                              |--line-height: 1.5;
                              |--content-width: 860px;
                              |--nav-width: 275px;""".stripMargin

  test("custom colors - via 'site' selector") {
    import laika.theme.config.Color._
    val helium = Helium.defaults
      .site.themeColors(primary = rgb(1,1,1), primaryDark = rgb(0,0,0), primaryLight = rgb(2,2,2), primaryMedium = rgb(4,4,4),
        secondary = rgb(212,212,212), text = rgb(10,10,10))
      .site.messageColors(info = hex("aaaaaa"), infoLight = hex("aaaaab"), 
        warning = hex("aaaaac"), warningLight = hex("aaaaad"),
        error = hex("aaaaae"), errorLight = hex("aaaaaf")
      )
      .site.syntaxHighlightingColors(
        base = ColorQuintet(hex("000011"), hex("000022"), hex("000033"), hex("000044"), hex("000055")),
        wheel = ColorQuintet(hex("110011"), hex("110022"), hex("110033"), hex("110044"), hex("110055"))
      )
    transformAndExtract(singleDoc, helium, ":root {", "}").assertEquals(customColors)
  }

  test("custom colors - via 'all' selector") {
    import laika.theme.config.Color._
    val helium = Helium.defaults
      .all.themeColors(primary = rgb(1,1,1), primaryDark = rgb(0,0,0), primaryLight = rgb(2,2,2), primaryMedium = rgb(4,4,4),
        secondary = rgb(212,212,212), text = rgb(10,10,10))
      .all.messageColors(info = hex("aaaaaa"), infoLight = hex("aaaaab"),
        warning = hex("aaaaac"), warningLight = hex("aaaaad"),
        error = hex("aaaaae"), errorLight = hex("aaaaaf")
      )
      .all.syntaxHighlightingColors(
        base = ColorQuintet(hex("000011"), hex("000022"), hex("000033"), hex("000044"), hex("000055")),
        wheel = ColorQuintet(hex("110011"), hex("110022"), hex("110033"), hex("110044"), hex("110055"))
      )
    transformAndExtract(singleDoc, helium, ":root {", "}").assertEquals(customColors)
  }

  private val customLayout = """--primary-color: #007c99;
                               |--primary-light: #ebf6f7;
                               |--primary-medium: #a7d4de;
                               |--primary-dark: #095269;
                               |--secondary-color: #931813;
                               |--text-color: #5f5f5f;
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
                               |--syntax-wheel5: #4dbed4;
                               |--body-font: "Lato";
                               |--header-font: "Lato";
                               |--code-font: "Fira Code";
                               |--body-font-size: 15px;
                               |--code-font-size: 14px;
                               |--small-font-size: 12px;
                               |--title-font-size: 34px;
                               |--header2-font-size: 28px;
                               |--header3-font-size: 20px;
                               |--header4-font-size: 15px;
                               |--block-spacing: 9px;
                               |--line-height: 1.2;
                               |--content-width: 1000px;
                               |--nav-width: 300px;""".stripMargin
  
  test("layout") {
    val helium = Helium.defaults
      .site.layout(contentWidth = px(1000), navigationWidth = px(300), 
        defaultBlockSpacing = px(9), defaultLineHeight = 1.2, anchorPlacement = AnchorPlacement.None)
    transformAndExtract(singleDoc, helium, ":root {", "}").assertEquals(customLayout)
  }

}
