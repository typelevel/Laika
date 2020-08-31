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

import cats.effect.IO
import laika.api.Transformer
import laika.ast.LengthUnit.px
import laika.ast.Path
import laika.ast.Path.Root
import laika.format.{EPUB, Markdown}
import laika.helium.config.ColorQuintet
import laika.io.helper.{InputBuilder, ResultExtractor, StringOps}
import laika.io.implicits._
import laika.io.model.StringTreeOutput
import laika.io.{FileIO, IOFunSuite}
import laika.theme.ThemeProvider

class HeliumEPUBCSSSpec extends IOFunSuite with InputBuilder with ResultExtractor with StringOps {

  def transformer (theme: ThemeProvider) = Transformer
    .from(Markdown)
    .to(EPUB.XHTML)
    .io(FileIO.blocker)
    .parallel[IO]
    .withTheme(theme)
    .build

  val singleDoc = Seq(
    Root / "name.md" -> "text"
  )
  
  def transformAndExtract(inputs: Seq[(Path, String)], helium: Helium, start: String, end: String): IO[String] = transformer(helium.build).use { t =>
    for {
      resultTree <- t.fromInput(build(inputs)).toOutput(StringTreeOutput).transform
      res        <- resultTree.extractStaticContent(Root / "helium" / "laika-helium.epub.css", start, end, FileIO.blocker)
    } yield res
  }

  def transformAndExtract(inputs: Seq[(Path, String)], helium: Helium): IO[String] = transformer(helium.build).use { t =>
    for {
      resultTree <- t.fromInput(build(inputs)).toOutput(StringTreeOutput).transform
      res        <- resultTree.extractStaticContent(Root / "helium" / "laika-helium.epub.css", FileIO.blocker)
    } yield res
  }
    
  test("defaults") {
    val expected = """--primary-color: #007c99;
                     |--primary-light: #ebf6f7;
                     |--secondary-color: #931813;
                     |--messages-info: #007c99;
                     |--messages-info-light: #ebf6f7;
                     |--messages-warning: #b1a400;
                     |--messages-warning-light: #fcfacd;
                     |--messages-error: #d83030;
                     |--messages-error-light: #ffe9e3;
                     |--syntax-base1: #F6F1EF;
                     |--syntax-base2: #AF9E84;
                     |--syntax-base3: #937F61;
                     |--syntax-base4: #645133;
                     |--syntax-base5: #362E21;
                     |--syntax-wheel1: #9A6799;
                     |--syntax-wheel2: #9F4C46;
                     |--syntax-wheel3: #A0742D;
                     |--syntax-wheel4: #7D8D4C;
                     |--syntax-wheel5: #6498AE;
                     |--body-font: "Lato";
                     |--header-font: "Lato";
                     |--code-font: "Fira Code";
                     |--body-font-size: 15px;
                     |--code-font-size: 14px;
                     |--small-font-size: 12px;
                     |--title-font-size: 34px;
                     |--header2-font-size: 28px;
                     |--header3-font-size: 20px;
                     |--header4-font-size: 15px;""".stripMargin
    transformAndExtract(singleDoc, Helium.defaults, ":root {", "}").assertEquals(expected)
  }

  test("embedded fonts") {
    val expected = 
      """@font-face {
        |font-family: "Lato";
        |font-weight: normal;
        |font-style: normal;
        |src: url("../laika/fonts/Lato-Regular.ttf");
        |}
        |@font-face {
        |font-family: "Lato";
        |font-weight: normal;
        |font-style: italic;
        |src: url("../laika/fonts/Lato-Italic.ttf");
        |}
        |@font-face {
        |font-family: "Lato";
        |font-weight: bold;
        |font-style: normal;
        |src: url("../laika/fonts/Lato-Bold.ttf");
        |}
        |@font-face {
        |font-family: "Lato";
        |font-weight: bold;
        |font-style: italic;
        |src: url("../laika/fonts/Lato-BoldItalic.ttf");
        |}
        |@font-face {
        |font-family: "Fira Code";
        |font-weight: normal;
        |font-style: normal;
        |src: url("../laika/fonts/FiraCode-Medium.otf");
        |}
        |@font-face {
        |font-family: "IcoFont";
        |font-weight: normal;
        |font-style: normal;
        |src: url("../laika/fonts/icofont.ttf");
        |}
        |""".stripMargin
    transformAndExtract(singleDoc, Helium.defaults)
      .map(_.removeIndentation.removeBlankLines.split(":root").head)
      .assertEquals(expected)
  }
  
  private val customFonts = """--primary-color: #007c99;
                              |--primary-light: #ebf6f7;
                              |--secondary-color: #931813;
                              |--messages-info: #007c99;
                              |--messages-info-light: #ebf6f7;
                              |--messages-warning: #b1a400;
                              |--messages-warning-light: #fcfacd;
                              |--messages-error: #d83030;
                              |--messages-error-light: #ffe9e3;
                              |--syntax-base1: #F6F1EF;
                              |--syntax-base2: #AF9E84;
                              |--syntax-base3: #937F61;
                              |--syntax-base4: #645133;
                              |--syntax-base5: #362E21;
                              |--syntax-wheel1: #9A6799;
                              |--syntax-wheel2: #9F4C46;
                              |--syntax-wheel3: #A0742D;
                              |--syntax-wheel4: #7D8D4C;
                              |--syntax-wheel5: #6498AE;
                              |--body-font: "Custom-Body";
                              |--header-font: "Custom-Header";
                              |--code-font: "Custom-Code";
                              |--body-font-size: 14px;
                              |--code-font-size: 13px;
                              |--small-font-size: 11px;
                              |--title-font-size: 33px;
                              |--header2-font-size: 27px;
                              |--header3-font-size: 19px;
                              |--header4-font-size: 14px;""".stripMargin

  test("custom font families and font sizes - via 'epub' selector") {
    val helium = Helium.defaults
      .epub.fontFamilies(body = "Custom-Body", headlines = "Custom-Header", code = "Custom-Code")
      .epub.fontSizes(body = px(14), code = px(13), title = px(33), 
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
                              |--secondary-color: rgb(212,212,212);
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
                              |--header4-font-size: 15px;""".stripMargin

  test("custom colors - via 'epub' selector") {
    import laika.theme.config.Color._
    val helium = Helium.defaults
      .epub.themeColors(primary = rgb(1,1,1), primaryDark = rgb(0,0,0), primaryLight = rgb(2,2,2),
        secondary = rgb(212,212,212))
      .epub.messageColors(info = hex("aaaaaa"), infoLight = hex("aaaaab"), 
        warning = hex("aaaaac"), warningLight = hex("aaaaad"),
        error = hex("aaaaae"), errorLight = hex("aaaaaf")
      )
      .epub.syntaxHighlightingColors(
        base = ColorQuintet(hex("000011"), hex("000022"), hex("000033"), hex("000044"), hex("000055")),
        wheel = ColorQuintet(hex("110011"), hex("110022"), hex("110033"), hex("110044"), hex("110055"))
      )
    transformAndExtract(singleDoc, helium, ":root {", "}").assertEquals(customColors)
  }

  test("custom colors - via 'all' selector") {
    import laika.theme.config.Color._
    val helium = Helium.defaults
      .all.themeColors(primary = rgb(1,1,1), primaryDark = rgb(0,0,0), primaryLight = rgb(2,2,2),
        secondary = rgb(212,212,212))
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

}
