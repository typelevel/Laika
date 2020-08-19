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

package laika.helium.config

import laika.ast.DocumentMetadata
import laika.ast.LengthUnit.{cm, mm, pt, px}
import laika.helium.Helium
import laika.theme.{BookConfig, Color, Font, FontDefinition, FontSizes, FontStyle, FontWeight, ThemeFonts}

private[laika] object HeliumDefaults {

  private val fontPath = "laika/helium/fonts/"

  private val defaultFonts = Seq(
    FontDefinition(
      Font.embedResource(fontPath + "Lato/Lato-Regular.ttf").webCSS("http://fonts.googleapis.com/css?family=Lato:400,700"),
      "Lato", FontWeight.Normal, FontStyle.Normal
    ),
    FontDefinition(
      Font.embedResource(fontPath + "Lato/Lato-Italic.ttf"),
      "Lato", FontWeight.Normal, FontStyle.Italic
    ),
    FontDefinition(
      Font.embedResource(fontPath + "Lato/Lato-Bold.ttf"),
      "Lato", FontWeight.Bold, FontStyle.Normal
    ),
    FontDefinition(
      Font.embedResource(fontPath + "Lato/Lato-BoldItalic.ttf"),
      "Lato", FontWeight.Bold, FontStyle.Italic
    ),
    FontDefinition(
      Font.embedResource(fontPath + "FiraCode/FiraCode-Medium.otf").webCSS("https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css"),
      "FiraCode", FontWeight.Normal, FontStyle.Normal
    ),
    FontDefinition(
      Font.embedResource(fontPath + "icofont/icofont.ttf").webCSS("../icons/icofont.min.css"),
      "IcoFont", FontWeight.Normal, FontStyle.Normal
    ),
  )
  private val defaultThemeFonts = ThemeFonts("Lato", "Lato", "FiraCode")
  private val defaultSiteSettings = SiteSettings(
    fontResources = defaultFonts,
    themeFonts = defaultThemeFonts,
    fontSizes = FontSizes( // TODO
      body = pt(10),
      code = pt(9),
      title = pt(24),
      header2 = pt(14),
      header3 = pt(12),
      header4 = pt(11),
      small = pt(8)
    ),
    colors = ColorSet( // TODO
      primary = Color.hex("007c99"),
      secondary = Color.hex("931813"),
      primaryDark = Color.hex("007c99"),
      primaryLight = Color.hex("ebf6f7"),
      messages = MessageColors(
        info = Color.hex("007c99"),
        infoLight = Color.hex("ebf6f7"),
        warning = Color.hex("b1a400"),
        warningLight = Color.hex("fcfacd"),
        error = Color.hex("d83030"),
        errorLight = Color.hex("ffe9e3"),
      ),
      syntaxHighlighting = SyntaxColors(
        base = ColorQuintet(
          Color.hex("F6F1EF"), Color.hex("AF9E84"), Color.hex("937F61"), Color.hex("645133"), Color.hex("362E21")
        ),
        wheel = ColorQuintet(
          Color.hex("9A6799"), Color.hex("9F4C46"), Color.hex("A0742D"), Color.hex("7D8D4C"), Color.hex("6498AE")
        )
      )
    ),
    htmlIncludes = HTMLIncludes(),
    landingPage = None,
    webLayout = WebLayout(
      contentWidth = px(860),
      navigationWidth = px(275),
      defaultBlockSpacing = px(10),
      defaultLineHeight = 1.5,
      anchorPlacement = AnchorPlacement.Left
    ),
    metadata = DocumentMetadata()
  )
  private val defaultEPUBSettings = EPUBSettings(
    bookConfig = BookConfig(fonts = defaultFonts),
    themeFonts = defaultThemeFonts,
    fontSizes = FontSizes( // TODO
      body = pt(10),
      code = pt(9),
      title = pt(24),
      header2 = pt(14),
      header3 = pt(12),
      header4 = pt(11),
      small = pt(8)
    ),
    colors = ColorSet( // TODO
      primary = Color.hex("007c99"),
      secondary = Color.hex("931813"),
      primaryDark = Color.hex("007c99"),
      primaryLight = Color.hex("ebf6f7"),
      messages = MessageColors(
        info = Color.hex("007c99"),
        infoLight = Color.hex("ebf6f7"),
        warning = Color.hex("b1a400"),
        warningLight = Color.hex("fcfacd"),
        error = Color.hex("d83030"),
        errorLight = Color.hex("ffe9e3"),
      ),
      syntaxHighlighting = SyntaxColors(
        base = ColorQuintet(
          Color.hex("F6F1EF"), Color.hex("AF9E84"), Color.hex("937F61"), Color.hex("645133"), Color.hex("362E21")
        ),
        wheel = ColorQuintet(
          Color.hex("9A6799"), Color.hex("9F4C46"), Color.hex("A0742D"), Color.hex("7D8D4C"), Color.hex("6498AE")
        )
      )
    ),
    htmlIncludes = HTMLIncludes()
  )

  private val defaultPDFSettings = PDFSettings(
    bookConfig = BookConfig(fonts = defaultFonts),
    themeFonts = defaultThemeFonts,
    fontSizes = FontSizes( // TODO
      body = pt(10),
      code = pt(9),
      title = pt(24),
      header2 = pt(14),
      header3 = pt(12),
      header4 = pt(11),
      small = pt(8)
    ),
    colors = ColorSet( // TODO
      primary = Color.hex("007c99"),
      secondary = Color.hex("931813"),
      primaryDark = Color.hex("007c99"),
      primaryLight = Color.hex("ebf6f7"),
      messages = MessageColors(
        info = Color.hex("007c99"),
        infoLight = Color.hex("ebf6f7"),
        warning = Color.hex("b1a400"),
        warningLight = Color.hex("fcfacd"),
        error = Color.hex("d83030"),
        errorLight = Color.hex("ffe9e3"),
      ),
      syntaxHighlighting = SyntaxColors(
        base = ColorQuintet(
          Color.hex("F6F1EF"), Color.hex("AF9E84"), Color.hex("937F61"), Color.hex("645133"), Color.hex("362E21")
        ),
        wheel = ColorQuintet(
          Color.hex("9A6799"), Color.hex("9F4C46"), Color.hex("A0742D"), Color.hex("7D8D4C"), Color.hex("6498AE")
        )
      )
    ),
    pdfLayout = PDFLayout(
      pageWidth = cm(21),
      pageHeight = cm(29.7),
      marginTop = cm(1),
      marginRight = cm(2.5),
      marginBottom = cm(1),
      marginLeft = cm(2.5),
      defaultBlockSpacing = mm(3),
      defaultLineHeight = 1.5,
      keepTogetherDecoratedLines = 12
    )
  )

  val instance: Helium = new Helium(defaultSiteSettings, defaultEPUBSettings, defaultPDFSettings)
  
}
