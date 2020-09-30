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

import java.util.Locale

import laika.ast.DocumentMetadata
import laika.ast.LengthUnit.{cm, em, mm, pt, px}
import laika.helium.Helium
import laika.theme.config._

private[helium] object HeliumDefaults {

  private val fontPath = "laika/helium/fonts/"

  private val defaultFonts = Seq(
    FontDefinition(
      Font.embedResource(fontPath + "Lato/Lato-Regular.ttf").webCSS("https://fonts.googleapis.com/css?family=Lato:400,700"),
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
      "Fira Code", FontWeight.Normal, FontStyle.Normal
    ),
    FontDefinition(
      Font.embedResource(fontPath + "icofont/fonts/icofont.ttf"),
      "IcoFont", FontWeight.Normal, FontStyle.Normal
    ),
  )
  private val defaultThemeFonts = ThemeFonts("Lato", "Lato", "Fira Code")
  private val defaultMessageColors = MessageColors(
    info = Color.hex("007c99"),
    infoLight = Color.hex("ebf6f7"),
    warning = Color.hex("b1a400"),
    warningLight = Color.hex("fcfacd"),
    error = Color.hex("d83030"),
    errorLight = Color.hex("ffe9e3"),
  )
  private val syntaxDarkScheme = SyntaxColors(
    base = ColorQuintet(
      Color.hex("2a3236"), Color.hex("8c878e"), Color.hex("b2adb4"), Color.hex("bddcee"), Color.hex("e8e8e8")
    ),
    wheel = ColorQuintet(
      Color.hex("e28e93"), Color.hex("ef9725"), Color.hex("ffc66d"), Color.hex("7fb971"), Color.hex("4dbed4")
    )
  )
  private val syntaxLightScheme = SyntaxColors(
    base = ColorQuintet(
      Color.hex("F6F1EF"), Color.hex("AF9E84"), Color.hex("937F61"), Color.hex("645133"), Color.hex("362E21")
    ),
    wheel = ColorQuintet(
      Color.hex("9A6799"), Color.hex("9F4C46"), Color.hex("A0742D"), Color.hex("7D8D4C"), Color.hex("6498AE")
    )
  )
  def colors (syntaxScheme: SyntaxColors): ColorSet = ColorSet(
    primary = Color.hex("007c99"),
    secondary = Color.hex("931813"),
    primaryDark = Color.hex("095269"),
    primaryMedium = Color.hex("a7d4de"),
    primaryLight = Color.hex("ebf6f7"),
    text = Color.hex("5f5f5f"),
    messages = defaultMessageColors,
    syntaxHighlighting = syntaxScheme
  )
  
  private val defaultSiteSettings = SiteSettings(
    fontResources = defaultFonts,
    themeFonts = defaultThemeFonts,
    fontSizes = FontSizes(
      body = px(15),
      code = px(14),
      title = px(34),
      header2 = px(28),
      header3 = px(20),
      header4 = px(15),
      small = px(12)
    ),
    colors = colors(syntaxDarkScheme),
    htmlIncludes = HTMLIncludes(),
    landingPage = None,
    layout = WebLayout(
      contentWidth = px(860),
      navigationWidth = px(275),
      defaultBlockSpacing = px(10),
      defaultLineHeight = 1.5,
      anchorPlacement = AnchorPlacement.Left
    ),
    metadata = DocumentMetadata(
      language = Some(Locale.getDefault.toLanguageTag)
    )
  )
  private val defaultEPUBSettings = EPUBSettings(
    bookConfig = BookConfig(
      fonts = defaultFonts, 
      navigationDepth = Some(2) // chosen as default as iBooks messes with the hierarchy of entries when using more than 2 levels
    ),
    themeFonts = defaultThemeFonts,
    fontSizes = FontSizes(
      body    = em(1.0),
      code    = em(0.9),
      title   = em(2.0),
      header2 = em(1.6),
      header3 = em(1.3),
      header4 = em(1.1),
      small   = em(0.8)
    ),
    colors = colors(syntaxLightScheme),
    htmlIncludes = HTMLIncludes(),
    layout = EPUBLayout(
      defaultBlockSpacing = px(10),
      defaultLineHeight = 1.5,
      keepTogetherDecoratedLines = 12
    ),
    coverImages = Nil
  )

  private val defaultPDFSettings = PDFSettings(
    bookConfig = BookConfig(fonts = defaultFonts),
    themeFonts = defaultThemeFonts,
    fontSizes = FontSizes(
      body = pt(10),
      code = pt(9),
      title = pt(24),
      header2 = pt(14),
      header3 = pt(12),
      header4 = pt(11),
      small = pt(8)
    ),
    colors = colors(syntaxLightScheme),
    layout = PDFLayout(
      pageWidth = cm(21),
      pageHeight = cm(29.7),
      marginTop = cm(1),
      marginRight = cm(2.5),
      marginBottom = cm(1),
      marginLeft = cm(2.5),
      defaultBlockSpacing = mm(3),
      defaultLineHeight = 1.5,
      keepTogetherDecoratedLines = 12
    ),
    coverImages = Nil
  )

  val instance: Helium = new Helium(defaultSiteSettings, defaultEPUBSettings, defaultPDFSettings)
  
}
