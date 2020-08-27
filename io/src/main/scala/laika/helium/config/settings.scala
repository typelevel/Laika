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

import java.time.Instant
import java.util.Date

import laika.ast.Path.Root
import laika.ast.{DocumentMetadata, Image, Path, Size}
import laika.helium.Helium
import laika.rewrite.nav.CoverImage
import laika.theme.config.{BookConfig, Color, FontDefinition}

private[helium] case class SiteSettings (fontResources: Seq[FontDefinition],
                                         themeFonts: ThemeFonts,
                                         fontSizes: FontSizes,
                                         colors: ColorSet,
                                         htmlIncludes: HTMLIncludes,
                                         landingPage: Option[LandingPage],
                                         layout: WebLayout,
                                         metadata: DocumentMetadata)

private[helium] case class PDFSettings (bookConfig: BookConfig,
                                        themeFonts: ThemeFonts,
                                        fontSizes: FontSizes,
                                        colors: ColorSet,
                                        layout: PDFLayout,
                                        coverImages: Seq[CoverImage])

private[helium] case class EPUBSettings (bookConfig: BookConfig,
                                         themeFonts: ThemeFonts,
                                         fontSizes: FontSizes,
                                         colors: ColorSet,
                                         htmlIncludes: HTMLIncludes,
                                         coverImages: Seq[CoverImage])

private[helium] trait CommonConfigOps {

  def fontResources (defn: FontDefinition*): Helium
  def fontFamilies (body: String, headlines: String, code: String): Helium
  def fontSizes (body: Size,
                 code: Size,
                 title: Size,
                 header2: Size,
                 header3: Size,
                 header4: Size,
                 small: Size): Helium
  def themeColors (primary: Color,
                   primaryDark: Color,
                   primaryLight: Color,
                   secondary: Color): Helium
  def messageColors (info: Color,
                     infoLight: Color,
                     warning: Color,
                     warningLight: Color,
                     error: Color,
                     errorLight: Color): Helium
  def syntaxHighlightingColors (base: ColorQuintet, wheel: ColorQuintet): Helium

  def metadata (title: Option[String] = None,
                description: Option[String] = None,
                identifier: Option[String] = None,
                authors: Seq[String] = Nil,
                language: Option[String] = None,
                date: Option[Instant] = None,
                version: Option[String] = None): Helium
}

private[helium] trait SingleConfigOps extends CommonConfigOps {
  protected def currentColors: ColorSet
  protected def withFontFamilies (fonts: ThemeFonts): Helium
  protected def withFontSizes (sizes: FontSizes): Helium
  protected def withColors (colors: ColorSet): Helium
  protected def withMetadata (metadata: DocumentMetadata): Helium

  def fontResources (defn: FontDefinition*): Helium
  def fontFamilies (body: String, headlines: String, code: String): Helium =
    withFontFamilies(ThemeFonts(body, headlines, code))
  def fontSizes (body: Size,
                 code: Size,
                 title: Size,
                 header2: Size,
                 header3: Size,
                 header4: Size,
                 small: Size): Helium =
    withFontSizes(FontSizes(body, code, title, header2, header3, header4, small))
  def themeColors (primary: Color,
                   primaryDark: Color,
                   primaryLight: Color,
                   secondary: Color): Helium = withColors(currentColors.copy(
    primary = primary, primaryDark = primaryDark, primaryLight = primaryLight, secondary = secondary
  ))
  def messageColors (info: Color,
                     infoLight: Color,
                     warning: Color,
                     warningLight: Color,
                     error: Color,
                     errorLight: Color): Helium = withColors(currentColors.copy(messages =
    MessageColors(info, infoLight, warning, warningLight, error, errorLight)
  ))
  def syntaxHighlightingColors (base: ColorQuintet, wheel: ColorQuintet): Helium =
    withColors(currentColors.copy(syntaxHighlighting =
      SyntaxColors(base, wheel)
    ))
  def metadata (title: Option[String] = None,
                description: Option[String] = None,
                identifier: Option[String] = None,
                authors: Seq[String] = Nil,
                language: Option[String] = None,
                date: Option[Instant] = None,
                version: Option[String] = None): Helium =
    withMetadata(DocumentMetadata(title, description, identifier, authors, language, date.map(Date.from), version))
}

private[helium] trait AllFormatsOps extends CommonConfigOps {
  protected def helium: Helium
  
  private val formats: Seq[Helium => CommonConfigOps] = Seq(_.site, _.epub, _.pdf)
  def fontResources (defn: FontDefinition*): Helium = formats.foldLeft(helium) {
    case (helium, format) => format(helium).fontResources(defn:_*)
  }
  def fontFamilies (body: String, headlines: String, code: String): Helium = formats.foldLeft(helium) {
    case (helium, format) => format(helium).fontFamilies(body, headlines, code)
  }
  def fontSizes (body: Size,
                 code: Size,
                 title: Size,
                 header2: Size,
                 header3: Size,
                 header4: Size,
                 small: Size): Helium = formats.foldLeft(helium) {
    case (helium, format) => format(helium).fontSizes(body, code, title, header2, header3, header4, small)
  }
  def themeColors (primary: Color,
                   primaryDark: Color,
                   primaryLight: Color,
                   secondary: Color): Helium = formats.foldLeft(helium) {
    case (helium, format) =>
      format(helium).themeColors(primary, primaryDark, primaryLight, secondary)
  }
  def messageColors (info: Color,
                     infoLight: Color,
                     warning: Color,
                     warningLight: Color,
                     error: Color,
                     errorLight: Color): Helium = formats.foldLeft(helium) {
    case (helium, format) =>
      format(helium).messageColors(info, infoLight, warning, warningLight, error, errorLight)
  }
  def syntaxHighlightingColors (base: ColorQuintet, wheel: ColorQuintet): Helium = formats.foldLeft(helium) {
    case (helium, format) =>
      format(helium).syntaxHighlightingColors(base, wheel)
  }
  def metadata (title: Option[String] = None,
                description: Option[String] = None,
                identifier: Option[String] = None,
                authors: Seq[String] = Nil,
                language: Option[String] = None,
                date: Option[Instant] = None,
                version: Option[String] = None): Helium = formats.foldLeft(helium) {
    case (helium, format) =>
      format(helium).metadata(title, description, identifier, authors, language, date, version)
  }
}

private[helium] trait CopyOps {
  protected def helium: Helium

  def copyWith (siteSettings: SiteSettings): Helium = new Helium(siteSettings, helium.epubSettings, helium.pdfSettings)
  def copyWith (epubSettings: EPUBSettings): Helium = new Helium(helium.siteSettings, epubSettings, helium.pdfSettings)
  def copyWith (pdfSettings: PDFSettings): Helium   = new Helium(helium.siteSettings, helium.epubSettings, pdfSettings)
}

private[helium] trait SiteOps extends SingleConfigOps with CopyOps {
  protected def currentColors: ColorSet = helium.siteSettings.colors
  def fontResources (defn: FontDefinition*): Helium =  copyWith(helium.siteSettings.copy(fontResources = defn))
  protected def withFontFamilies (fonts: ThemeFonts): Helium = copyWith(helium.siteSettings.copy(themeFonts = fonts))
  protected def withFontSizes (sizes: FontSizes): Helium = copyWith(helium.siteSettings.copy(fontSizes = sizes))
  protected def withColors (colors: ColorSet): Helium = copyWith(helium.siteSettings.copy(colors = colors))
  protected def withMetadata (metadata: DocumentMetadata): Helium = copyWith(helium.siteSettings.copy(metadata = metadata))

  def autoLinkCSS (paths: Path*): Helium =
    copyWith(helium.siteSettings.copy(htmlIncludes = helium.siteSettings.htmlIncludes.copy(includeCSS = paths)))
  def autoLinkJS (paths: Path*): Helium =
    copyWith(helium.siteSettings.copy(htmlIncludes = helium.siteSettings.htmlIncludes.copy(includeJS = paths)))

  def layout (contentWidth: Size,
              navigationWidth: Size,
              defaultBlockSpacing: Size,
              defaultLineHeight: Double,
              anchorPlacement: AnchorPlacement): Helium = {
    val layout = helium.siteSettings.layout.copy(
      contentWidth = contentWidth,
      navigationWidth = navigationWidth,
      defaultBlockSpacing = defaultBlockSpacing,
      defaultLineHeight = defaultLineHeight,
      anchorPlacement = anchorPlacement
    )
    copyWith(helium.siteSettings.copy(layout = layout))
  }

  def favIcons (icons: Favicon*): Helium = {
    val newLayout = helium.siteSettings.layout.copy(favIcons = icons)
    copyWith(helium.siteSettings.copy(layout = newLayout))
  }
  def topNavigationBar (logo: Option[Image], links: Seq[ThemeLink]): Helium = {
    val newLayout = helium.siteSettings.layout.copy(topNavigationBar = TopNavigationBar(logo, links))
    copyWith(helium.siteSettings.copy(layout = newLayout))
  }
  def tableOfContent (title: String, depth: Int): Helium = {
    val newLayout = helium.siteSettings.layout.copy(tableOfContent = Some(TableOfContent(title, depth)))
    copyWith(helium.siteSettings.copy(layout = newLayout))
  }
  def downloadPage (title: String, description: Option[String], downloadPath: Path = Root / "downloads",
                    includeEPUB: Boolean = true, includePDF: Boolean = true): Helium = {
    val newLayout = helium.siteSettings.layout
      .copy(downloadPage = Some(DownloadPage(title, description, downloadPath, includeEPUB, includePDF)))
    copyWith(helium.siteSettings.copy(layout = newLayout))
  }
  def markupEditLinks (text: String, baseURL: String): Helium = {
    val newLayout = helium.siteSettings.layout.copy(markupEditLinks = Some(MarkupEditLinks(text, baseURL)))
    copyWith(helium.siteSettings.copy(layout = newLayout))
  }

  def landingPage (logo: Option[Image] = None,
                   title: Option[String] = None,
                   subtitle: Option[String] = None,
                   latestReleases: Seq[ReleaseInfo] = Nil,
                   license: Option[String] = None,
                   documentationLinks: Seq[TextLink] = Nil,
                   projectLinks: Seq[ThemeLink] = Nil,
                   teasers: Seq[Teaser] = Nil): Helium = {
    val page = LandingPage(logo, title, subtitle, latestReleases, license, documentationLinks, projectLinks, teasers)
    copyWith(helium.siteSettings.copy(landingPage = Some(page)))
  }
}

private[helium] trait EPUBOps extends SingleConfigOps with CopyOps {
  protected def currentColors: ColorSet = helium.epubSettings.colors
  
  def fontResources (defn: FontDefinition*): Helium =
    copyWith(helium.epubSettings.copy(bookConfig = helium.epubSettings.bookConfig.copy(fonts = defn)))
  protected def withFontFamilies (fonts: ThemeFonts): Helium =
    copyWith(helium.epubSettings.copy(themeFonts = fonts))
  protected def withFontSizes (sizes: FontSizes): Helium =
    copyWith(helium.epubSettings.copy(fontSizes = sizes))
  protected def withColors (colors: ColorSet): Helium =
    copyWith(helium.epubSettings.copy(colors = colors))
  protected def withMetadata (metadata: DocumentMetadata): Helium =
    copyWith(helium.epubSettings.copy(bookConfig = helium.epubSettings.bookConfig.copy(metadata = metadata)))

  def navigationDepth (depth: Int): Helium =
    copyWith(helium.epubSettings.copy(bookConfig = helium.epubSettings.bookConfig.copy(navigationDepth = Some(depth))))
  
  def autoLinkCSS (paths: Path*): Helium =
    copyWith(helium.epubSettings.copy(htmlIncludes = helium.epubSettings.htmlIncludes.copy(includeCSS = paths)))
  def autoLinkJS (paths: Path*): Helium =
    copyWith(helium.epubSettings.copy(htmlIncludes = helium.epubSettings.htmlIncludes.copy(includeJS = paths)))
  def coverImages (images: CoverImage*): Helium =
    copyWith(helium.epubSettings.copy(coverImages = images))
}

private[helium] trait PDFOps extends SingleConfigOps with CopyOps {
  protected def currentColors: ColorSet = helium.pdfSettings.colors
  
  def fontResources (defn: FontDefinition*): Helium =
    copyWith(helium.pdfSettings.copy(bookConfig = helium.pdfSettings.bookConfig.copy(fonts = defn)))
  protected def withFontFamilies (fonts: ThemeFonts): Helium = copyWith(helium.pdfSettings.copy(themeFonts = fonts))
  protected def withFontSizes (sizes: FontSizes): Helium = copyWith(helium.pdfSettings.copy(fontSizes = sizes))
  protected def withColors (colors: ColorSet): Helium = copyWith(helium.pdfSettings.copy(colors = colors))
  protected def withMetadata (metadata: DocumentMetadata): Helium =
    copyWith(helium.pdfSettings.copy(bookConfig = helium.pdfSettings.bookConfig.copy(metadata = metadata)))

  def navigationDepth (depth: Int): Helium =
    copyWith(helium.pdfSettings.copy(bookConfig = helium.pdfSettings.bookConfig.copy(navigationDepth = Some(depth))))
  
  def layout (pageWidth: Size, pageHeight: Size,
              marginTop: Size, marginRight: Size, marginBottom: Size, marginLeft: Size,
              defaultBlockSpacing: Size, defaultLineHeight: Double,
              keepTogetherDecoratedLines: Int,
              navigationDepth: Int): Helium =
    copyWith(helium.pdfSettings.copy(layout = PDFLayout(
      pageWidth, pageHeight, marginTop, marginRight, marginBottom, marginLeft,
      defaultBlockSpacing, defaultLineHeight, keepTogetherDecoratedLines
    )))
  def tableOfContent (title: String, depth: Int): Helium = {
    val newLayout = helium.pdfSettings.layout.copy(tableOfContent = Some(TableOfContent(title, depth)))
    copyWith(helium.pdfSettings.copy(layout = newLayout))
  }
  def coverImages (images: CoverImage*): Helium =
    copyWith(helium.pdfSettings.copy(coverImages = images))
}
