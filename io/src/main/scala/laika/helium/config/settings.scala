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

  /** Adds one or more font resources to the theme, either based on a local classpath or file system resource,
    * or a web font URL, or both.
    *
    * E-book formats like EPUB or PDF require a local font file to be available for embedding.
    * A web font URL can only be used for website generation.
    * 
    * When using this method, all default fonts of the Helium theme will be de-registered.
    */
  def fontResources (defn: FontDefinition*): Helium

  /** Specifies which font family to use for the body text, for headlines
    * and for inline code and code blocks.
    * 
    * All specified fonts need to be made available for the theme first by using the `fontResources` method.
    * The body font needs to be available at least as regular, italic, bold and bold italic.
    * The headline font needs to support bold and bold italic.
    * For the code font a regular font weight is sufficient.
    */
  def fontFamilies (body: String, headlines: String, code: String): Helium

  /** Configure the theme's font sizes.
    * Most property names are self-explanatory, the small font is currently only used for footnote references 
    * in PDF output.
    */
  def fontSizes (body: Size,
                 code: Size,
                 title: Size,
                 header2: Size,
                 header3: Size,
                 header4: Size,
                 small: Size): Helium

  /** Configures the four main colors used by the theme.
    * 
    * @param primary      this color is used for headlines, navigation highlights and other decorative elements
    * @param primaryDark  is supposed to be a darker shade of the primary color and is currently only used for the 
    *                     background gradient of the landing page
    * @param primaryLight is supposed to be a lighter shade of the primary color and is used for the background
    *                     color of sidebars and other decorated blocks; ensure that the text in the `primary` color
    *                     is readable when placed on a `primaryLight` background
    * @param secondary    this color is used for navigation headers and links
    */
  def themeColors (primary: Color,
                   primaryDark: Color,
                   primaryLight: Color,
                   secondary: Color): Helium

  /** Configures the colors of runtime messages embedded in the rendered result.
    * Warnings and errors will only be rendered if you change the configuration to visual debugging,
    * by default the presence of errors will lead to the abortion of the process.
    * See the section "Visual Debugging" on the Configuration page in the manual for details.
    * 
    * @param info          the text color for info-level messages
    * @param infoLight     the background color for info-level messages
    * @param warning       the text color for warning-level messages
    * @param warningLight  the background color for warning-level messages
    * @param error         the text color for error-level messages
    * @param errorLight    the background color for error-level messages
    * @return
    */
  def messageColors (info: Color,
                     infoLight: Color,
                     warning: Color,
                     warningLight: Color,
                     error: Color,
                     errorLight: Color): Helium

  /** Specifies two sets of five colors each to be used by Laika's support for syntax highlighting.
    *
    * If you use the built-in highlighters (which are based on Laika's own parsers) the display is based
    * on a 10-color scheme with 5 base colors which are usually grayish/low saturation and 5 "wheel" colors
    * which are usually placed around the color wheel.
    * 
    * If you use external tools like `highlight.js` these settings will have no effect and you need to use
    * the styling mechanism of that 3rd-party tool.
    */
  def syntaxHighlightingColors (base: ColorQuintet, wheel: ColorQuintet): Helium

  /** Allows to define a small set of metadata that describes the output.
    * 
    * In the generated site it will be used to populate the `<head>` section of the HTML output,
    * for EPUB and PDF it will be used to embed the information into the generated files 
    * in a way that the respective readers understand.
    *
    * When using the sbt plugin the `title`, `description` and `version` metadata will be pre-populated by the standard
    * sbt settings `name`, `description` and `version` respectively. 
    * When using the library API no medata will be defined by default.
    * It is recommended to always define the language and title as the minimum set of metadata.
    * 
    * @param title       the title of the site and/or e-book
    * @param description a short description of the site and/or e-book
    * @param identifier  a unique identifier for the e-book, not used for site generation
    * @param authors     one or more author names
    * @param language    the language of the site and/or e-book, should always be defined
    * @param date        the publication date as a UTC date-time
    * @param version     the version string for the output
    */
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

  protected def copyWith (siteSettings: SiteSettings): Helium = new Helium(siteSettings, helium.epubSettings, helium.pdfSettings)
  protected def copyWith (epubSettings: EPUBSettings): Helium = new Helium(helium.siteSettings, epubSettings, helium.pdfSettings)
  protected def copyWith (pdfSettings: PDFSettings): Helium   = new Helium(helium.siteSettings, helium.epubSettings, pdfSettings)
}

private[helium] trait SiteOps extends SingleConfigOps with CopyOps {
  protected def currentColors: ColorSet = helium.siteSettings.colors
  def fontResources (defn: FontDefinition*): Helium =  copyWith(helium.siteSettings.copy(fontResources = defn))
  protected def withFontFamilies (fonts: ThemeFonts): Helium = copyWith(helium.siteSettings.copy(themeFonts = fonts))
  protected def withFontSizes (sizes: FontSizes): Helium = copyWith(helium.siteSettings.copy(fontSizes = sizes))
  protected def withColors (colors: ColorSet): Helium = copyWith(helium.siteSettings.copy(colors = colors))
  protected def withMetadata (metadata: DocumentMetadata): Helium = copyWith(helium.siteSettings.copy(metadata = metadata))

  /** Auto-links CSS documents from the specified paths.
    * By default all CSS documents found anywhere in the input tree will be linked in HTML files.
    * This setting allows to narrow it down to one or more dedicated paths within the virtual tree,
    * which might be useful when your input contains CSS files unrelated to the pages rendered by Laika.
    */
  def autoLinkCSS (paths: Path*): Helium =
    copyWith(helium.siteSettings.copy(htmlIncludes = helium.siteSettings.htmlIncludes.copy(includeCSS = paths)))

  /** Auto-links JavaScript documents from the specified paths.
    * By default all JavaScript documents found anywhere in the input tree will be linked in HTML files.
    * This setting allows to narrow it down to one or more dedicated paths within the virtual tree,
    * which might be useful when your input contains JavaScript files unrelated to the pages rendered by Laika.
    */
  def autoLinkJS (paths: Path*): Helium =
    copyWith(helium.siteSettings.copy(htmlIncludes = helium.siteSettings.htmlIncludes.copy(includeJS = paths)))

  /** Allows to override the defaults for Helium's layout.
    * You can use the constructors found in the `LengthUnit` companion to create length values,
    * e.g. `LengthUnit.px(12)`.
    * It's usually most convenient to import `laika.ast.LengthUnit._` for your configuration code.
    *  
    * @param contentWidth        the maximum width of the main content column 
    * @param navigationWidth     the width of the left navigation sidebar
    * @param defaultBlockSpacing the default space between block elements
    * @param defaultLineHeight   the default line height
    * @param anchorPlacement     the placement of anchors for copying the links of section headlines (left, right or none)
    */
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

  /** Adds one or more favicons which can be an internal resource or an external URL.
    */
  def favIcons (icons: Favicon*): Helium = {
    val newLayout = helium.siteSettings.layout.copy(favIcons = icons)
    copyWith(helium.siteSettings.copy(layout = newLayout))
  }

  /** Configures the top navigation bar of the main content pages.
    * 
    * @param logo  an optional logo to be placed in the middle of the bar and linking to the landing page,
    *              if omitted a default home icon will be used instead.
    * @param links an optional set of links to be placed at the right side of the bar, supported link
    *              types are `IconLink`, `ButtonLink` and a plain `TextLink`
    */
  def topNavigationBar (logo: Option[Image], links: Seq[ThemeLink]): Helium = {
    val newLayout = helium.siteSettings.layout.copy(topNavigationBar = TopNavigationBar(logo, links))
    copyWith(helium.siteSettings.copy(layout = newLayout))
  }

  /** Adds a dedicated page for a table of content, in addition to the left navigation bar.
    * 
    * @param title the title to display on the page and in navigation that links to the page
    * @param depth the navigation depth which may be different than the one for the navigation bar
    */
  def tableOfContent (title: String, depth: Int): Helium = {
    val newLayout = helium.siteSettings.layout.copy(tableOfContent = Some(TableOfContent(title, depth)))
    copyWith(helium.siteSettings.copy(layout = newLayout))
  }

  /** Adds a download page to the generated site that contains links to EPUB and PDF versions of the site.
    * When the sbt plugin is used, this setting will automatically trigger the rendering of the corresponding
    * EPUB and PDF documents when running `laikaSite`.
    * When the library API is used, this setting only causes the inclusion of the download page itself,
    * the actual EPUB and PDF content must be generated by running the respective renderers manually.
    * 
    * @param title         the title to display on the page and in navigation that links to the page
    * @param description   a short description that appears on the download page right under the title
    * @param downloadPath  the virtual path the EPUB and PDF documents will be generated into
    * @param includeEPUB   whether EPUB documents will be automatically generated (only having an effect when using the sbt plugin)
    * @param includePDF    whether PDF documents will be automatically generated (only having an effect when using the sbt plugin)
    */
  def downloadPage (title: String, description: Option[String], downloadPath: Path = Root / "downloads",
                    includeEPUB: Boolean = true, includePDF: Boolean = true): Helium = {
    val newLayout = helium.siteSettings.layout
      .copy(downloadPage = Some(DownloadPage(title, description, downloadPath, includeEPUB, includePDF)))
    copyWith(helium.siteSettings.copy(layout = newLayout))
  }

  /** Adds a link to the markup source of each page on the bottom of the page navigation pane on the right side.
    *
    * @param text    the text of the link
    * @param baseURL the base URL to prepend to the local path of the rendered document
    */
  def markupEditLinks (text: String, baseURL: String): Helium = {
    val newLayout = helium.siteSettings.layout.copy(markupEditLinks = Some(MarkupEditLinks(text, baseURL)))
    copyWith(helium.siteSettings.copy(layout = newLayout))
  }

  /** Adds a dedicated landing page to the site that is tailored for software documentation sites.
    * By default no landing page will be included and the site will render the homepage (if present)
    * with the same default template as the main content pages.
    * 
    * See the section "Website Landing Page" in the "Theme Settings" chapter for visual guide to the
    * layout of the page.
    * 
    * All arguments of this method are optional, but the page would look strange if you omit most of them.
    * 
    * Additionally or alternatively you can also add a regular markup document called `landing-page.<suffix>` to one
    * of your input directories and its content will be inserted at the bottom of this page.
    * This is in case you want to stick with the traditional approach of some sites, which give you 'Getting Started'
    * style content right on the start page.
    * It can also be used to list adopters, provide a feature overview or links to presentations or videos.
    * 
    * @param logo                a logo to be placed on the left hand side of the header
    * @param title               a title to be placed right under the logo
    * @param subtitle            a subtitle to be place right under the title
    * @param latestReleases      a set of release versions to display on the right side of the header
    * @param license             the license info to render right under the release info
    * @param documentationLinks  a set of documentation links to render in a dedicated panel on the right side of the header
    * @param projectLinks        a set of project links to render at the bottom of the right side of the header
    * @param teasers             a set of teasers containing of headline and description to render below the header
    */
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

  /** The navigation depth of the main navigation structure provided to the EPUB reader.
    * The depth value counts all elements that form the hierarchy, directories, documents and sections within
    * documents.
    */
  def navigationDepth (depth: Int): Helium =
    copyWith(helium.epubSettings.copy(bookConfig = helium.epubSettings.bookConfig.copy(navigationDepth = Some(depth))))

  /** Auto-links CSS documents from the specified paths.
    * By default all CSS documents found anywhere in the input tree will be linked in HTML files.
    * This setting allows to narrow it down to one or more dedicated paths within the virtual tree,
    * which might be useful when your input contains CSS files unrelated to the pages rendered by Laika.
    * 
    * For inclusion in EPUB document a special suffix is expected:
    * 
    * - `.shared.css` for CSS documents to be auto-linked for EPUB and the website
    * - `.epub.css` for CSS document to be auto-linked only for EPUB, but not the website
    */
  def autoLinkCSS (paths: Path*): Helium =
    copyWith(helium.epubSettings.copy(htmlIncludes = helium.epubSettings.htmlIncludes.copy(includeCSS = paths)))

  /** Auto-links JavaScript documents from the specified paths.
    * By default all JavaScript documents found anywhere in the input tree will be linked in HTML files.
    * This setting allows to narrow it down to one or more dedicated paths within the virtual tree,
    * which might be useful when your input contains JavaScript files unrelated to the pages rendered by Laika.
    */
  def autoLinkJS (paths: Path*): Helium =
    copyWith(helium.epubSettings.copy(htmlIncludes = helium.epubSettings.htmlIncludes.copy(includeJS = paths)))

  /** Specifies one or more cover images for the EPUB document.
    * 
    * Multiple cover images are only relevant when the `@:select` directive is used to generate different 
    * versions of the same e-book which can all have their own cover image.
    * See the documentation for the `@:select` directive in the chapter "Standard Directives" in the manual
    * for details.
    */
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

  /** The navigation depth of the main navigation structure provided to the PDF reader.
    * 
    * This is a navigation structure that will usually be displayed in a separate pane by PDF reader software.
    * For including a table of content right in the page flow, so that is also available when printing the document,
    * see the `tableOfContent` method.
    * 
    * The depth value counts all elements that form the hierarchy, directories, documents and sections within
    * documents.
    */
  def navigationDepth (depth: Int): Helium =
    copyWith(helium.pdfSettings.copy(bookConfig = helium.pdfSettings.bookConfig.copy(navigationDepth = Some(depth))))

  /** Allows to override the defaults for Helium's PDF layout.
    * 
    * You can use the constructors found in the `LengthUnit` companion to create length values,
    * e.g. `LengthUnit.px(12)`.
    * It's usually most convenient to import `laika.ast.LengthUnit._` for your configuration code.
    * 
    * Most arguments should be self-explanatory.
    * The `keepTogetherDecoratedLines` value controls the number of lines for decorated blocks like code examples 
    * or callouts that should always be kept on the same page. 
    * With a setting of `12` for example only blocks with more than 12 lines are allowed to be split across multiple pages.
    * If you choose very high numbers for this setting you might see pages with a lot of blank space when it has
    * to move a large block to the next page.
    */
  def layout (pageWidth: Size, pageHeight: Size,
              marginTop: Size, marginRight: Size, marginBottom: Size, marginLeft: Size,
              defaultBlockSpacing: Size, defaultLineHeight: Double,
              keepTogetherDecoratedLines: Int): Helium =
    copyWith(helium.pdfSettings.copy(layout = PDFLayout(
      pageWidth, pageHeight, marginTop, marginRight, marginBottom, marginLeft,
      defaultBlockSpacing, defaultLineHeight, keepTogetherDecoratedLines
    )))

  /** Adds a dedicated page for a table of content, in addition to the reader-native navigation structure.
    *
    * @param title the title to display on the page and in navigation that links to the page
    * @param depth the navigation depth which may be different than the one for the reader-native navigation structure
    */
  def tableOfContent (title: String, depth: Int): Helium = {
    val newLayout = helium.pdfSettings.layout.copy(tableOfContent = Some(TableOfContent(title, depth)))
    copyWith(helium.pdfSettings.copy(layout = newLayout))
  }

  /** Specifies one or more cover images for the PDF document.
    *
    * Multiple cover images are only relevant when the `@:select` directive is used to generate different 
    * versions of the same e-book which can all have their own cover image.
    * See the documentation for the `@:select` directive in the chapter "Standard Directives" in the manual
    * for details.
    */
  def coverImages (images: CoverImage*): Helium =
    copyWith(helium.pdfSettings.copy(coverImages = images))
}
