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

import java.io.{InputStream, SequenceInputStream}
import java.util.Date

import cats.data.Kleisli
import cats.effect.{Resource, Sync}
import cats.implicits._
import laika.ast.LengthUnit.{cm, mm, pt, px}
import laika.ast.Path.Root
import laika.ast._
import laika.bundle.{BundleOrigin, ExtensionBundle}
import laika.config.Config
import laika.factory.{Format, TwoPhaseRenderFormat}
import laika.format.HTML
import laika.helium.Helium.{CommonConfigOps, SingleConfigOps}
import laika.helium.generate._
import laika.io.model.{BinaryInput, InputTree, ParsedTree}
import laika.rewrite.DefaultTemplatePath
import laika.theme._

/**
  * @author Jens Halm
  */
class Helium private[laika] (private[laika] val siteSettings: Helium.SiteSettings,
                             private[laika] val epubSettings: Helium.EPUBSettings,
                             private[laika] val pdfSettings: Helium.PDFSettings) { self =>
  
  object site extends SingleConfigOps {
    def fontResources (defn: FontDefinition*): Helium = 
      new Helium(siteSettings.copy(fontResources = defn), epubSettings, pdfSettings)
    protected def currentColors: ColorSet = siteSettings.colors
    protected def withFontFamilies (fonts: ThemeFonts): Helium =
      new Helium(siteSettings.copy(themeFonts = fonts), epubSettings, pdfSettings)
    protected def withFontSizes (sizes: FontSizes): Helium =
      new Helium(siteSettings.copy(fontSizes = sizes), epubSettings, pdfSettings)
    protected def withColors (colors: ColorSet): Helium =
      new Helium(siteSettings.copy(colors = colors), epubSettings, pdfSettings)
    protected def withMetadata (metadata: DocumentMetadata): Helium =
      new Helium(siteSettings.copy(metadata = metadata), epubSettings, pdfSettings)

    def autoLinkCSS (paths: Path*): Helium = 
      new Helium(siteSettings.copy(htmlIncludes = siteSettings.htmlIncludes.copy(includeCSS = paths)), epubSettings, pdfSettings)
    def autoLinkJS (paths: Path*): Helium =
      new Helium(siteSettings.copy(htmlIncludes = siteSettings.htmlIncludes.copy(includeJS = paths)), epubSettings, pdfSettings)
    
    def layout (contentWidth: Size,
                navigationWidth: Size,
                defaultBlockSpacing: Size,
                defaultLineHeight: Double,
                anchorPlacement: AnchorPlacement,
                favIcons: Seq[Favicon] = Nil,
                topNavigationBar: TopNavigationBar = TopNavigationBar.default,
                tableOfContent: Option[TableOfContent] = None,
                downloadPage: Option[DownloadPage] = None,
                markupEditLinks: Option[MarkupEditLinks] = None): Helium = {
      val layout = WebLayout(contentWidth, navigationWidth, defaultBlockSpacing, defaultLineHeight, anchorPlacement,
        favIcons, topNavigationBar, tableOfContent, downloadPage, markupEditLinks)
      new Helium(siteSettings.copy(webLayout = layout), epubSettings, pdfSettings)
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
      new Helium(siteSettings.copy(landingPage = Some(page)), epubSettings, pdfSettings)
    }
  }
  
  object epub extends SingleConfigOps {
    def fontResources (defn: FontDefinition*): Helium =
      new Helium(siteSettings, epubSettings.copy(bookConfig = epubSettings.bookConfig.copy(fonts = defn)), pdfSettings)
    protected def currentColors: ColorSet = epubSettings.colors
    protected def withFontFamilies (fonts: ThemeFonts): Helium =
      new Helium(siteSettings, epubSettings.copy(themeFonts = fonts), pdfSettings)
    protected def withFontSizes (sizes: FontSizes): Helium =
      new Helium(siteSettings, epubSettings.copy(fontSizes = sizes), pdfSettings)
    protected def withColors (colors: ColorSet): Helium =
      new Helium(siteSettings, epubSettings.copy(colors = colors), pdfSettings)
    protected def withMetadata (metadata: DocumentMetadata): Helium =
      new Helium(siteSettings, epubSettings.copy(bookConfig = epubSettings.bookConfig.copy(metadata = metadata)), pdfSettings)
    
    def autoLinkCSS (paths: Path*): Helium =
      new Helium(siteSettings, epubSettings.copy(htmlIncludes = epubSettings.htmlIncludes.copy(includeCSS = paths)), pdfSettings)
    def autoLinkJS (paths: Path*): Helium = 
      new Helium(siteSettings, epubSettings.copy(htmlIncludes = epubSettings.htmlIncludes.copy(includeJS = paths)), pdfSettings)
  }
  
  object pdf extends SingleConfigOps {
    def fontResources (defn: FontDefinition*): Helium =
      new Helium(siteSettings, epubSettings, pdfSettings.copy(bookConfig = pdfSettings.bookConfig.copy(fonts = defn)))
    protected def currentColors: ColorSet = pdfSettings.colors
    protected def withFontFamilies (fonts: ThemeFonts): Helium =
      new Helium(siteSettings, epubSettings, pdfSettings.copy(themeFonts = fonts))
    protected def withFontSizes (sizes: FontSizes): Helium =
      new Helium(siteSettings, epubSettings, pdfSettings.copy(fontSizes = sizes))
    protected def withColors (colors: ColorSet): Helium =
      new Helium(siteSettings, epubSettings, pdfSettings.copy(colors = colors))
    protected def withMetadata (metadata: DocumentMetadata): Helium =
      new Helium(siteSettings, epubSettings, pdfSettings.copy(bookConfig = pdfSettings.bookConfig.copy(metadata = metadata)))
    
    def layout (pageWidth: Size, pageHeight: Size,
                marginTop: Size, marginRight: Size, marginBottom: Size, marginLeft: Size,
                defaultBlockSpacing: Size, defaultLineHeight: Double,
                keepTogetherDecoratedLines: Int,
                navigationDepth: Int,
                tableOfContent: Option[TableOfContent] = None,
                coverImage: Option[Path] = None): Helium =
      new Helium(siteSettings, epubSettings, pdfSettings.copy(pdfLayout = PDFLayout(
        pageWidth, pageHeight, marginTop, marginRight, marginBottom, marginLeft,
        defaultBlockSpacing, defaultLineHeight, keepTogetherDecoratedLines, tableOfContent
      )))
  }

  // TODO - extract
  object allFormats extends CommonConfigOps {
    private val formats: Seq[Helium => CommonConfigOps] = Seq(_.site, _.epub, _.pdf)
    def fontResources (defn: FontDefinition*): Helium = formats.foldLeft(self) {
      case (helium, format) => format(helium).fontResources(defn:_*)
    }
    def fontFamilies (body: String, headlines: String, code: String): Helium = formats.foldLeft(self) {
      case (helium, format) => format(helium).fontFamilies(body, headlines, code)
    }
    def fontSizes (body: Size,
                   code: Size,
                   title: Size,
                   header2: Size,
                   header3: Size,
                   header4: Size,
                   small: Size): Helium = formats.foldLeft(self) {
      case (helium, format) => format(helium).fontSizes(body, code, title, header2, header3, header4, small)
    }
    def themeColors (primary: Color,
                  primaryDark: Color,
                  primaryLight: Color,
                  secondary: Color): Helium = formats.foldLeft(self) {
      case (helium, format) => 
        format(helium).themeColors(primary, primaryDark, primaryLight, secondary)
    }
    def messageColors (info: Color,
                       infoLight: Color,
                       warning: Color,
                       warningLight: Color,
                       error: Color,
                       errorLight: Color): Helium = formats.foldLeft(self) {
      case (helium, format) =>
        format(helium).messageColors(info, infoLight, warning, warningLight, error, errorLight)
    }
    def syntaxHighlightingColors (base: ColorQuintet, wheel: ColorQuintet): Helium = formats.foldLeft(self) {
      case (helium, format) =>
        format(helium).syntaxHighlightingColors(base, wheel)
    }
    def metadata (title: Option[String] = None,
                  description: Option[String] = None,
                  identifier: Option[String] = None,
                  authors: Seq[String] = Nil,
                  language: Option[String] = None,
                  date: Option[Date] = None,
                  version: Option[String] = None): Helium = formats.foldLeft(self) {
      case (helium, format) =>
        format(helium).metadata(title, description, identifier, authors, language, date, version)
    }
  }
  
  def build[F[_]: Sync]: Resource[F, Theme[F]] = {

    type TreeProcessor = Kleisli[F, ParsedTree[F], ParsedTree[F]]
    
    val noOp: TreeProcessor = Kleisli.ask[F, ParsedTree[F]]
    
    val fontResources = (siteSettings.fontResources ++ epubSettings.bookConfig.fonts ++ pdfSettings.bookConfig.fonts)
      .flatMap(_.resource.embedResource).distinct

    val fontInputs = fontResources.foldLeft(InputTree[F]) { case (tree, embedResource) =>
      embedResource match {
        case res: EmbeddedFontFile     => tree.addFile(res.file, res.path)
        case res: EmbeddedFontResource => tree.addClasspathResource(res.name, res.path)
      }
    }

    val themeInputs = fontInputs
      .addTemplate(TemplateDocument(DefaultTemplatePath.forEPUB, EPUBTemplate.default))
      .addClasspathResource("laika/helium/templates/default.template.html", DefaultTemplatePath.forHTML)
      .addClasspathResource("laika/helium/templates/landing.template.html", Root / "landing.template.html")
      .addClasspathResource("laika/helium/templates/default.template.fo", DefaultTemplatePath.forFO)
      .addClasspathResource("laika/helium/css/container.css", Root / "css" / "container.css")
      .addClasspathResource("laika/helium/css/content.css", Root / "css" / "content.css")
      .addClasspathResource("laika/helium/css/nav.css", Root / "css" / "nav.css")
      .addClasspathResource("laika/helium/css/code.css", Root / "css" / "code.css")
      .addClasspathResource("laika/helium/css/toc.css", Root / "css" / "toc.css")
      .addClasspathResource("laika/helium/js/theme.js", Root / "laika" / "helium.js")
      .addString(new FOStyles(this).input , FOStyles.defaultPath)
      .addString(CSSVarGenerator.generate(this), Root / "css" / "vars.css")

    def estimateLines (blocks: Seq[Block]): Int = blocks.collect {
      case sp: SpanContainer => sp.extractText.count(_ == '\n')
      case bc: BlockContainer => estimateLines(bc.content) // TODO - handle lists and tables
    }.sum

    val rewriteRule: RewriteRules = RewriteRules.forBlocks {
      case cb: CodeBlock if cb.extractText.count(_ == '\n') <= pdfSettings.pdfLayout.keepTogetherDecoratedLines =>
        Replace(cb.mergeOptions(Style.keepTogether))
      case bs: BlockSequence if bs.options.styles.contains("callout") && estimateLines(bs.content) <= pdfSettings.pdfLayout.keepTogetherDecoratedLines =>
        Replace(bs.mergeOptions(Style.keepTogether))
    }

    val bundle: ExtensionBundle = new ExtensionBundle {
      override val origin: BundleOrigin = BundleOrigin.Theme
      val description = "Helium Theme Rewrite Rules and Render Overrides"
      override val rewriteRules: Seq[DocumentCursor => RewriteRules] = Seq(_ => rewriteRule)
      override val renderOverrides = Seq(HTML.Overrides(HeliumRenderOverrides.create(siteSettings.webLayout.anchorPlacement)))
      override val baseConfig: Config = ConfigGenerator.populateConfig(self)
    }

    def addDownloadPage: TreeProcessor = siteSettings.webLayout.downloadPage
      .filter(p => p.includeEPUB || p.includePDF)
      .fold(noOp)(DownloadPageGenerator.generate)

    def mergeCSS: TreeProcessor = Kleisli { tree =>
      val css = Root / "css"
      val webCSS = IndexedSeq(css / "vars.css", css / "container.css", css / "content.css", css / "nav.css", css / "code.css", css / "toc.css")
      val (cssDocs, otherDocs) = tree.staticDocuments.partition(doc => webCSS.contains(doc.path))
      // TODO - 0.16 - merge just once upfront as soon as Theme is a Resource itself
      def mergedInput: InputStream = {
        val cssInputs = cssDocs.toList.sortBy(in => webCSS.indexOf(in.path))
        val iter = cssInputs.iterator
        val enum = new java.util.Enumeration[InputStream] {
          def hasMoreElements = iter.hasNext
          def nextElement() = iter.next().stream()
        }
        new SequenceInputStream(enum)
      }
      val newTree = tree.replaceStaticDocuments(otherDocs :+ BinaryInput(css / "laika-helium.css", () => mergedInput))
      Sync[F].pure(newTree)
    }

    def filterFonts (format: Format): TreeProcessor = format match {
      case _: TwoPhaseRenderFormat[_,_] => noOp
      case _ => Kleisli { tree: ParsedTree[F] =>
        val filteredOther = tree.staticDocuments.filterNot(_.path.isSubPath(Root / "laika" / "fonts"))
        Sync[F].pure(tree.copy(staticDocuments = filteredOther))
      }
    }

    Theme(themeInputs, bundle).processTree {
      case HTML => addDownloadPage
        .andThen(TocPageGenerator.generate(self, HTML))
        .andThen(siteSettings.landingPage.fold(noOp)(LandingPageGenerator.generate))
        .andThen(mergeCSS)
        .andThen(filterFonts(HTML)): TreeProcessor
      case format => TocPageGenerator.generate(self, format).andThen(filterFonts(format)): TreeProcessor
    }.build
    
  } 
  
}

object Helium {
  
//  trait CommonSettings {
//    def fontResources: Seq[FontDefinition]
//    def themeFonts: ThemeFonts
//    def fontSizes: FontSizes
//    def colors: ColorSet
//  }
  
  trait CommonConfigOps {
    
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
                  date: Option[Date] = None,
                  version: Option[String] = None): Helium
  }
  
  trait SingleConfigOps extends CommonConfigOps {
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
      primary = primary, primaryDark = primaryDark, primaryLight = primaryLight
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
                  date: Option[Date] = None,
                  version: Option[String] = None): Helium =
      withMetadata(DocumentMetadata(title, description, identifier, authors, language, date, version))
  }
  

  case class SiteSettings (fontResources: Seq[FontDefinition],
                           themeFonts: ThemeFonts,
                           fontSizes: FontSizes,
                           colors: ColorSet,
                           htmlIncludes: HTMLIncludes,
                           landingPage: Option[LandingPage],
                           webLayout: WebLayout,
                           metadata: DocumentMetadata)
  
  case class PDFSettings (bookConfig: BookConfig,
                          themeFonts: ThemeFonts,
                          fontSizes: FontSizes,
                          colors: ColorSet,
                          pdfLayout: PDFLayout)
  
  case class EPUBSettings (bookConfig: BookConfig,
                           themeFonts: ThemeFonts,
                           fontSizes: FontSizes,
                           colors: ColorSet,
                           htmlIncludes: HTMLIncludes)
  
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
  // TODO - extract into HeliumDefaults
  
  val defaults: Helium = new Helium(defaultSiteSettings, defaultEPUBSettings, defaultPDFSettings)
    
}

object HeliumStyles {
  val button: Options = Styles("button")
}

object HeliumIcon {
  private val options = Styles("icofont-laika")
  val navigationMenu: Icon = Icon('\uefa2', options)
  val home: Icon = Icon('\uef47', options)
  val link: Icon = Icon('\uef71', options)
  val close: Icon = Icon('\ueedd', options)
  val check: Icon = Icon('\ueed7', options)
  val chat: Icon = Icon('\ueed5', options)
  val settings: Icon = Icon('\ueed5', options)
  val edit: Icon = Icon('\uef10', options)
  val demo: Icon = Icon('\ueeea', options)
  val download: Icon = Icon('\uef08', options)
  val info: Icon = Icon('\uef4e', options)
  val warning: Icon = Icon('\uefb0', options)
  val error: Icon = Icon('\ueedd', options)
  val twitter: Icon = Icon('\ued7a', options)
}
