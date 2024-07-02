package laika.helium.internal.config

import laika.config.{ CoverImage, Versions }
import laika.theme.config.{
  BookConfig,
  DocumentMetadata,
  FontDefinition,
  IncludeCSSConfig,
  IncludeJSConfig
}

private[helium] trait CommonSettings {
  def themeFonts: ThemeFonts
  def fontSizes: FontSizes
  def colors: ColorSet
  def metadata: DocumentMetadata
  def layout: CommonLayout
}

private[helium] trait DarkModeSupport extends CommonSettings {
  def darkMode: Option[ColorSet]
}

private[helium] case class SiteSettings(
    fontResources: Seq[FontDefinition],
    themeFonts: ThemeFonts,
    fontSizes: FontSizes,
    colors: ColorSet,
    darkMode: Option[ColorSet],
    layout: WebLayout,
    content: WebContent,
    metadata: DocumentMetadata,
    versions: Option[Versions] = None,
    baseURL: Option[String] = None
) extends DarkModeSupport

private[helium] case class PDFSettings(
    bookConfig: BookConfig,
    themeFonts: ThemeFonts,
    fontSizes: FontSizes,
    colors: ColorSet,
    layout: PDFLayout,
    coverImages: Seq[CoverImage]
) extends CommonSettings {
  val metadata: DocumentMetadata = bookConfig.metadata
}

private[helium] case class EPUBSettings(
    bookConfig: BookConfig,
    themeFonts: ThemeFonts,
    fontSizes: FontSizes,
    colors: ColorSet,
    darkMode: Option[ColorSet],
    styleIncludes: IncludeCSSConfig = IncludeCSSConfig.empty,
    scriptIncludes: IncludeJSConfig = IncludeJSConfig.empty,
    layout: EPUBLayout,
    coverImages: Seq[CoverImage]
) extends DarkModeSupport {
  val metadata: DocumentMetadata = bookConfig.metadata
}
