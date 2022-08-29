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

import laika.ast.Path.Root
import laika.ast.{ExternalTarget, Image, InternalTarget, Length, Options, Path, SpanLink, Styles, Target}

private[helium] sealed trait CommonLayout {
  def defaultBlockSpacing: Length
  def defaultLineHeight: Double
  def tableOfContent: Option[TableOfContent]
}

private[helium] case class WebLayout(contentWidth: Length,
                                     navigationWidth: Length,
                                     topBarHeight: Length,
                                     defaultBlockSpacing: Length,
                                     defaultLineHeight: Double,
                                     anchorPlacement: AnchorPlacement,
                                     favIcons: Seq[Favicon] = Nil,
                                     topNavigationBar: TopNavigationBar = TopNavigationBar.default,
                                     tableOfContent: Option[TableOfContent] = None,
                                     downloadPage: Option[DownloadPage] = None,
                                     markupEditLinks: Option[MarkupEditLinks] = None) extends CommonLayout

private[helium] case class PDFLayout (pageWidth: Length, pageHeight: Length,
                                      marginTop: Length, marginRight: Length, marginBottom: Length, marginLeft: Length,
                                      defaultBlockSpacing: Length,
                                      defaultLineHeight: Double,
                                      keepTogetherDecoratedLines: Int,
                                      tableOfContent: Option[TableOfContent] = None) extends CommonLayout

private[helium] case class EPUBLayout (defaultBlockSpacing: Length,
                                       defaultLineHeight: Double,
                                       keepTogetherDecoratedLines: Int,
                                       tableOfContent: Option[TableOfContent] = None) extends CommonLayout

private[helium] case class TableOfContent (title: String, depth: Int)

private[helium] case class TopNavigationBar (homeLink: ThemeLink, 
                                             navLinks: Seq[ThemeLink],
                                             versionMenu: VersionMenu = VersionMenu.default,
                                             highContrast: Boolean = false)

private[helium] object TopNavigationBar {
  def withHomeLink (path: Path): TopNavigationBar = TopNavigationBar(IconLink.internal(path, HeliumIcon.home), Nil)
  val default: TopNavigationBar = withHomeLink(Root / "README.md")
}

private[helium] case class DownloadPage (title: String, description: Option[String], downloadPath: Path = Root / "downloads", 
                                        includeEPUB: Boolean = true, includePDF: Boolean = true)

private[helium] case class LandingPage (logo: Option[Image] = None,
                                        title: Option[String] = None,
                                        subtitle: Option[String] = None,
                                        latestReleases: Seq[ReleaseInfo] = Nil,
                                        license: Option[String] = None,
                                        documentationLinks: Seq[TextLink] = Nil,
                                        projectLinks: Seq[ThemeLinkSpan] = Nil,
                                        teasers: Seq[Teaser] = Nil)

private[helium] case class MarkupEditLinks (text: String, baseURL: String)

/** Configuration for a single favicon which can be an internal resource or an external URL.
  * 
  * The sizes string will be used in the corresponding `sizes` attribute of the generated `&lt;link&gt;` tag.
  */
case class Favicon private (target: Target, sizes: Option[String] = None, mediaType: Option[String] = None)

/** Companion for creating Favicon configuration instances.
  */
object Favicon {
  private def mediaType (suffix: Option[String]): Option[String] = suffix.collect {
    case "ico" => "image/x-icon"
    case "png" => "image/png"
    case "gif" => "image/gif"
    case "jpg" | "jpeg" => "image/jpeg"
    case "svg" => "image/svg+xml"
  }

  /** Creates the configuration for a single favicon with an external URL.
    *
    * The sizes string will be used in the corresponding `sizes` attribute of the generated `&lt;link&gt;` tag.
    */
  def external (url: String, sizes: String, mediaType: String): Favicon = 
    Favicon(ExternalTarget(url), Some(sizes), Some(mediaType))

  /** Creates the configuration for a single favicon based on an internal resource and its virtual path.
    * This resource must be part of the inputs known to Laika.
    *
    * The sizes string will be used in the corresponding `sizes` attribute of the generated `&lt;link&gt;` tag.
    */
  def internal (path: Path, sizes: String): Favicon = 
    Favicon(InternalTarget(path), Some(sizes), mediaType(path.suffix))
}

/** Represents release info to be displayed on the landing page.
  * 
  * This is specific for sites that serve as documentation for software projects.
  * 
  * @param title the header above the version number, e.g. "Latest Stable Release"
  * @param version the version number of the release
  */
case class ReleaseInfo (title: String, version: String)

/** Represents a single teaser block to be displayed on the landing page.
  * Any number of these blocks can be passed to the Helium configuration.
  */
case class Teaser (title: String, description: String)

/** Configures the anchor placement for section headers.
  * Anchors appear on mouse-over and allow to copy the direct link to the section.
  */
sealed trait AnchorPlacement

object AnchorPlacement {
  /** Disables anchors for all section headers. */
  object None extends AnchorPlacement
  /** Places anchors to the left of section headers. */
  object Left extends AnchorPlacement
  /** Places anchors to the right of section headers. */
  object Right extends AnchorPlacement
}

private[helium] case class HTMLIncludes (includeCSS: Seq[Path] = Seq(Root), includeJS: Seq[Path] = Seq(Root))

private[helium] object HeliumStyles {
  val row: Options = Styles("row")
  val linkRow: Options = Styles("row", "links")
  val buttonLink: Options = Styles("button-link")
  val textLink: Options = Styles("text-link")
  val iconLink: Options = Styles("icon-link")
  val imageLink: Options = Styles("image-link")
  val menuToggle: Options = Styles("menu-toggle")
  val menuContainer: Options = Styles("menu-container")
  val menuContent: Options = Styles("menu-content")
  val versionMenu: Options = Styles("version-menu")
}

private[helium] case class ThemeFonts (body: String, headlines: String, code: String)

private[helium] case class FontSizes (body: Length, code: Length, title: Length, header2: Length, header3: Length, header4: Length, small: Length)
