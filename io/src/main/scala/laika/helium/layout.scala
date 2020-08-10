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

import laika.ast.{Image, Path, Size}

case class WebLayout (contentWidth: Size, 
                      navigationWidth: Size, 
                      defaultBlockSpacing: Size, 
                      defaultLineHeight: Double,
                      anchorPlacement: AnchorPlacement,
                      favIcons: Seq[Favicon] = Nil,
                      topNavigationBar: TopNavigationBar = TopNavigationBar.default,
                      tableOfContent: Option[TableOfContent] = None,
                      downloadPage: Option[DownloadPage] = None)

case class PDFLayout (pageWidth: Size, pageHeight: Size,
                      marginTop: Size, marginRight: Size, marginBottom: Size, marginLeft: Size,
                      defaultBlockSpacing: Size, defaultLineHeight: Double,
                      keepTogetherDecoratedLines: Int,
                      tableOfContent: Option[TableOfContent] = None)

case class TableOfContent (title: String, depth: Int)

case class TopNavigationBar (logo: Option[Image], links: Seq[ThemeLink])

object TopNavigationBar {
  val default: TopNavigationBar = TopNavigationBar(None, Nil) // TODO - use home icon instead of image if empty
}

case class DownloadPage (title: String, description: Option[String], includeEPUB: Boolean = true, includePDF: Boolean = true)

case class LandingPage (logo: Option[Image] = None,
                        title: Option[String] = None,
                        subtitle: Option[String] = None,
                        latestReleases: Seq[ReleaseInfo] = Nil,
                        license: Option[String] = None,
                        documentationLinks: Seq[TextLink] = Nil,
                        projectLinks: Seq[ThemeLink] = Nil,
                        teasers: Seq[Teaser] = Nil)

case class Favicon (target: ThemeTarget, sizes: Option[String], mediaType: Option[String])

object Favicon {
  private def mediaType (suffix: Option[String]): Option[String] = suffix.collect {
    case "ico" => "image/x-icon"
    case "png" => "image/png"
    case "gif" => "image/gif"
    case "jpg" | "jpeg" => "image/jpeg"
    case "svg" => "image/svg+xml"
  }
  def external (url: String, sizes: String, mediaType: String): Favicon = 
    Favicon(ThemeTarget.external(url), Some(sizes), Some(mediaType))
  def internal (path: Path, sizes: String): Favicon = 
    Favicon(ThemeTarget.internal(path), Some(sizes), mediaType(path.suffix))
}

case class ReleaseInfo (title: String, version: String)

case class Teaser (title: String, description: String)

sealed trait AnchorPlacement

object AnchorPlacement {
  object None extends AnchorPlacement
  object Left extends AnchorPlacement
  object Right extends AnchorPlacement
}
