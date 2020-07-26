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
                      favIcon32x32: Option[Path] = None)

case class PDFLayout (pageWidth: Size, pageHeight: Size,
                      marginTop: Size, marginRight: Size, marginBottom: Size, marginLeft: Size,
                      defaultBlockSpacing: Size, defaultLineHeight: Double,
                      keepTogetherDecoratedLines: Int)

case class LandingPage (logo: Option[Image] = None,
                        title: Option[String] = None,
                        subtitle: Option[String] = None,
                        latestReleases: Seq[ReleaseInfo] = Nil,
                        license: Option[String] = None,
                        documentationLinks: Seq[LandingPageLink] = Nil,
                        projectLinks: Seq[LandingPageLink] = Nil,
                        teasers: Seq[Teaser] = Nil)

case class ReleaseInfo (title: String, version: String)
sealed trait LandingPageLink {
  def text: String
}
case class ExternalLink (text: String, target: String) extends LandingPageLink
case class InternalLink (text: String, target: Path) extends LandingPageLink

case class Teaser (title: String, description: String)

sealed trait AnchorPlacement

object AnchorPlacement {
  object None extends AnchorPlacement
  object Left extends AnchorPlacement
  object Right extends AnchorPlacement
}
