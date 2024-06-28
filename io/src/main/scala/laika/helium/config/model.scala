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

import laika.ast.*
import laika.theme.config.Color

/** Specifies a set of five colors which is a grouping used in Laika's support for syntax highlighting.
  *
  * If you use the built-in highlighters (which are based on Laika's own parsers) the display is based
  * on a 10-color scheme with 5 base colors which are usually grayish/low saturation and 5 "wheel" colors
  * which are usually placed around the color wheel.
  */
case class ColorQuintet(c1: Color, c2: Color, c3: Color, c4: Color, c5: Color)

/** Configuration for a single favicon which can be an internal resource or an external URL.
  *
  * The sizes string will be used in the corresponding `sizes` attribute of the generated `&lt;link&gt;` tag.
  */
sealed abstract class Favicon private {
  def target: Target

  def sizes: Option[String]

  def mediaType: Option[String]
}

/** Companion for creating Favicon configuration instances.
  */
object Favicon {

  private final case class Impl(
      target: Target,
      sizes: Option[String],
      mediaType: Option[String]
  ) extends Favicon {
    override def productPrefix = "Favicon"
  }

  private def mediaType(suffix: Option[String]): Option[String] = suffix.collect {
    case "ico"          => "image/x-icon"
    case "png"          => "image/png"
    case "gif"          => "image/gif"
    case "jpg" | "jpeg" => "image/jpeg"
    case "svg"          => "image/svg+xml"
  }

  /** Creates the configuration for a single favicon with an external URL.
    *
    * The sizes string will be used in the corresponding `sizes` attribute of the generated `&lt;link&gt;` tag.
    */
  def external(url: String, sizes: String, mediaType: String): Favicon =
    Impl(ExternalTarget(url), Some(sizes), Some(mediaType))

  /** Creates the configuration for a single favicon with an external URL.
    */
  def external(url: String, mediaType: String): Favicon =
    Impl(ExternalTarget(url), None, Some(mediaType))

  /** Creates the configuration for a single favicon based on an internal resource and its virtual path.
    * This resource must be part of the inputs known to Laika.
    *
    * The sizes string will be used in the corresponding `sizes` attribute of the generated `&lt;link&gt;` tag.
    */
  def internal(path: Path, sizes: String): Favicon =
    Impl(InternalTarget(path), Some(sizes), mediaType(path.suffix))

  /** Creates the configuration for a single favicon based on an internal resource and its virtual path.
    * This resource must be part of the inputs known to Laika.
    */
  def internal(path: Path): Favicon =
    Impl(InternalTarget(path), None, mediaType(path.suffix))

}

/** Represents link panel to be displayed on the top right side of the landing page.
  *
  * @param title the title shown at the top of the panel
  * @param links the links to render inside the panel
  */
case class LinkPanel(title: String, links: Seq[TextLink])

object LinkPanel {

  def apply(title: String, link: TextLink, links: TextLink*): LinkPanel =
    apply(title, link +: links)

}

/** Represents release info to be displayed on the landing page.
  *
  * This is specific for sites that serve as documentation for software projects.
  *
  * @param title the header above the version number, e.g. "Latest Stable Release"
  * @param version the version number of the release
  */
case class ReleaseInfo(title: String, version: String)

/** Represents a single teaser block to be displayed on the landing page.
  * Any number of these blocks can be passed to the Helium configuration.
  */
case class Teaser(title: String, description: String)

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
