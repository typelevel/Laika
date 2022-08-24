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

import laika.ast._
import laika.parse.{GeneratedSource, SourceFragment}

/** A Helium link type available for navigation bars and the landing page.
  */
sealed trait ThemeLink extends SpanResolver {
  
  val source: SourceFragment = GeneratedSource

  type Self <: ThemeLink
  
  def unresolvedMessage: String = s"Unresolved theme link: $this"

  def runsIn (phase: RewritePhase): Boolean = phase.isInstanceOf[RewritePhase.Render]

}

sealed trait SingleTargetLink extends ThemeLink {

  /** The target of the link, either internal or external. */
  def target: Target

  def resolve (cursor: DocumentCursor): Span = target match {
    case et: ExternalTarget => createLink(et)
    case it: InternalTarget => cursor.validateAndRecover(createLink(it), source)
  }

  protected def createLink (target: Target): Link
  
}

/** A link consisting of an icon and optional text.
  */
sealed abstract case class IconLink (target: Target, icon: Icon, text: Option[String] = None, options: Options = NoOpt) extends SingleTargetLink {
  type Self = IconLink
  protected def createLink (target: Target): Link = SpanLink(icon +: text.map(Text(_)).toSeq, target, options = HeliumStyles.iconLink + options)
  def withOptions(newOptions: Options): IconLink = new IconLink(target, icon, text, newOptions) {}
}

object IconLink {
  /** Creates an icon link to an external target, consisting of an icon and optional text. */
  def external (url: String, icon: Icon, text: Option[String] = None, options: Options = NoOpt): IconLink =
    new IconLink(ExternalTarget(url), icon, text, options) {}
  /** Creates an icon link to an internal target, consisting of an icon and optional text. */
  def internal (path: Path, icon: Icon, text: Option[String] = None, options: Options = NoOpt): IconLink =
    new IconLink(InternalTarget(path), icon, text, options) {}
}

/** A link consisting of text and an optional icon, by default rendered in a rounded rectangle.
  */
sealed abstract case class ButtonLink (target: Target, text: String, icon: Option[Icon] = None, options: Options = NoOpt) extends SingleTargetLink {
  type Self = ButtonLink
  protected def createLink (target: Target): Link = SpanLink(icon.toSeq :+ Text(text), target, options = HeliumStyles.buttonLink + options)
  def withOptions(newOptions: Options): ButtonLink = new ButtonLink(target, text, icon, newOptions) {}
}

object ButtonLink {
  /** Creates a button link to an external target, consisting of text and an optional icon rendered in a rounded rectangle. */
  def external (url: String, text: String, icon: Option[Icon] = None, options: Options = NoOpt): ButtonLink =
    new ButtonLink(ExternalTarget(url), text, icon, options) {}
  /** Creates a button link to an internal target, consisting of text and an optional icon rendered in a rounded rectangle. */
  def internal (path: Path, text: String, icon: Option[Icon] = None, options: Options = NoOpt): ButtonLink =
    new ButtonLink(InternalTarget(path), text, icon, options) {}
}

/** A simple text link.
  */
sealed abstract case class TextLink (target: Target, text: String, options: Options = NoOpt) extends SingleTargetLink {
  type Self = TextLink
  protected def createLink (target: Target): Link = SpanLink(Seq(Text(text)), target, options = HeliumStyles.textLink + options)
  def withOptions(newOptions: Options): TextLink = new TextLink(target, text, newOptions) {}
}

object TextLink {
  /** Creates a simple text link to an external target. */
  def external (url: String, text: String, options: Options = NoOpt): TextLink =
    new TextLink(ExternalTarget(url), text, options) {}
  /** Creates a simple text link to an internal target. */
  def internal (path: Path, text: String, options: Options = NoOpt): TextLink =
    new TextLink(InternalTarget(path), text, options) {}
}

/** A simple image link.
  */
sealed abstract case class ImageLink (target: Target, image: Image, options: Options = NoOpt) extends SingleTargetLink {
  type Self = ImageLink
  protected def createLink (target: Target): Link = SpanLink(Seq(image), target, options = HeliumStyles.imageLink + options)
  def withOptions(newOptions: Options): ImageLink =
    new ImageLink(target, image, newOptions) {}
}

object ImageLink {
  /** Creates a simple image link to an external target. */
  def external (url: String, image: Image, options: Options = NoOpt): ImageLink = 
    new ImageLink(ExternalTarget(url), image, options) {}
  /** Creates a simple image link to an internal target. */
  def internal (path: Path, image: Image, options: Options = NoOpt): ImageLink =
    new ImageLink(InternalTarget(path), image, options) {}
}

/** A generic group of theme links.
  *
  * Can be used to create structures like a row of icon links in a vertical column of text links. 
  */
sealed abstract case class LinkGroup (links: Seq[ThemeLink], options: Options = NoOpt) extends ThemeLink {
  type Self = LinkGroup
  def resolve (cursor: DocumentCursor): Span = SpanSequence(links.map(_.resolve(cursor)), HeliumStyles.linkRow + options)
  def withOptions(newOptions: Options): LinkGroup = new LinkGroup(links, newOptions) {}
}

object LinkGroup {
  def create (link: ThemeLink, links: ThemeLink*): LinkGroup = new LinkGroup(link +: links) {}
}
