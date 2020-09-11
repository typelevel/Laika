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

/** A Helium link type available for navigation bars and the landing page.
  */
sealed trait ThemeLink extends SpanResolver {

  type Self <: ThemeLink
  
  /** The target of the link, either internal or external. */
  def target: ThemeTarget
  
  def resolve (cursor: DocumentCursor): Span = target.resolve(cursor) match {
    case Left(msg) => InvalidElement(msg, s"<ThemeLink: $this>").asSpan
    case Right(target) => createLink(target)
  }

  def unresolvedMessage: String = s"Unresolved theme link: $this"

  protected def createLink (target: Target): Span
  
}

/** A link consisting of an icon and optional text.
  */
sealed abstract case class IconLink (target: ThemeTarget, icon: Icon, text: Option[String] = None, options: Options = NoOpt) extends ThemeLink {
  type Self = IconLink
  protected def createLink (target: Target): Span = SpanLink(icon +: text.map(Text(_)).toSeq, target, options = HeliumStyles.iconLink + options)
  def withOptions(newOptions: Options): IconLink = new IconLink(target, icon, text, newOptions) {}
}

object IconLink {
  /** Creates an icon link to an external target, consisting of an icon and optional text. */
  def external (url: String, icon: Icon, text: Option[String] = None, options: Options = NoOpt): IconLink =
    new IconLink(ThemeTarget.external(url), icon, text, options) {}
  /** Creates an icon link to an internal target, consisting of an icon and optional text. */
  def internal (path: Path, icon: Icon, text: Option[String] = None, options: Options = NoOpt): IconLink =
    new IconLink(ThemeTarget.internal(path), icon, text, options) {}
}

/** A link consisting of text and an optional icon rendered in a rounded rectangle.
  */
sealed abstract case class ButtonLink (target: ThemeTarget, text: String, icon: Option[Icon] = None, options: Options = NoOpt) extends ThemeLink {
  type Self = ButtonLink
  protected def createLink (target: Target): Span = SpanLink(icon.toSeq :+ Text(text), target, options = HeliumStyles.buttonLink + options)
  def withOptions(newOptions: Options): ButtonLink = new ButtonLink(target, text, icon, newOptions) {}
}

object ButtonLink {
  /** Creates a button link to an external target, consisting of text and an optional icon rendered in a rounded rectangle. */
  def external (url: String, text: String, icon: Option[Icon] = None, options: Options = NoOpt): ButtonLink =
    new ButtonLink(ThemeTarget.external(url), text, icon, options) {}
  /** Creates a button link to an internal target, consisting of text and an optional icon rendered in a rounded rectangle. */
  def internal (path: Path, text: String, icon: Option[Icon] = None, options: Options = NoOpt): ButtonLink =
    new ButtonLink(ThemeTarget.internal(path), text, icon, options) {}
}

/** A simple text link.
  */
sealed abstract case class TextLink (target: ThemeTarget, text: String, options: Options = NoOpt) extends ThemeLink {
  type Self = TextLink
  protected def createLink (target: Target): Span = SpanLink(Seq(Text(text)), target, options = HeliumStyles.textLink + options)
  def withOptions(newOptions: Options): TextLink = new TextLink(target, text, newOptions) {}
}

object TextLink {
  /** Creates a simple text link to an external target. */
  def external (url: String, text: String, options: Options = NoOpt): TextLink =
    new TextLink(ThemeTarget.external(url), text, options) {}
  /** Creates a simple text link to an internal target. */
  def internal (path: Path, text: String, options: Options = NoOpt): TextLink =
    new TextLink(ThemeTarget.internal(path), text, options) {}
}

/** A link target for Helium's buttons.
  * 
  * The API is slightly different than for Laika's core AST nodes, 
  * as the Helium configuration is global where it would not make sense to allow for relative paths
  * to be used for linking.
  * Therefore a theme target is either based on an external URL or an internal, absolute path.
  */
sealed trait ThemeTarget {
  private[helium] def description: String
  private[helium] def resolve (cursor: DocumentCursor): Either[String, Target]
}

/** Companion for creating ThemeTarget instances.
  * 
  * Usually not used directly in theme configuration, as all objects expecting a target have a 
  * corresponding companion with shortcuts, e.g. `TextLink.external("[URL]", "[Text]")`.
  * 
  * This is a type tailored for theme configuration as it limits the way a target can be defined:
  * it has to be either an external URL or an absolute, internal path.
  * 
  * A relative internal path would be impossible to interpret in global theme configuration,
  * since a relative path can only be resolved from the perspective of a concrete document.
  * Laika's core AST and its `Target` type are most often used by parsers when constructing
  * the result of parsed markup, and inside a markup document relative paths can be properly resolved.
  */
object ThemeTarget {

  /** Creates an internal target based on an absolute, virtual path.
    * 
    * The path will be validated, therefore must point to the an input resource known to Laika.
    */
  private[helium] def internal (target: Path): ThemeTarget = new ThemeTarget {
    val description = s"internal target: '${target.toString}'"
    def resolve  (cursor: DocumentCursor): Either[String, Target] = {
      val valid = cursor.root.target.tree.selectDocument(target.withoutFragment.relative).nonEmpty || 
        cursor.root.target.staticDocuments.contains(target.withoutFragment)
      if (valid) Right(InternalTarget(target).relativeTo(cursor.path))
      else Left(s"Theme Link to unresolved target: $target")
    }
  }
  
  /** Creates a target based on an external URL */
  private[helium] def external (url: String): ThemeTarget = new ThemeTarget {
    val description = s"external target: '$url'"
    def resolve  (cursor: DocumentCursor): Either[String, Target] = Right(ExternalTarget(url))
  }
}

/** A logo type that can be used in various Helium configuration options.
  * The only required property is the target, which is either an external URL or an internal, relative path.
  */
sealed abstract case class Logo(target: ThemeTarget,
                                width: Option[Length] = None,
                                height: Option[Length] = None,
                                alt: Option[String] = None,
                                title: Option[String] = None,
                                options: Options = NoOpt) extends ThemeLink {
  type Self = Logo
  protected def createLink (target: Target): Span = Image(target, width, height, alt, title)
  def withOptions(newOptions: Options): Logo =
    new Logo(target, width, height, alt, title, newOptions) {}
}

object Logo {
  
  /** Creates a logo with an external image URL.
    * The width and height are interpreted as the intrinsic size of the image and do not necessarily represent
    * the actual display size which should be set via CSS. 
    * You can assign classes to the options property for this purpose.
    * The `alt` and `title` properties will be rendered as the corresponding attributes in HTML and ignored for PDF.
   */
  def external (url: String,
                width: Option[Length] = None,
                height: Option[Length] = None,
                alt: Option[String] = None,
                title: Option[String] = None,
                options: Options = NoOpt): Logo = 
    new Logo(ThemeTarget.external(url), width, height, alt, title, options) {}
  
  /** Creates a logo for an image that is part of the input resources of the transformation.
    * The width and height are interpreted as the intrinsic size of the image and do not necessarily represent
    * the actual display size which should be set via CSS. 
    * You can assign classes to the options property for this purpose.
    * The `alt` and `title` properties will be rendered as the corresponding attributes in HTML and ignored for PDF.
    */
  def internal (path: Path,
                width: Option[Length] = None,
                height: Option[Length] = None,
                alt: Option[String] = None,
                title: Option[String] = None,
                options: Options = NoOpt): Logo =
    new Logo(ThemeTarget.internal(path), width, height, alt, title, options) {}
}
