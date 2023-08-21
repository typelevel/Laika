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

import cats.data.NonEmptyList
import laika.ast.RelativePath.CurrentDocument
import laika.ast._
import laika.config.LaikaKeys
import laika.parse.{ GeneratedSource, SourceFragment }
import laika.rewrite.Versions

/** A Helium link type available for navigation bars and the landing page.
  */
sealed trait ThemeLink extends Unresolved {

  val source: SourceFragment = GeneratedSource

  type Self <: ThemeLink

  def unresolvedMessage: String = s"Unresolved theme link: $this"

  def runsIn(phase: RewritePhase): Boolean = phase.isInstanceOf[RewritePhase.Render]

}

sealed trait ThemeLinkSpan extends ThemeLink with SpanResolver {
  type Self <: ThemeLinkSpan
}

sealed trait ThemeLinkBlock extends ThemeLink with BlockResolver {
  type Self <: ThemeLinkBlock
}

sealed trait SingleTargetLink extends ThemeLinkSpan {

  /** The target of the link, either internal or external. */
  def target: Target

  def resolve(cursor: DocumentCursor): Span = target match {
    case et: ExternalTarget => createLink(et)
    case it: InternalTarget => cursor.validateAndRecover(createLink(it), source)
  }

  protected def createLink(target: Target): Link

}

sealed trait MultiTargetLink extends ThemeLink {

  def links: Seq[SingleTargetLink]

}

/** A link consisting of an icon and optional text.
  */
sealed abstract case class IconLink(
    target: Target,
    icon: Icon,
    text: Option[String] = None,
    options: Options = NoOpt
) extends SingleTargetLink {
  type Self = IconLink

  protected def createLink(target: Target): Link = {
    val iconTypeStyle = icon match {
      case _: IconGlyph     => "glyph-link"
      case _: IconStyle     => "style-link"
      case _: InlineSVGIcon => "svg-link"
      case _: SVGSymbolIcon => "svg-link"
    }
    val allOptions    = HeliumStyles.iconLink + Styles(iconTypeStyle) + options
    SpanLink(icon +: text.map(Text(_)).toSeq, target, options = allOptions)
  }

  def withOptions(newOptions: Options): IconLink = new IconLink(target, icon, text, newOptions) {}
}

object IconLink {

  /** Creates an icon link to an external target, consisting of an icon and optional text. */
  def external(
      url: String,
      icon: Icon,
      text: Option[String] = None,
      options: Options = NoOpt
  ): IconLink =
    new IconLink(ExternalTarget(url), icon, text, options) {}

  /** Creates an icon link to an internal target, consisting of an icon and optional text. */
  def internal(
      path: Path,
      icon: Icon,
      text: Option[String] = None,
      options: Options = NoOpt
  ): IconLink =
    new IconLink(InternalTarget(path), icon, text, options) {}

}

/** A link consisting of text and an optional icon, by default rendered in a rounded rectangle.
  */
sealed abstract case class ButtonLink(
    target: Target,
    text: String,
    icon: Option[Icon] = None,
    options: Options = NoOpt
) extends SingleTargetLink {
  type Self = ButtonLink

  protected def createLink(target: Target): Link =
    SpanLink(icon.toSeq :+ Text(text), target, options = HeliumStyles.buttonLink + options)

  def withOptions(newOptions: Options): ButtonLink =
    new ButtonLink(target, text, icon, newOptions) {}

}

object ButtonLink {

  /** Creates a button link to an external target, consisting of text and an optional icon rendered in a rounded rectangle. */
  def external(
      url: String,
      text: String,
      icon: Option[Icon] = None,
      options: Options = NoOpt
  ): ButtonLink =
    new ButtonLink(ExternalTarget(url), text, icon, options) {}

  /** Creates a button link to an internal target, consisting of text and an optional icon rendered in a rounded rectangle. */
  def internal(
      path: Path,
      text: String,
      icon: Option[Icon] = None,
      options: Options = NoOpt
  ): ButtonLink =
    new ButtonLink(InternalTarget(path), text, icon, options) {}

}

/** A simple text link.
  */
sealed abstract case class TextLink(target: Target, text: String, options: Options = NoOpt)
    extends SingleTargetLink {
  type Self = TextLink

  protected def createLink(target: Target): Link =
    SpanLink(Seq(Text(text)), target, options = HeliumStyles.textLink + options)

  def withOptions(newOptions: Options): TextLink = new TextLink(target, text, newOptions) {}
}

object TextLink {

  /** Creates a simple text link to an external target. */
  def external(url: String, text: String, options: Options = NoOpt): TextLink =
    new TextLink(ExternalTarget(url), text, options) {}

  /** Creates a simple text link to an internal target. */
  def internal(path: Path, text: String, options: Options = NoOpt): TextLink =
    new TextLink(InternalTarget(path), text, options) {}

}

/** A simple image link.
  */
sealed abstract case class ImageLink(target: Target, image: Image, options: Options = NoOpt)
    extends SingleTargetLink {
  type Self = ImageLink

  protected def createLink(target: Target): Link =
    SpanLink(Seq(image), target, options = HeliumStyles.imageLink + options)

  def withOptions(newOptions: Options): ImageLink =
    new ImageLink(target, image, newOptions) {}

}

object ImageLink {

  /** Creates a simple image link to an external target. */
  def external(url: String, image: Image, options: Options = NoOpt): ImageLink =
    new ImageLink(ExternalTarget(url), image, options) {}

  /** Creates a simple image link to an internal target. */
  def internal(path: Path, image: Image, options: Options = NoOpt): ImageLink =
    new ImageLink(InternalTarget(path), image, options) {}

}

/** A home link that inserts a link to the title page of the root document tree (if available)
  * or otherwise an invalid element.
  */
final case class DynamicHomeLink(options: Options = NoOpt) extends ThemeLinkSpan {
  type Self = DynamicHomeLink

  def resolve(cursor: DocumentCursor): Span = {
    cursor.root.tree.titleDocument match {
      case Some(homePage) =>
        IconLink.internal(homePage.path, HeliumIcon.home).resolve(cursor)
      case None           =>
        val message =
          "No target for home link found - for options see 'Theme Settings / Top Navigation Bar' in the manual"
        InvalidSpan(message, GeneratedSource)
    }
  }

  def withOptions(newOptions: Options): DynamicHomeLink = copy(options = newOptions)
}

object DynamicHomeLink {
  val default: DynamicHomeLink = DynamicHomeLink()
}

/** A generic group of theme links.
  *
  * Can be used to create structures like a row of icon links in a vertical column of text links.
  */
sealed abstract case class LinkGroup(links: Seq[SingleTargetLink], options: Options = NoOpt)
    extends ThemeLinkSpan with MultiTargetLink {
  type Self = LinkGroup

  def resolve(cursor: DocumentCursor): Span =
    SpanSequence(links.map(_.resolve(cursor)), HeliumStyles.linkRow + options)

  def withOptions(newOptions: Options): LinkGroup = new LinkGroup(links, newOptions) {}
}

object LinkGroup {

  def create(link: SingleTargetLink, links: SingleTargetLink*): LinkGroup = new LinkGroup(
    link +: links
  ) {}

}

/** A menu for the top navigation bar or the landing page.
  */
sealed abstract case class Menu(
    label: Seq[Span],
    links: Seq[SingleTargetLink],
    options: Options = NoOpt
) extends ThemeLinkBlock with MultiTargetLink {
  type Self = Menu

  def resolve(cursor: DocumentCursor): Block = {

    val toggle = SpanLink(label, InternalTarget(CurrentDocument()))
      .withOptions(HeliumStyles.textLink + HeliumStyles.menuToggle)

    val navLinks = links.map(_.resolve(cursor)).collect { case sl: SpanLink =>
      NavigationItem(SpanSequence(sl.content), Nil, Some(NavigationLink(sl.target)))
        .withOptions(Style.level(1))
    }

    val content = BlockSequence(NavigationList(navLinks)).withOptions(HeliumStyles.menuContent)

    BlockSequence(SpanSequence(toggle), content).withOptions(HeliumStyles.menuContainer + options)
  }

  def withOptions(newOptions: Options): Menu = new Menu(label, links, newOptions) {}
}

object Menu {

  def create(label: String, link: SingleTargetLink, links: SingleTargetLink*): Menu =
    new Menu(Seq(Text(label)), link +: links) {}

  def create(label: Seq[Span], link: SingleTargetLink, links: SingleTargetLink*): Menu =
    new Menu(label, link +: links) {}

}

sealed abstract case class VersionMenu(
    versionedLabelPrefix: String,
    unversionedLabel: String,
    links: Seq[SingleTargetLink],
    options: Options = NoOpt
) extends ThemeLinkBlock with MultiTargetLink {
  type Self = VersionMenu

  def resolve(cursor: DocumentCursor): Block = {
    cursor.config.get[Versions].toOption.fold[Block](BlockSequence.empty) { versions =>
      val isVersioned = cursor.config.get[Boolean](LaikaKeys.versioned).getOrElse(false)
      val labelText   =
        if (isVersioned) s"$versionedLabelPrefix ${versions.currentVersion.displayValue}"
        else unversionedLabel
      val menu        = new Menu(Seq(Text(labelText)), links, HeliumStyles.versionMenu) {}
      menu.resolve(cursor)
    }
  }

  def withOptions(newOptions: Options): VersionMenu =
    new VersionMenu(versionedLabelPrefix, unversionedLabel, links, newOptions) {}

}

object VersionMenu {

  def create(
      versionedLabelPrefix: String = default.versionedLabelPrefix,
      unversionedLabel: String = default.unversionedLabel,
      additionalLinks: Seq[SingleTargetLink] = Nil
  ): VersionMenu =
    new VersionMenu(versionedLabelPrefix, unversionedLabel, additionalLinks) {}

  val default: VersionMenu = create("Version", "Choose Version")

}

sealed abstract class ThemeNavigationSection {
  def title: String
  def links: NonEmptyList[TextLink]
}

object ThemeNavigationSection {

  private final case class Impl(title: String, links: NonEmptyList[TextLink])
      extends ThemeNavigationSection {
    override def productPrefix = "ThemeNavigationSection"
  }

  def apply(title: String, link: TextLink, links: TextLink*): ThemeNavigationSection =
    Impl(title, NonEmptyList.of(link, links *))

}
