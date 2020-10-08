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

package laika.ast

import laika.rewrite.nav.TargetFormats

/** A bullet list that may contain nested lists.
  */
case class BulletList (content: Seq[BulletListItem], format: BulletFormat, options: Options = NoOpt) extends Block
  with ListContainer
  with RewritableContainer {
  type Self = BulletList

  override def rewriteChildren (rules: RewriteRules): BulletList =
    copy(content = content.map(_.rewriteChildren(rules)))

  def withOptions (options: Options): BulletList = copy(options = options)
}

/** An enumerated list that may contain nested lists.
  */
case class EnumList (content: Seq[EnumListItem], format: EnumFormat, start: Int = 1, options: Options = NoOpt) extends Block
  with ListContainer
  with RewritableContainer {
  type Self = EnumList

  override def rewriteChildren (rules: RewriteRules): EnumList =
    copy(content = content.map(_.rewriteChildren(rules)))

  def withOptions (options: Options): EnumList = copy(options = options)
}

/** The format of a bullet list item.
  */
trait BulletFormat

/** Bullet format based on a simple string.
  */
case class StringBullet (bullet: String) extends BulletFormat

/** The format of enumerated list items.
  */
case class EnumFormat (enumType: EnumType = EnumType.Arabic, prefix: String = "", suffix: String = ".") {
  override def toString = s"EnumFormat($enumType,${prefix}N$suffix)"
}

/** Represents the type of an ordered list.
  */
sealed abstract class EnumType

/** Enumeration of supported enum types.
  */
object EnumType {

  /** Arabic enumeration style (1, 2, 3...)
    */
  case object Arabic extends EnumType

  /** Lowercase letter enumeration style (a, b, c...)
    */
  case object LowerAlpha extends EnumType

  /** Uppercase letter enumeration style (A, B, C...)
    */
  case object UpperAlpha extends EnumType

  /** Lowercase Roman numeral enumeration style (i, ii, iii, iv...)
    */
  case object LowerRoman extends EnumType

  /** Uppercase Roman numeral enumeration style (I, II, III, IV...)
    */
  case object UpperRoman extends EnumType
}


/** A single bullet list item consisting of one or more block elements.
  */
case class BulletListItem (content: Seq[Block], format: BulletFormat, options: Options = NoOpt) extends ListItem
  with BlockContainer {
  type Self = BulletListItem
  def withContent (newContent: Seq[Block]): BulletListItem = copy(content = newContent)
  def withOptions (options: Options): BulletListItem = copy(options = options)
}

/** A single enum list item consisting of one or more block elements.
  */
case class EnumListItem (content: Seq[Block], format: EnumFormat, position: Int, options: Options = NoOpt) extends ListItem
  with BlockContainer {
  type Self = EnumListItem
  def withContent (newContent: Seq[Block]): EnumListItem = copy(content = newContent)
  def withOptions (options: Options): EnumListItem = copy(options = options)
}

/** A list of terms and their definitions.
  *  Not related to the `Definition` base trait.
  */
case class DefinitionList (content: Seq[DefinitionListItem], options: Options = NoOpt) extends Block with ListContainer
  with RewritableContainer {
  type Self = DefinitionList
  def rewriteChildren (rules: RewriteRules): DefinitionList = copy(content = content.map(_.rewriteChildren(rules)))
  def withOptions (options: Options): DefinitionList = copy(options = options)
}

/** A single definition item, containing the term and definition (as the content property).
  */
case class DefinitionListItem (term: Seq[Span], content: Seq[Block], options: Options = NoOpt) extends ListItem
  with BlockContainer {
  type Self = DefinitionListItem

  override def rewriteChildren (rules: RewriteRules): DefinitionListItem =
    copy(content = rules.rewriteBlocks(content), term = rules.rewriteSpans(term))

  def withContent (newContent: Seq[Block]): DefinitionListItem = copy(content = newContent)
  def withOptions (options: Options): DefinitionListItem = copy(options = options)
}

/** The root node of a navigation structure */
case class NavigationList (content: Seq[NavigationItem], options: Options = NoOpt) extends Block with ListContainer with RewritableContainer {

  type Self = NavigationList
  
  /** Create a new navigation list that only contains the entries matching the specified target format.
    */
  def forFormat (format: String): NavigationList = {
    def filter (items: Seq[NavigationItem]): Seq[NavigationItem] = items.flatMap { item =>
      if (item.targetFormats.contains(format)) {
        val link = item.link.filter(_.targetFormats.contains(format))
        val content = filter(item.content)
        if (link.isEmpty && content.isEmpty) None
        else Some(item.copy(link = link, content = content))
      }
      else None
    }
    copy(content = filter(content))
  }

  def rewriteChildren (rules: RewriteRules): NavigationList = copy(
    content = content.map(_.rewriteChildren(rules))
  )
  def withOptions (options: Options): NavigationList = copy(options = options)
}

/** Represents a navigation entry with an optional target link and optional child items.
  * When the target link is not present, this item only serves as a navigation header for its children.
  */
case class NavigationItem (title: SpanSequence, 
                           content: Seq[NavigationItem],
                           link: Option[NavigationLink] = None,
                           targetFormats: TargetFormats = TargetFormats.All, 
                           options: Options = NoOpt) extends Block with ListItem with ElementContainer[NavigationItem] with RewritableContainer with ListContainer {

  type Self = NavigationItem

  /** Returns the first link from the children of this navigation item.
    * This is useful for navigation systems where each entry must contain a concrete link.  */
  def firstChildLink: Option[NavigationLink] = content.collectFirst { 
    case item if item.link.nonEmpty => item.link
    case item if item.content.nonEmpty => item.firstChildLink
  }.flatten

  def rewriteChildren (rules: RewriteRules): NavigationItem = copy(
    title = title.rewriteChildren(rules),
    content = content.map(_.rewriteChildren(rules))
  )
  def withOptions (options: Options): NavigationItem = copy(options = options)
}

/** Represents a book navigation entry that links to content in the document tree.
  */
case class NavigationLink (target: Target,
                           selfLink: Boolean = false,
                           targetFormats: TargetFormats = TargetFormats.All)
