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

import laika.ast.Path.Root
import laika.config.TargetFormats

/** Represents a document structure with sections that can be turned into a navigation structure.
  *
  * @author Jens Halm
  */
trait DocumentNavigation extends Navigatable {

  /** The title of this document, obtained from the document
    * structure or from the configuration. In case no title
    * is defined in either of the two places the result will
    * be `None`.
    */
  def title: Option[SpanSequence]

  /** The section structure of this document based on the hierarchy
    * of headers found in the original text markup.
    */
  def sections: Seq[SectionInfo]

  /** Creates the navigation structure for this document up to the specified depth.
    * The returned instance can be used as part of a bigger navigation structure comprising of trees, documents and their sections.
    *
    * @param context captures the navigation depth, reference path and styles for the navigation tree being built
    * @return a navigation item that can be used as part of a bigger navigation structure comprising of trees, documents and their sections
    */
  def asNavigationItem(
      context: NavigationBuilderContext = NavigationBuilderContext.defaults
  ): NavigationItem = {
    val children =
      if (context.isComplete || context.excludeSections) Nil
      else sections.map(_.asNavigationItem(path, context.nextLevel))
    context.newNavigationItem(
      title.getOrElse(SpanSequence(path.name)),
      path,
      children,
      targetFormats
    )
  }

  /** The formats this document should be rendered to.
    */
  def targetFormats: TargetFormats
}

/** The context of a navigation builder that can get passed down in recursive calls to the
  * various types that have an asNavigationItem method.
  */
sealed abstract class NavigationBuilderContext {

  /** The path of the document for which links will be created
    * (meaning all generated links will be relative to this path).
    */
  def refPath: Path

  /** The styles to assign to each navigation item as a render hint. */
  def itemStyles: Set[String]

  /** The number of levels of sub-trees, documents or sections to create navigation info for. */
  def maxLevels: Int

  /** The current level of the navigation tree being built. */
  def currentLevel: Int

  /** Indicates whether the recursion should exclude sections of documents
    * even when maxLevels has not been reached yet.
    */
  def excludeSections: Boolean

  /** Indicates that links pointing to the current document should be omitted.
    */
  def excludeSelf: Boolean

  /** Creates a new instance for the next navigation level.
    */
  def nextLevel: NavigationBuilderContext

  /** Indicates whether the final navigation level has been reached.
    *
    * True if `currentLevel >= maxLevels`.
    */
  val isComplete: Boolean = currentLevel >= maxLevels

  def withRefPath(path: Path): NavigationBuilderContext
  def withItemStyles(styles: String*): NavigationBuilderContext
  def withMaxLevels(max: Int): NavigationBuilderContext
  def withExcludeSections(value: Boolean): NavigationBuilderContext
  def withExcludeSelf(value: Boolean): NavigationBuilderContext
  private[laika] def withCurrentLevel(value: Int): NavigationBuilderContext

  def newNavigationItem(
      title: SpanSequence,
      target: Option[DocumentNavigation],
      children: Seq[NavigationItem],
      targetFormats: TargetFormats
  ): NavigationItem =
    createNavigationItem(
      title,
      target.map(doc => (doc.path, doc.targetFormats)),
      children,
      targetFormats
    )

  def newNavigationItem(
      title: SpanSequence,
      target: Path,
      children: Seq[NavigationItem],
      targetFormats: TargetFormats
  ): NavigationItem =
    createNavigationItem(title, Some((target, TargetFormats.All)), children, targetFormats)

  private def createNavigationItem(
      title: SpanSequence,
      target: Option[(Path, TargetFormats)],
      children: Seq[NavigationItem],
      targetFormats: TargetFormats
  ): NavigationItem = {
    val styles = Style.level(currentLevel) + Styles(itemStyles)
    val link   = target.map { case (path, formats) =>
      NavigationLink(InternalTarget(path).relativeTo(refPath), path == refPath, formats)
    }
    NavigationItem(title, children, link, targetFormats, styles)
  }

}

object NavigationBuilderContext {

  def defaults: NavigationBuilderContext =
    Impl(Root, Set(), Int.MaxValue, 1, excludeSections = false, excludeSelf = false)

  case class Impl(
      refPath: Path,
      itemStyles: Set[String],
      maxLevels: Int,
      currentLevel: Int,
      excludeSections: Boolean,
      excludeSelf: Boolean
  ) extends NavigationBuilderContext {

    override def productPrefix: String = "NavigationBuilderContext"

    lazy val nextLevel: NavigationBuilderContext = copy(currentLevel = currentLevel + 1)

    def withRefPath(path: Path): NavigationBuilderContext = copy(refPath = path)

    def withItemStyles(itemStyles: String*): NavigationBuilderContext =
      copy(itemStyles = itemStyles.toSet)

    def withMaxLevels(max: Int): NavigationBuilderContext = copy(maxLevels = max)

    def withExcludeSections(value: Boolean): NavigationBuilderContext =
      copy(excludeSections = value)

    def withExcludeSelf(value: Boolean): NavigationBuilderContext = copy(excludeSelf = value)

    private[laika] def withCurrentLevel(value: Int): NavigationBuilderContext =
      copy(currentLevel = value)

  }

}
