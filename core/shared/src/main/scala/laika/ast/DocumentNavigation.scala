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
  def asNavigationItem (context: NavigationBuilderContext = NavigationBuilderContext()): NavigationItem = {
    val children = if (context.isComplete || context.excludeSections) Nil else sections.map(_.asNavigationItem(path, context.nextLevel))
    context.newNavigationItem(title.getOrElse(SpanSequence(path.name)), Some(path), children)
  }

}

/** The context of a navigation builder that can get passed down in recursive calls to the
  * various types that have an asNavigationItem method.
  *
  * @param refPath the path of document from which this document will be linked (for creating a corresponding relative path)
  * @param itemStyles the styles to assign to each navigation item as a render hint
  * @param maxLevels the number of levels of sub-trees, documents or sections to create navigation info for
  * @param currentLevel the current level of the navigation tree being built
  * @param excludeSections indicates whether the recursion should exclude sections of documents even when maxLevels
  *                        has not been reached yet
  */
case class NavigationBuilderContext (refPath: Path = Root,
                                     itemStyles: Set[String] = Set(),
                                     maxLevels: Int = Int.MaxValue,
                                     currentLevel: Int = 1,
                                     excludeSections: Boolean = false,
                                     excludeSelf: Boolean = false) {

  lazy val nextLevel: NavigationBuilderContext = copy(currentLevel = currentLevel + 1)

  val isComplete: Boolean = currentLevel >= maxLevels

  def newNavigationItem (title: SpanSequence, target: Option[Path], children: Seq[NavigationItem]): NavigationItem = {
    val styles = Style.level(currentLevel) + Styles(itemStyles)
    target.fold[NavigationItem](NavigationHeader(title, children, styles)) { target =>
      NavigationLink(title, InternalTarget(target).relativeTo(refPath), children, target == refPath, styles)
    }
  }
}
