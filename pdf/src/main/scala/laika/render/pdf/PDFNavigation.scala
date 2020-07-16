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

package laika.render.pdf

import laika.ast._
import laika.format.PDF
import laika.io.model.RenderedTreeRoot

/** Prepares a document tree for the PDF rendering step by inserting all enabled navigation elements, 
  * like PDF bookmarks.
  * 
  * @author Jens Halm
  */
object PDFNavigation {

  /** Adds link targets for each document in the specified tree, including documents in subtrees. 
    */
  def addDocLinks (tree: DocumentTree): DocumentTree =
    tree rewrite { _ => RewriteRules.forBlocks {
      case title: Title =>
        // nav directives will link to an empty id, not the id of the title element
        Replace(BlockSequence(Seq(title), Id("")))
      case root: RootElement if root.collect { case t: Title => t }.isEmpty =>
        val insert = InternalLinkTarget(Id(""))
        Replace(RootElement(insert +: root.content))
    }}

  /** Generates bookmarks for the structure of the DocumentTree. 
    *
    * Individual bookmarks can stem from tree or subtree titles, document titles or document sections, 
    * depending on which recursion depth is configured.
    * The configuration key for setting the recursion depth is `pdf.bookmarks.depth`.
    *
    *  @param result the rendered result tree to generate bookmarks for
    *  @param depth the recursion depth through trees, documents and sections
    *  @return a fragment map containing the generated bookmarks
    */
  def generateBookmarks[F[_]] (result: RenderedTreeRoot[F], depth: Option[Int]): Map[String, Element] = if (depth.contains(0)) Map() else {
    val context = NavigationBuilderContext(
      maxLevels = depth.getOrElse(Int.MaxValue),
      currentLevel = 0,
      itemStyles = Set("bookmark")
    )
    val toc = result.tree.asNavigationItem(context).content
    Map("bookmarks" -> NavigationList(toc, Style.bookmark))
  }
  
  /** Prepares the document tree before rendering the interim XSL-FO output,
    * adding link targets for each document in the specified tree, including documents in subtrees.  
    */
  def prepareTree (root: DocumentTreeRoot, config: PDF.BookConfig): DocumentTreeRoot = {
    val finalTree = 
      if (config.navigationDepth.contains(0)) root.tree
      else addDocLinks(root.tree)
    root.copy(tree = finalTree)
  }
  
}
