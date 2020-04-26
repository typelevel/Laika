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
import laika.config.ConfigBuilder
import laika.format.PDF
import laika.io.model.RenderedTreeRoot
import laika.rewrite.nav.TitleDocumentConfig

/** Prepares a document tree for the PDF rendering step by inserting all enabled navigation elements, 
  * like PDF bookmarks or table of contents.
  * 
  * @author Jens Halm
  */
object PDFNavigation {

  object DocNames {
    val toc = "_toc_"
  }
  
  /** Adds link targets for each tree and subtree in the specified root tree
    * that does not already contain a title document.
    */
  def addTreeLinks (tree: DocumentTree, titleName: String): DocumentTree = {
    val newContent = tree.content map {
      case t: DocumentTree => addTreeLinks(t, titleName)
      case d: Document => d
    }
    val titleDoc = tree.titleDocument.orElse {
      if (tree.isEmpty) None
      else Some(Document(
        path = tree.path / titleName,
        content = RootElement(InternalLinkTarget(Id(""))),
        config = ConfigBuilder.empty.withValue("title", tree.title.fold(tree.name)(_.extractText)).build
      ))
    }
    tree.copy(
      titleDocument = titleDoc,
      content = newContent
    )
  }

  /** Adds link targets for each document in the specified tree, including documents in subtrees. 
    */
  def addDocLinks (tree: DocumentTree): DocumentTree =
    tree rewrite { _ => RewriteRules.forBlocks {
      case title: Title =>
        // toc directives will link to an empty id, not the id of the title element
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
  def generateBookmarks[F[_]] (result: RenderedTreeRoot[F], depth: Int): Map[String, Element] = if (depth == 0) Map() else {
    val context = NavigationBuilderContext(
      maxLevels = depth,
      currentLevel = 0,
      itemStyles = Set("bookmark")
    )
    val toc = result.tree.asNavigationItem(context).content
    Map("bookmarks" -> NavigationList(toc, Style.bookmark))
  }
  
  /** Inserts a table of content into the specified document tree.
    * The recursion depth can be set with the configuration key `pdf.toc.depth`.
    */
  def insertToc (tree: DocumentTree, depth: Int, title: Option[String]): DocumentTree = {
    val context = NavigationBuilderContext(
      refPath = tree.path / DocNames.toc,
      maxLevels = depth,
      currentLevel = 0
    )
    val toc = tree.asNavigationItem(context).content
    val root = title.fold(RootElement(toc)){ title => RootElement(Title(title) +: toc) }
    val doc = Document(tree.path / DocNames.toc, root)
    tree.copy(content = doc +: tree.content)
  }

  /** Prepares the document tree before rendering the interim XSL-FO
    *  output. Preparation may include insertion of tree or document titles, PDF bookmarks
    *  and a table of content, depending on configuration.
    */
  def prepareTree (root: DocumentTreeRoot, config: PDF.Config): DocumentTreeRoot = {
    val withLinks = 
      if (config.bookmarkDepth == 0 && config.tocDepth == 0) root.tree
      else addTreeLinks(addDocLinks(root.tree), TitleDocumentConfig.inputName(root.config))
    val finalTree = if (config.tocDepth > 0) insertToc(withLinks, config.tocDepth, config.tocTitle) else withLinks
    root.copy(tree = finalTree)
  }
  
}
