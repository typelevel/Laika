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

import laika.config.ConfigBuilder
import laika.ast._
import laika.format.PDF
import laika.io.model.{RenderedDocument, RenderedTree, RenderedTreeRoot}
import laika.render.FOFormatter.{Bookmark, BookmarkTree, Leader, PageNumberCitation}
import laika.rewrite.nav.TocGenerator

/** Prepares a document tree for the PDF rendering step by inserting all enabled navigation elements, 
  * like PDF bookmarks or table of contents.
  * 
  * @author Jens Halm
  */
object PDFNavigation {

  object DocNames {
    val treeTitle = "title"
    val toc = "_toc_"
  }
  
  /** Adds title elements for each tree and subtree in the specified root tree. 
    * Tree titles can be specified in the configuration file for each tree.
    */
  def addTreeLinks (tree: DocumentTree): DocumentTree = {
    val newContent = tree.content map {
      case t: DocumentTree => addTreeLinks(t)
      case d: Document => d
    }
    val titleDoc = tree.titleDocument.orElse {
      if (tree.isEmpty) None
      else Some(Document(
        path = tree.path / DocNames.treeTitle,
        content = RootElement(InternalLinkTarget(Id(""))),
        config = ConfigBuilder.empty.withValue("title", tree.title.fold(tree.name)(_.extractText)).build
      ))
    }
    tree.copy(
      titleDocument = titleDoc,
      content = newContent
    )
  }

  /** Adds title elements for each document in the specified tree, including documents in subtrees. 
    * Document titles will be obtained either from a `Title` element in the document's content 
    * or from its configuration header.
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
  def generateBookmarks[F[_]] (result: RenderedTreeRoot[F], depth: Int): Map[String, Element] = {

    def sectionBookmarks (path: Path, sections: Seq[SectionInfo], levels: Int): Seq[Bookmark] =
      if (levels == 0) Nil
      else for (section <- sections) yield {
        val title = section.title.extractText
        val children = sectionBookmarks(path, section.content, levels - 1)
        Bookmark(InternalTarget.fromPath(path.withFragment(section.id), result.tree.path), title, children)
      }

    def treeBookmarks (tree: RenderedTree, levels: Int): Seq[Bookmark] = {
      if (levels == 0) Nil
      else tree.content.collect {
        case doc: RenderedDocument if doc.name != DocNames.toc =>
          val title = doc.title.fold(doc.name)(_.extractText)
          val children = sectionBookmarks(doc.path, doc.sections, levels - 1)
          Seq(Bookmark(InternalTarget.fromPath(doc.path, result.tree.path), title, children))
        case subtree: RenderedTree if !tree.isEmpty =>
          val title = subtree.title.fold(subtree.name)(_.extractText)
          val children = treeBookmarks(subtree, levels - 1)
          Seq(Bookmark(InternalTarget.fromPath(subtree.path / DocNames.treeTitle, result.tree.path), title, children))
      }.flatten
    }

    if (depth == 0) Map()
    else Map("bookmarks" -> BookmarkTree(treeBookmarks(result.tree, depth)))
  }

  /** Inserts a table of content into the specified document tree.
    * The recursion depth can be set with the configuration key `pdf.toc.depth`.
    */
  def insertToc (tree: DocumentTree, depth: Int, title: Option[String]): DocumentTree = {

    def toBlockSequence (blocks: Seq[Element]): Seq[Block] = blocks flatMap {
      case BulletList(items, _, _) => toBlockSequence(items)
      case BulletListItem(content, _, _) => toBlockSequence(content)
      case Paragraph(Seq(link @ SpanLink(content, target: InternalTarget, _, _)), opt) => 
        Seq(Paragraph(Seq(link.copy(
          content = content :+ Leader() :+ PageNumberCitation(target)
        )), opt))
    }

    val toc = toBlockSequence(TocGenerator.fromTree(tree, depth, tree.path / DocNames.toc))
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
      else addTreeLinks(addDocLinks(root.tree))
    val finalTree = if (config.tocDepth > 0) insertToc(withLinks, config.tocDepth, config.tocTitle) else withLinks
    root.copy(tree = finalTree)
  }
  
}
