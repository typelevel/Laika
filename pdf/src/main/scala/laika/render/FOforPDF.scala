/*
 * Copyright 2014 the original author or authors.
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

package laika.render

import laika.tree.Documents.DocumentTree
import laika.io.OutputProvider.OutputConfig
import laika.io.OutputProvider.ResultTree
import laika.io.OutputProvider.StringOutputProvider
import laika.tree.Documents._
import laika.tree.Elements._
import laika.render.FOWriter._
import laika.tree.TreeUtil
import laika.tree.TocGenerator

/** Responsible for rendering the XSL-FO for an entire document tree
 *  as an interim result to be consumed by the PDF post processor.
 *  
 *  On top of the regular XSL-FO renderer in laika-core this renderer
 *  inserts tree titles, bookmarks and a table of contents into
 *  the document tree before rendering.
 * 
 *  @author Jens Halm
 */
class FOforPDF {

  object DocNames {
    val treeTitle = "__title__"
    val toc = "__toc__"
    val bookmarks = "__bookmarks__"
  }
  
  protected def hasDocuments (tree: DocumentTree): Boolean = tree.documents.nonEmpty || tree.subtrees.exists(hasDocuments)
  
  protected def hasContent (nav: Navigatable): Boolean = nav match {
    case _:Document => true
    case tree: DocumentTree => hasDocuments(tree)
  }
  
  protected def getDepth (tree: DocumentTree, key: String) = (tree.config collect {
    case c if c.hasPath(key) => c.getInt(key)
  }).getOrElse(Int.MaxValue)
  
  protected def id (path: Path, ref: String) = 
    if (ref.isEmpty) path.toString
    else path.toString + "." + ref
  
  def addTreeTitles (tree: DocumentTree): DocumentTree =
    if (!hasDocuments(tree) || tree.title.isEmpty) tree
    else {
      val title = Header(1, tree.title, Styles("treeTitle") + Id(""))
      val root = RootElement(Seq(title))
      val doc = new Document(tree.path / DocNames.treeTitle, root)
      tree.prependDocument(doc)
    }
  
  def insertDocTitles (tree: DocumentTree): DocumentTree =
    tree rewrite { context => {
      case title: Title => Some(BlockSequence(Seq(title), Id("")))
      case root: RootElement => 
        if ((root select { case _: Title => true }).isEmpty)
          Some(RootElement(Title(context.document.title, Id("")) +: root.content))
        else 
          Some(root)
    }}
    
  def insertBookmarks (tree: DocumentTree): DocumentTree = {

    def sectionBookmarks (path: Path, sections: Seq[SectionInfo], levels: Int): Seq[Bookmark] = 
      if (levels == 0) Nil
      else for (section <- sections) yield {
        val ref = id(path,section.id)
        val title = section.title.text
        val children = sectionBookmarks(path, section.content, levels - 1)
        Bookmark(ref, title, children)
      }
    
    def treeBookmarks (tree: DocumentTree, levels: Int): Seq[Bookmark] = {
      if (levels == 0) Nil
      else (for (nav <- tree.navigatables if hasContent(nav)) yield nav match {
        case doc: Document =>
          val ref = id(doc.path,"")
          val title = TreeUtil.extractText(doc.title)
          val children = sectionBookmarks(doc.path, doc.sections, levels - 1)
          Bookmark(ref, title, children)
        case subtree: DocumentTree => 
          val ref = id(subtree.path / DocNames.treeTitle,"")
          val title = TreeUtil.extractText(subtree.title)
          val children = treeBookmarks(subtree, levels - 1)
          Bookmark(ref, title, children) 
      })
    }

    val depth = getDepth(tree, "pdf.bookmarks.depth")
    if (depth == 0) tree
    else {
      val bookmarks = BookmarkTree(treeBookmarks(tree, depth)) 
      val root = RootElement(Seq(bookmarks))
      val doc = new Document(tree.path / DocNames.bookmarks, root)
      tree.prependDocument(doc)
    }
  }
  
  def insertToc (tree: DocumentTree): DocumentTree = {

    def toBlockSequence (blocks: Seq[Element]): Seq[Block] = ((blocks map {
      case BulletList(items,_,_)      => toBlockSequence(items)
      case BulletListItem(blocks,_,_) => toBlockSequence(blocks)
      case Paragraph(Seq(link:CrossLink),opt) => Seq(Paragraph(Seq(link.copy(
          content = link.content :+ Leader() :+ PageNumberCitation(link.path.toString + "." + link.ref)
      )), opt))
    }).flatten)
    
    val depth = getDepth(tree, "pdf.toc.depth")
    if (depth == 0) tree
    else {
      val toc = toBlockSequence(TocGenerator.fromTree(tree, depth, tree.path / DocNames.toc, treeTitleDoc = Some(DocNames.treeTitle)))
      val root = RootElement(toc)
      val doc = new Document(tree.path / DocNames.toc, root)
      tree.prependDocument(doc)
    }
  }
      
  def prepareTree (tree: DocumentTree, bookmarks: Boolean = true, toc: Boolean = true, docTitles: Boolean = true, treeTitles: Boolean = true): DocumentTree = {
    val withDocTitles = if (docTitles) insertDocTitles(tree) else tree
    val withBookmarks = if (bookmarks) insertBookmarks(withDocTitles) else withDocTitles
    val withToc = if (toc) insertToc(withBookmarks) else withBookmarks
    if (treeTitles) addTreeTitles(withToc) else withToc
  }
  
  def renderFO (tree: DocumentTree, render: (DocumentTree, OutputConfig) => Unit) = {
      
    def append (sb: StringBuilder, result: ResultTree, src: DocumentTree): Unit = {
      src.navigatables.foreach {
        case d: Document => result.result(d.name).foreach(sb.append)
        case t: DocumentTree => result.subtree(t.name).foreach(append(sb, _, t))
      }
    }
    
    val foOutput = new StringOutputProvider(tree.path)
    render(prepareTree(tree), OutputConfig(foOutput, parallel = false, copyStaticFiles = false))
    val sb = new StringBuilder
    append(sb, foOutput.result, tree)
    sb.toString
  }
    
}

object FOforPDF extends FOforPDF
