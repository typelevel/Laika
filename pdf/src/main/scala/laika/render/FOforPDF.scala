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

import laika.api.Render
import laika.io.OutputProvider.OutputConfig
import laika.io.OutputProvider.ResultTree
import laika.io.OutputProvider.StringOutputProvider
import laika.render.FOWriter._
import laika.tree.Documents._
import laika.tree.Elements._
import laika.tree.TreeUtil
import laika.tree.TocGenerator
import laika.tree.Templates.TemplateDocument
import laika.tree.Templates.TemplateRoot
import laika.tree.Templates.TemplateContextReference

/** Responsible for rendering the XSL-FO for an entire document tree
 *  as an interim result to be consumed by the PDF post processor.
 *  
 *  On top of the regular XSL-FO renderer in laika-core this renderer
 *  inserts tree titles, bookmarks and a table of contents into
 *  the document tree before rendering.
 * 
 *  @author Jens Halm
 */
class FOforPDF (config: PDFConfig) {

  object DocNames {
    val treeTitle = "_title_"
    val toc = "_toc_"
    val bookmarks = "_bookmarks_"
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
  
  def addTreeTitles (tree: DocumentTree): DocumentTree = {
    val treeWithTitle = if (!hasDocuments(tree) || tree.title.isEmpty) tree
    else {
      val title = Header(1, tree.title, Styles("treeTitle") + Id(""))
      val root = RootElement(Seq(title))
      val doc = new Document(tree.path / DocNames.treeTitle, root)
      tree.prependDocument(doc)
    }
    treeWithTitle.mapSubtrees(addTreeTitles)
  }
  
  def insertDocTitles (tree: DocumentTree): DocumentTree =
    tree rewrite { context => {
      case title: Title =>
        // toc directives will link to an empty id, not the id of the title element
        Some(BlockSequence(Seq(title), Id("")))
      case root: RootElement if ((root select { _.isInstanceOf[Title] }).isEmpty) => 
        Some(RootElement(Title(context.document.title, Id("")) +: root.content))
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
          content = link.content :+ Leader() :+ PageNumberCitation(link.ref, link.path)
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
      
  def prepareTree (tree: DocumentTree): DocumentTree = {
    val withoutTemplates = tree.withoutTemplates.withTemplate(new TemplateDocument(Root / "default.template.fo", 
        TemplateRoot(List(TemplateContextReference("document.content")))))
    val withDocTitles = if (config.docTitles) insertDocTitles(withoutTemplates) else withoutTemplates
    val withBookmarks = if (config.bookmarks) insertBookmarks(withDocTitles) else withDocTitles
    val withToc = if (config.toc) insertToc(withBookmarks) else withBookmarks
    if (config.treeTitles) addTreeTitles(withToc) else withToc
  }
  
  def renderFO (tree: DocumentTree, render: (DocumentTree, OutputConfig) => Unit) = {
      
    def getDefaultTemplate = {
      val templateName = "default.template.fo"
      tree.selectTemplate(Current / templateName)
        .getOrElse(new TemplateDocument(Root / templateName, XSLFO.defaultTemplate))
    }
    
    def append (sb: StringBuilder, result: ResultTree, src: DocumentTree): Unit = {
      
      def baseName(docName: String) = docName.takeWhile(_ != '.')
          
      val children = if (src.navigatables.nonEmpty) src.navigatables else src.documents ++ src.subtrees
      children foreach {
        case d: Document => result.result(baseName(d.name) + ".fo").foreach(sb.append)
        case t: DocumentTree => result.subtree(t.name).foreach(append(sb, _, t))
      }
    }

    def renderDocuments: String = {
      val foOutput = new StringOutputProvider(tree.path)
      val preparedTree = prepareTree(tree)
      render(preparedTree, OutputConfig(foOutput, parallel = false, copyStaticFiles = false))
      
      val sb = new StringBuilder
      append(sb, foOutput.result, preparedTree) // TODO - improve formatting
      sb.toString
    }
    
    def applyTemplate(foString: String, template: TemplateDocument): String = {
      val result = RawContent(Seq("fo"), foString)
      val finalDoc = new Document(Root / "merged.fo", RootElement(Seq(result)))
      val templateApplied = template.rewrite(DocumentContext(finalDoc))
      Render as XSLFO from templateApplied toString
    }
    
    val defaultTemplate = getDefaultTemplate
    val foString = renderDocuments
    applyTemplate(foString, defaultTemplate)
  }
    
}

object FOforPDF extends FOforPDF(PDFConfig.default)
