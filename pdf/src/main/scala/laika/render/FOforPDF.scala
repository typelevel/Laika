/*
 * Copyright 2014-2016 the original author or authors.
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
import laika.rewrite.TreeUtil
import laika.tree.Documents._
import laika.tree.Elements._
import laika.tree.TocGenerator
import laika.tree.Templates.TemplateDocument
import laika.tree.Templates.TemplateRoot
import laika.tree.Templates.TemplateContextReference
import com.typesafe.config.Config

/** Responsible for rendering the XSL-FO for an entire document tree
 *  as an interim result to be consumed by the PDF post processor.
 *  
 *  On top of the regular XSL-FO renderer in laika-core this renderer
 *  inserts tree titles, bookmarks and a table of contents into
 *  the document tree before rendering.
 * 
 *  @author Jens Halm
 */
class FOforPDF (config: Option[PDFConfig]) {

  private object DocNames {
    val treeTitle = "_title_"
    val toc = "_toc_"
  }
  
  /** Indicates whether the specified tree contains at least one document.
   */
  protected def hasDocuments (tree: DocumentTree): Boolean = tree.documents.nonEmpty || tree.subtrees.exists(hasDocuments)
  
  /** Indicates whether the specified navigatable contains at least one document.
   */
  protected def hasContent (nav: Navigatable): Boolean = nav match {
    case _: Document => true
    case tree: DocumentTree => hasDocuments(tree)
  }
  
  /** Adds title elements for each tree and subtree in the specified
   *  root tree. Tree titles can be specified in the configuration file
   *  for each tree.
   */
  def addTreeTitles (linksOnly: Boolean)(tree: DocumentTree): DocumentTree = {
    val treeWithTitle = if (!hasDocuments(tree)) tree
    else {
      val title = if (linksOnly || tree.title.isEmpty) InternalLinkTarget(Id(""))
                  else Header(1, tree.title, Styles("treeTitle") + Id(""))
      val root = RootElement(Seq(title))
      val doc = new Document(tree.path / DocNames.treeTitle, root)
      tree.prependDocument(doc)
    }
    treeWithTitle.mapSubtrees(addTreeTitles(linksOnly))
  }
  
  /** Adds title elements for each document in the specified
   *  tree, including documents in subtrees. Document titles will be obtained either
   *  from a `Title` element in the document's content or from its configuration header.
   */
  def insertDocTitles (linksOnly: Boolean)(tree: DocumentTree): DocumentTree =
    tree rewrite { context => {
      case title: Title =>
        // toc directives will link to an empty id, not the id of the title element
        Some(BlockSequence(Seq(title), Id("")))
      case root: RootElement if ((root select { _.isInstanceOf[Title] }).isEmpty) => 
        val insert = if (linksOnly || context.document.title.isEmpty) InternalLinkTarget(Id(""))
                     else Title(context.document.title, Id(""))
        Some(RootElement(insert +: root.content))
    }}
    
  /** Generates bookmarks for the structure of the DocumentTree. Individual
   *  bookmarks can stem from tree or subtree titles, document titles or
   *  document sections, depending on which recursion depth is configured.
   *  The configuration key for setting the recursion depth is `pdf.bookmarks.depth`.
   *  
   *  @param root the document tree to generate bookmarks for
   *  @return a fragment map containing the generated bookmarks
   */
  def generateBookmarks (root: DocumentTree, depth: Int): Map[String, Element] = {

    def sectionBookmarks (path: Path, sections: Seq[SectionInfo], levels: Int): Seq[Bookmark] = 
      if (levels == 0) Nil
      else for (section <- sections) yield {
        val title = section.title.text
        val children = sectionBookmarks(path, section.content, levels - 1)
        Bookmark(section.id, PathInfo.fromPath(path, root.path), title, children)
      }
    
    def treeBookmarks (tree: DocumentTree, levels: Int): Seq[Bookmark] = {
      def navigatables(tree: DocumentTree) = if (tree.navigatables.nonEmpty) tree.navigatables else tree.documents ++ tree.subtrees
      if (levels == 0) Nil
      else (for (nav <- navigatables(tree) if hasContent(nav)) yield nav match {
        case doc: Document if doc.name == DocNames.treeTitle || doc.name == DocNames.toc => Seq()
        case doc: Document =>
          val title = if (doc.title.nonEmpty) TreeUtil.extractText(doc.title) else doc.name
          val children = sectionBookmarks(doc.path, doc.sections, levels - 1)
          Seq(Bookmark("", PathInfo.fromPath(doc.path, root.path), title, children))
        case subtree: DocumentTree => 
          val title = if (subtree.title.nonEmpty) TreeUtil.extractText(subtree.title) else subtree.name
          val children = treeBookmarks(subtree, levels - 1)
          Seq(Bookmark("", PathInfo.fromPath(subtree.path / DocNames.treeTitle, root.path), title, children)) 
      }).flatten
    }

    if (depth == 0) Map()
    else Map("bookmarks" -> BookmarkTree(treeBookmarks(root, depth))) 
  }
  
  /** Inserts a table of content into the specified document tree.
   *  The recursion depth can be set with the configuration key
   *  `pdf.toc.depth`.
   */
  def insertToc (tree: DocumentTree, depth: Int, title: Option[String]): DocumentTree = {

    def toBlockSequence (blocks: Seq[Element]): Seq[Block] = ((blocks map {
      case BulletList(items,_,_)      => toBlockSequence(items)
      case BulletListItem(blocks,_,_) => toBlockSequence(blocks)
      case Paragraph(Seq(link:CrossLink),opt) => Seq(Paragraph(Seq(link.copy(
          content = link.content :+ Leader() :+ PageNumberCitation(link.ref, link.path)
      )), opt))
    }).flatten)
    
    val toc = toBlockSequence(TocGenerator.fromTree(tree, depth, tree.path / DocNames.toc, treeTitleDoc = Some(DocNames.treeTitle)))
    val root = title.fold(RootElement(toc)){ title => RootElement(Title(Seq(Text(title))) +: toc) }
    val doc = new Document(tree.path / DocNames.toc, root)
    tree.prependDocument(doc)
  }
      
  /** Prepares the document tree before rendering the interim XSL-FO
   *  output. Preparation may include insertion of tree or document titles
   *  and a table of content, depending on configuration.
   */
  def prepareTree (tree: DocumentTree, config: PDFConfig): DocumentTree = {
    val insertLinks = config.bookmarkDepth > 0 || config.tocDepth > 0
    val withoutTemplates = tree.withoutTemplates.withTemplate(new TemplateDocument(Root / "default.template.fo", 
        TemplateRoot(List(TemplateContextReference("document.content")))))
    val withDocTitles = if (config.insertTitles || insertLinks) insertDocTitles(!config.insertTitles)(withoutTemplates) else withoutTemplates
    val withToc = if (config.tocDepth > 0) insertToc(withDocTitles, config.tocDepth, config.tocTitle) else withDocTitles
    if (config.insertTitles || insertLinks) addTreeTitles(!config.insertTitles)(withToc) else withToc
  }
  
  /** Renders the XSL-FO that serves as a basis for producing the final PDF output.
   *  The result should include the output from rendering the documents in the 
   *  specified tree as well as any additional insertions like bookmarks or
   *  table of content. For this the specified `DocumentTree` instance may get
   *  modified before passing it to the given render function, depending on
   *  configuration settings.
   *  
   *  @param tree the document tree serving as input for the renderer
   *  @param render the actual render function for producing the XSL-FO output
   *  @return the rendered XSL-FO as a String 
   */
  def renderFO (tree: DocumentTree, render: (DocumentTree, OutputConfig) => Unit): String = {
    
    val pdfConfig = config getOrElse {
      val defaults = PDFConfig.default
      
      tree.config.fold(defaults) { treeConfig =>
        
        def getOpt [T](key: String, read: String => T): Option[T] = 
          if (treeConfig.hasPath(key)) Some(read(key)) else None
        
        val insertTitles = getOpt("pdf.insertTitles", treeConfig.getBoolean).getOrElse(defaults.insertTitles)
        val bookmarkDepth = getOpt("pdf.bookmarks.depth", treeConfig.getInt).getOrElse(defaults.bookmarkDepth)
        val tocDepth = getOpt("pdf.toc.depth", treeConfig.getInt).getOrElse(defaults.tocDepth)
        val tocTitle = getOpt("pdf.toc.title", treeConfig.getString).orElse(defaults.tocTitle)
 
        PDFConfig(insertTitles, bookmarkDepth, tocDepth, tocTitle)
      }
    }
    
    def getDefaultTemplate: TemplateDocument = {
      val templateName = "default.template.fo"
      tree.selectTemplate(Current / templateName)
        .getOrElse(new TemplateDocument(Root / templateName, XSLFO.defaultTemplate))
    }
    
    def append (sb: StringBuilder, result: ResultTree, src: DocumentTree): Unit = {
      
      def baseName(docName: String): String = docName.takeWhile(_ != '.')
          
      val children = if (src.navigatables.nonEmpty) src.navigatables else src.documents ++ src.subtrees
      children foreach {
        case d: Document => result.result(baseName(d.name) + ".fo").foreach(sb.append)
        case t: DocumentTree => result.subtree(t.name).foreach(append(sb, _, t))
      }
    }

    def renderDocuments(preparedTree: DocumentTree): String = {
      val foOutput = new StringOutputProvider(preparedTree.path)
      render(preparedTree, OutputConfig(foOutput, parallel = false, copyStaticFiles = false))
      
      val sb = new StringBuilder
      append(sb, foOutput.result, preparedTree) // TODO - improve formatting
      sb.toString
    }
    
    def applyTemplate(foString: String, template: TemplateDocument, tree: DocumentTree): String = {
      val result = RawContent(Seq("fo"), foString)
      val finalDoc = new Document(Root / "merged.fo", RootElement(Seq(result)), fragments = generateBookmarks(tree, pdfConfig.bookmarkDepth))
      val templateApplied = template.rewrite(DocumentContext(finalDoc))
      Render as XSLFO from templateApplied toString
    }
    
    val defaultTemplate = getDefaultTemplate
    val preparedTree = prepareTree(tree, pdfConfig)
    val foString = renderDocuments(preparedTree)
    applyTemplate(foString, defaultTemplate, preparedTree)
  }
    
}

/** The default FOforPDF instance using a PDFConfig with all 
 *  optional features like document titles, bookmarks and table
 *  of content enabled.
 */
object FOforPDF extends FOforPDF(None)
