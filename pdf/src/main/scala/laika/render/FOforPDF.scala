/*
 * Copyright 2012-2019 the original author or authors.
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

import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}
import laika.api.Render
import laika.ast.Path.Root
import laika.ast._
import laika.format.{PDF, XSLFO}
import laika.io.{RenderedTreeRoot, RenderedDocument, RenderedTree}
import laika.render.FOFormatter.{Bookmark, BookmarkTree, Leader, PageNumberCitation}
import laika.rewrite.nav.TocGenerator

/** Responsible for rendering the XSL-FO for an entire document tree
 *  as an interim result to be consumed by the PDF post processor.
 *  
 *  On top of the regular XSL-FO renderer in laika-core this renderer
 *  inserts tree titles, bookmarks and a table of contents into
 *  the document tree before rendering.
 * 
 *  @author Jens Halm
 */
class FOforPDF (config: Option[PDF.Config]) {


  private object DocNames {
    val treeTitle = "title"
    val toc = "_toc_"
  }
  
  /** Indicates whether the specified tree contains at least one document.
   */
  protected def hasDocuments (tree: DocumentTree): Boolean = 
    tree.content.exists(hasContent)
  
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
  def addTreeLinks (tree: DocumentTree): DocumentTree = {
    val newContent = tree.content map {
      case t: DocumentTree => addTreeLinks(t)
      case d: Document => d
    }
    val contentWithTitle =
      if (!hasDocuments(tree) || tree.titleDocument.isDefined) newContent
      else {
        val root = RootElement(Seq(InternalLinkTarget(Id(""))))
        val doc = Document(
          path = tree.path / DocNames.treeTitle,
          content = root,
          config = ConfigFactory.empty.withValue("title", ConfigValueFactory.fromAnyRef(SpanSequence(tree.title).extractText))
        )
        doc +: newContent
      }
    tree.copy(content = contentWithTitle)
  }

  /** Adds title elements for each document in the specified
   *  tree, including documents in subtrees. Document titles will be obtained either
   *  from a `Title` element in the document's content or from its configuration header.
   */
  def addDocLinks (tree: DocumentTree): DocumentTree =
    tree rewrite { _ => RewriteRules.forBlocks {
      case title: Title =>
        // toc directives will link to an empty id, not the id of the title element
        Replace(BlockSequence(Seq(title), Id("")))
      case root: RootElement if (root select { _.isInstanceOf[Title] }).isEmpty =>
        val insert = InternalLinkTarget(Id(""))
        Replace(RootElement(insert +: root.content)) 
    }}

  /** Generates bookmarks for the structure of the DocumentTree. Individual
   *  bookmarks can stem from tree or subtree titles, document titles or
   *  document sections, depending on which recursion depth is configured.
   *  The configuration key for setting the recursion depth is `pdf.bookmarks.depth`.
   *
   *  @param result the rendered result tree to generate bookmarks for
   *  @param depth the recursion depth through trees, documents and sections
   *  @return a fragment map containing the generated bookmarks
   */
  def generateBookmarks (result: RenderedTreeRoot, depth: Int): Map[String, Element] = {

    def sectionBookmarks (path: Path, sections: Seq[SectionInfo], levels: Int): Seq[Bookmark] =
      if (levels == 0) Nil
      else for (section <- sections) yield {
        val title = section.title.extractText
        val children = sectionBookmarks(path, section.content, levels - 1)
        Bookmark(section.id, PathInfo.fromPath(path, result.rootTree.path), title, children)
      }

    def treeBookmarks (tree: RenderedTree, levels: Int): Seq[Bookmark] = {
      if (levels == 0) Nil
      else {
        (for (nav <- tree.navigationContentAfterTitle if hasContent(nav)) yield nav match {
          case doc: RenderedDocument if doc.name == DocNames.treeTitle || doc.name == DocNames.toc => Seq()
          case doc: RenderedDocument =>
            val title = if (doc.title.nonEmpty) SpanSequence(doc.title).extractText else doc.name
            val children = sectionBookmarks(doc.path, doc.sections, levels - 1)
            Seq(Bookmark("", PathInfo.fromPath(doc.path, result.rootTree.path), title, children))
          case subtree: RenderedTree =>
            val title = if (subtree.title.nonEmpty) SpanSequence(subtree.title).extractText else subtree.name
            val children = treeBookmarks(subtree, levels - 1)
            Seq(Bookmark("", PathInfo.fromPath(subtree.path / DocNames.treeTitle, result.rootTree.path), title, children))
        }).flatten
      }
    }

    if (depth == 0) Map()
    else Map("bookmarks" -> BookmarkTree(treeBookmarks(result.rootTree, depth)))
  }

  /** Inserts a table of content into the specified document tree.
   *  The recursion depth can be set with the configuration key
   *  `pdf.toc.depth`.
   */
  def insertToc (tree: DocumentTree, depth: Int, title: Option[String]): DocumentTree = {

    def toBlockSequence (blocks: Seq[Element]): Seq[Block] = blocks flatMap {
      case BulletList(items, _, _) => toBlockSequence(items)
      case BulletListItem(blocks, _, _) => toBlockSequence(blocks)
      case Paragraph(Seq(link: CrossLink), opt) => Seq(Paragraph(Seq(link.copy(
        content = link.content :+ Leader() :+ PageNumberCitation(link.ref, link.path)
      )), opt))
    }

    val toc = toBlockSequence(TocGenerator.fromTree(tree, depth, tree.path / DocNames.toc, treeTitleDoc = Some(DocNames.treeTitle)))
    val root = title.fold(RootElement(toc)){ title => RootElement(Title(Seq(Text(title))) +: toc) }
    val doc = Document(tree.path / DocNames.toc, root)
    tree.copy(content = doc +: tree.content)
  }

  /** Prepares the document tree before rendering the interim XSL-FO
   *  output. Preparation may include insertion of tree or document titles, PDF bookmarks
   *  and a table of content, depending on configuration.
   */
  def prepareTree (tree: DocumentTree): DocumentTree = {
    val pdfConfig = config getOrElse configFromTree(tree.config)
    val insertLinks = pdfConfig.bookmarkDepth > 0 || pdfConfig.tocDepth > 0
    val withoutTemplates = tree.copy(templates = Seq(TemplateDocument(Path.Root / "default.template.fo",
        TemplateRoot(List(TemplateContextReference("document.content"))))))
    val withDocTitles = if (insertLinks) addDocLinks(withoutTemplates) else withoutTemplates
    val withToc = if (pdfConfig.tocDepth > 0) insertToc(withDocTitles, pdfConfig.tocDepth, pdfConfig.tocTitle) else withDocTitles
    if (insertLinks) addTreeLinks(withToc) else withToc
    
    // TODO - 0.12 - split this class into PDFNavigation, PDF.Config factory and FOConcatenation in render.pdf sub-package
  }
  
  private def configFromTree (treeConfig: Config): PDF.Config = {
    val defaults = PDF.Config.default

    def getOpt [T](key: String, read: String => T): Option[T] =
      if (treeConfig.hasPath(key)) Some(read(key)) else None

    val bookmarkDepth = getOpt("pdf.bookmarks.depth", treeConfig.getInt).getOrElse(defaults.bookmarkDepth)
    val tocDepth = getOpt("pdf.toc.depth", treeConfig.getInt).getOrElse(defaults.tocDepth)
    val tocTitle = getOpt("pdf.toc.title", treeConfig.getString).orElse(defaults.tocTitle)

    PDF.Config(bookmarkDepth, tocDepth, tocTitle)
  }
  
  /** Renders the XSL-FO that serves as a basis for producing the final PDF output.
   *  The result should include the output from rendering the documents in the 
   *  specified tree as well as any additional insertions like bookmarks or
   *  table of content. For this the specified `DocumentTree` instance may get
   *  modified before passing it to the given render function, depending on
   *  configuration settings.
   *  
   *  @param result the tree of rendered XSL-FO documents
   *  @param defaultTemplateRoot the template to apply the concatenated FO documents to
   *  @return the rendered merged XSL-FO as a String 
   */
  def renderFO (result: RenderedTreeRoot, defaultTemplateRoot: TemplateRoot): String = {
    
    val pdfConfig = config getOrElse configFromTree(result.config)
    
//    def getDefaultTemplate: TemplateDocument = { // TODO - 0.12 - ensure the right template is already passed in
//      val templateName = "default.template.fo"
//      result.selectTemplate(Path.Current / templateName)
//        .getOrElse(TemplateDocument(Path.Root / templateName, defaultTemplateRoot))
//    }
    
    def concatDocuments (resultTree: RenderedTree): String = { // TODO - 0.12 - move to core as a util

      def append (sb: StringBuilder, result: RenderedTree): Unit = {

        result.content foreach {
          case d: RenderedDocument => sb.append(d.content)
          case t: RenderedTree => append(sb, t)
          case _ => ()
        }
      }
      
      val sb = new StringBuilder
      append(sb, resultTree) // TODO - improve formatting
      sb.toString
    }

    def resolveCoverImagePath: Config =
      if (result.config.hasPath("pdf.coverImage")) {
        val uri = result.config.getString("pdf.coverImage")
        val resolvedUri = PathInfo.fromURI(uri, Root).fold(uri)(_.absolute.toString)
        result.config.withValue("pdf.coverImage", ConfigValueFactory.fromAnyRef(resolvedUri))
      } else result.config

    
    def applyTemplate(foString: String, template: TemplateDocument): String = {
      val foElement = RawContent(Seq("fo"), foString)
      val finalConfig = resolveCoverImagePath
      val finalDoc = Document(Path.Root / "merged.fo", RootElement(Seq(foElement)), fragments = generateBookmarks(result, pdfConfig.bookmarkDepth), config = finalConfig)
      val templateApplied = template.applyTo(finalDoc)
      Render as XSLFO from templateApplied toString
    }

    val defaultTemplate = TemplateDocument(Path.Root / "default.template.fo", defaultTemplateRoot)
    val foString = concatDocuments(result.rootTree)
    applyTemplate(foString, defaultTemplate)
  }

    
}


