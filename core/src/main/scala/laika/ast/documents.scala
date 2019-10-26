/*
* Copyright 2013-2016 the original author or authors.
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

import java.time.Instant
import java.util.Locale

import laika.config.Config.ConfigResult
import laika.config.{Config, ConfigDecoder, ConfigError, ConfigValue, DefaultKey, InvalidType, ObjectValue, Traced, ValidationError}
import laika.ast.Path.Root
import laika.bundle.UnresolvedConfig
import laika.rewrite.TemplateRewriter
import laika.rewrite.link.LinkTargetProvider
import laika.rewrite.link.LinkTargets._
import laika.rewrite.nav.AutonumberConfig

import scala.annotation.tailrec
import scala.util.Try


/** A navigatable object is anything that has an associated path.
 */
trait Navigatable {

  def path: Path

  /** The local name of this navigatable.
   */
  lazy val name: String = path.name

}

/** A titled, positional element in the document tree.
  */
sealed trait TreeContent extends Navigatable {

  /** The title of this element or an empty sequence in case
    * this element does not have a title.
   */
  def title: Seq[Span]

  /** The configuration associated with this element.
    */
  def config: Config

  /** The position of this element within the document tree.
    */
  def position: TreePosition

  /** All link targets that can get referenced from anywhere
    * in the document tree.
    */
  def globalLinkTargets: Map[Selector, TargetResolver]

  /** Selects a link target by the specified selector
   *  if it is defined somewhere in a document inside this document tree.
   */
  def selectTarget (selector: Selector): Option[TargetResolver] = globalLinkTargets.get(selector)

  protected def titleFromConfig: Option[Seq[Span]] = {
    config.get[String]("title").toOption.map { titleString =>
      val title = List(Text(titleString))
      val autonumberConfig = AutonumberConfig.fromConfig(config)
      val autonumberEnabled = autonumberConfig.documents && position.depth < autonumberConfig.maxDepth
      if (autonumberEnabled) position.toSpan +: title
      else title
    }
  }

}


/** A template document containing the element tree of a parsed template and its extracted
 *  configuration section (if present).
 */
case class TemplateDocument (path: Path, content: TemplateRoot, config: UnresolvedConfig = UnresolvedConfig.empty) extends Navigatable {

  /** Applies this template to the specified document, replacing all
   *  span and block resolvers in the template with the final resolved element.
   */
  def applyTo (document: Document): Either[ConfigError, Document] = TemplateRewriter.applyTemplate(DocumentCursor(document), this)

}

/** A temporary structure usually not exposed to user code.
  * It holds a document with an empty Config instance and its actual config
  * (obtained from a header section if present) in unresolved form, as it
  * needs to be resolved based on a fallback configuration later.
  */
case class UnresolvedDocument (document: Document, config: UnresolvedConfig)

/** Captures information about a document section, without its content.
 */
case class SectionInfo (id: String, title: TitleInfo, content: Seq[SectionInfo]) extends Element with ElementContainer[SectionInfo]

/** Represents a section title.
 */
case class TitleInfo (content: Seq[Span], options: Options = NoOpt) extends SpanContainer {
  type Self = TitleInfo
  def withContent (newContent: Seq[Span]): TitleInfo = copy(content = newContent)
  def withOptions (options: Options): TitleInfo = copy(options = options)
}

/** Metadata associated with a document.
  */
case class DocumentMetadata (identifier: Option[String] = None, authors: Seq[String] = Nil, language: Option[Locale] = None, date: Option[Instant] = None)

object DocumentMetadata {
  
  implicit val localeDecoder: ConfigDecoder[Locale] = ConfigDecoder.string.flatMap { lang => 
    Try(Locale.forLanguageTag(lang)).toEither.left.map(e => ValidationError(e.getMessage)) 
  }

  def toInstant[C] (in: ConfigResult[Option[String]])(f: String => ConfigResult[Instant]): ConfigResult[Option[Instant]] = 
    in.flatMap(_.fold[ConfigResult[Option[Instant]]](Right(None))(f(_).map(Some(_))))
  
  implicit val decoder: ConfigDecoder[DocumentMetadata] = {
    case Traced(ov: ObjectValue, _) =>
      val config = ov.toConfig
      for {
        identifier <- config.getOpt[String]("identifier")
        author     <- config.getOpt[String]("author")
        authors    <- config.get[Seq[String]]("authors", Nil)
        lang       <- config.getOpt[Locale]("language")
        date       <- toInstant(config.getOpt[String]("date")) { ds => 
                        Try(Instant.parse(ds)).toEither.left.map(f => ValidationError(s"Invalid date format: ${f.getMessage}")) 
                      }
      } yield {
        DocumentMetadata(identifier, authors ++ author.toSeq, lang, date)
      }

    case Traced(invalid: ConfigValue, _) => Left(InvalidType("Object", invalid))
  }
  implicit val defaultKey: DefaultKey[DocumentMetadata] = DefaultKey("metadata")

  /** Tries to obtain the document metadata
    * from the specified configuration instance or returns
    * an empty instance.
    */
  def fromConfig (config: Config): DocumentMetadata = config.get[DocumentMetadata].toOption.getOrElse(DocumentMetadata()) 

}

/** The position of an element within a document tree.
  *
  * @param toSeq the positions (one-based) of each nesting level of this
  *              position (an empty sequence for the root position)
  */
case class TreePosition(toSeq: Seq[Int]) extends Ordered[TreePosition] {

  override def toString: String = toSeq.mkString(".")

  /** This tree position as a span that can get rendered
    * as part of a numbered title for example.
    */
  def toSpan: Span = SectionNumber(toSeq)

  /** The depth (or nesting level) of this position within the document tree.
    */
  def depth: Int = toSeq.size

  /** Creates a position instance for a child of this element.
    */
  def forChild(childPos: Int) = TreePosition(toSeq :+ childPos)

  def compare (other: TreePosition): Int = {

    @tailrec
    def compare (pos1: Seq[Int], pos2: Seq[Int]): Int = (pos1.headOption, pos2.headOption) match {
      case (Some(a), Some(b)) => a.compare(b) match {
        case 0 => compare(pos1.tail, pos2.tail)
        case nonZero => nonZero
      }
      case _ => 0
    }

    val maxLen = Math.max(toSeq.length, other.toSeq.length)
    compare(toSeq.padTo(maxLen, 0), other.toSeq.padTo(maxLen, 0))
  }

}

object TreePosition {
  def root = TreePosition(Seq())
}

/** The structure of a markup document.
  */
trait DocumentStructure { this: TreeContent =>

  /** The tree model obtained from parsing the markup document.
    */
  def content: RootElement

  private def findRoot: Seq[Block] = {
    (content select {
      case RootElement(TemplateRoot(_,_) :: Nil, _) => false
      case RootElement(_, _) => true
      case _ => false
    }).headOption map { case RootElement(content, _) => content } getOrElse Nil
  }

  /** The title of this document, obtained from the document
    * structure or from the configuration. In case no title
    * is defined in either of the two places the sequence will
    * be empty.
    */
  def title: Seq[Span] = {

    def titleFromTree = (RootElement(findRoot) collect {
      case Title(content, _) => content
    }).headOption

    titleFromConfig.orElse(titleFromTree).getOrElse(Seq())
  }

  /** The section structure of this document based on the hierarchy
   *  of headers found in the original text markup.
   */
  lazy val sections: Seq[SectionInfo] = {

    def extractSections (blocks: Seq[Block]): Seq[SectionInfo] = {
      blocks collect {
        case Section(Header(_,header,Id(id)), content, _) =>
          SectionInfo(id, TitleInfo(header), extractSections(content))
      }
    }
    extractSections(findRoot)
  }

  /** All link targets of this document, including global and local targets.
   */
  lazy val linkTargets: LinkTargetProvider = new LinkTargetProvider(path,content)

  /** All link targets that can get referenced from anywhere
    * in the document tree.
    */
  lazy val globalLinkTargets: Map[Selector, TargetResolver] = linkTargets.global

}

/** The structure of a document tree.
  */
trait TreeStructure { this: TreeContent =>

  import Path.Current

  /** The actual document tree that this ast structure represents.
    */
  def targetTree: DocumentTree

  /** The content of this tree structure, containing
    * all markup documents and subtrees, except for the (optional) title document.
    */
  def content: Seq[TreeContent]
  
  /** The title of this tree, obtained from configuration.
   */
  lazy val title: Seq[Span] = titleDocument.map(_.title).orElse(titleFromConfig).getOrElse(Nil)

  /** The title document for this tree, if present.
    *
    * A document with the base name `title` and the corresponding
    * suffix for the input markup, e.g. `title.md` for Markdown,
    * can be used as an introductory section for a chapter represented
    * by a directory tree.
    */
  def titleDocument: Option[Document]

  /** All templates on this level of the tree hierarchy that might
    * get applied to a document when it gets rendered.
    */
  def templates: Seq[TemplateDocument]

  /** Selects a document from this tree or one of its subtrees by the specified path.
   *  The path needs to be relative.
   */
  def selectDocument (path: String): Option[Document] = selectDocument(Path(path))

  /** Selects a document from this tree or one of its subtrees by the specified path.
   *  The path needs to be relative.
   */
  def selectDocument (path: Path): Option[Document] = path match {
    case Current / localName => content.collectFirst { case d: Document if d.path.name == localName => d }
    case base / localName    => selectSubtree(base) flatMap (_.selectDocument(localName))
    case _                   => None
  }

  /** Selects a template from this tree or one of its subtrees by the specified path.
   *  The path needs to be relative.
   */
  def selectTemplate (path: String): Option[TemplateDocument] = selectTemplate(Path(path))

  /** Selects a template from this tree or one of its subtrees by the specified path.
   *  The path needs to be relative.
   */
  def selectTemplate (path: Path): Option[TemplateDocument] = path match {
    case Current / localName => templates.find(_.path.name == localName)
    case base / localName => selectSubtree(base) flatMap (_.selectTemplate(localName))
    case _ => None
  }
  
  private val defaultTemplatePathBase: Path = Path.Current / "default.template.<format>"
  
  /** Selects the template with the name `default.template.&lt;suffix&gt;` for the 
    * specified format suffix from this level of the document tree.
    */
  def getDefaultTemplate (formatSuffix: String): Option[TemplateDocument] = {
    selectTemplate(defaultTemplatePathBase.withSuffix(formatSuffix))
  }

  /** Create a new document tree that contains the specified template as the default.
    */
  def withDefaultTemplate (template: TemplateRoot, formatSuffix: String): DocumentTree = {
    targetTree.copy(templates = targetTree.templates :+ 
      TemplateDocument(defaultTemplatePathBase.withSuffix(formatSuffix), template))
  }

  /** Selects a subtree of this tree by the specified path.
   *  The path needs to be relative and it may point to a deeply nested
   *  subtree, not just immediate children.
   */
  def selectSubtree (path: String): Option[DocumentTree] = selectSubtree(Path(path))

  /** Selects a subtree of this tree by the specified path.
   *  The path needs to be relative and it may point to a deeply nested
   *  subtree, not just immediate children.
   */
  def selectSubtree (path: Path): Option[DocumentTree] = path match {
    case Current => Some(targetTree)
    case Current / localName => content.collectFirst { case t: DocumentTree if t.path.name == localName => t }
    case base / localName => selectSubtree(base) flatMap (_.selectSubtree(localName))
    case _ => None
  }

  /** All link targets that can get referenced from anywhere
    * in the document tree.
    */
  lazy val globalLinkTargets: Map[Selector, TargetResolver] = {
    content.flatMap(_.globalLinkTargets.toList).groupBy(_._1) collect {
      case (selector, Seq((_, target))) => (selector, target)
      case (s@UniqueSelector(sName), _) => (s, DuplicateTargetResolver(path, sName))
    }
  }

}

/** Generically builds a tree structure out of a flat sequence of elements with a `Path` property that 
  * signifies the position in the tree. Essentially factors recursion out of the tree building process.
  */
object TreeBuilder {

  /** Builds a tree structure from the specified leaf elements, using the given builder function.
    * The function will be invoked for each node recursively, with the path for the node to build
    * and the child nodes that are immediate children of the node to build.
    */
  def build[C <: Navigatable, T <: C] (content: Seq[C], buildNode: (Path, Seq[C]) => T): T = {

    def toMap (items: Iterable[C]): Map[Path, C] = items.map(c => (c.path, c)).toMap
    
    def buildNodes (depth: Int, contentMap: Map[Path, C]): Seq[T] = {

      val newNodes = content
        .filter(_.path.parent.depth >= depth)
        .map(p => Path(Root, p.path.components.take(depth + 1)))
        .distinct
        .groupBy(_.parent)
        .map { 
          case (parent, contentPaths) => buildNode(parent, contentPaths.map(contentMap))
        }

      if (depth == 0) newNodes.toSeq
      else buildNodes(depth - 1, contentMap ++ toMap(newNodes))
    }

    if (content.isEmpty) buildNode(Root, Nil)
    else {
      val maxPathLength = content.map(_.path.parent.depth).max
      buildNodes(maxPathLength, toMap(content)).head
    }
  }

}

/** Base type for all document type descriptors.
  */
sealed abstract class DocumentType

/** Base type for all document type descriptors for text input.
  */
sealed abstract class TextDocumentType extends DocumentType

/** Provides all available DocumentTypes.
  */
object DocumentType {

  /** A configuration document in HOCON format.
    */
  case object Config extends TextDocumentType

  /** A text markup document produced by a parser.
    */
  case object Markup extends TextDocumentType

  /** A template document that might get applied
    *  to a document when it gets rendered.
    */
  case object Template extends TextDocumentType

  /** A style sheet that needs to get passed
    *  to a renderer.
    */
  case class StyleSheet (format: String) extends TextDocumentType

  /** A static file that needs to get copied
    *  over to the output target.
    */
  case object Static extends DocumentType

  /** A document that should be ignored and neither
    *  get processed nor copied.
    */
  case object Ignored extends DocumentType

}


/** Represents a single document and provides access
 *  to the document content and structure as well
 *  as hooks for triggering rewrite operations.
 *
 *  @param path the full, absolute path of this document in the (virtual) document tree
 *  @param content the tree model obtained from parsing the markup document
 *  @param fragments separate named fragments that had been extracted from the content
 *  @param config the configuration for this document
 *  @param position the position of this document inside a document tree hierarchy, expressed as a list of Ints
 */
case class Document (path: Path,
                     content: RootElement,
                     fragments: Map[String, Element] = Map.empty,
                     config: Config = Config.empty,
                     position: TreePosition = TreePosition(Seq())) extends DocumentStructure with TreeContent {

  /** Returns a new, rewritten document model based on the specified rewrite rules.
   *
   *  If the rule is not defined for a specific element or the rule returns
   *  a `Retain` action as a result the old element remains in the tree unchanged. 
   * 
   *  If it returns `Remove` then the node gets removed from the ast,
   *  if it returns `Replace` with a new element it will replace the old one. 
   *
   *  The rewriting is performed bottom-up (depth-first), therefore
   *  any element container passed to the rule only contains children which have already
   *  been processed.
   */
  def rewrite (rules: RewriteRules): Document = DocumentCursor(this).rewriteTarget(rules)

}

/** Represents a tree with all its documents, templates, configurations and subtrees.
 *
 *  @param path the full, absolute path of this (virtual) document tree
 *  @param content the markup documents and subtrees
 *  @param titleDocument the optional title document of this tree               
 *  @param templates all templates on this level of the tree hierarchy that might get applied to a document when it gets rendered
 *  @param config the configuration associated with this tree
 *  @param position the position of this tree inside a document ast hierarchy, expressed as a list of Ints
 */
case class DocumentTree (path: Path,
                         content: Seq[TreeContent],
                         titleDocument: Option[Document] = None,
                         templates: Seq[TemplateDocument] = Nil,
                         config: Config = Config.empty,
                         position: TreePosition = TreePosition.root) extends TreeStructure with TreeContent {

  val targetTree: DocumentTree = this

  /** Returns a new tree, with all the document models contained in it
   *  rewritten based on the specified rewrite rules.
   *
   *  If the rule is not defined for a specific element or the rule returns
   *  a `Retain` action as a result the old element remains in the tree unchanged. 
   * 
   *  If it returns `Remove` then the node gets removed from the ast,
   *  if it returns `Replace` with a new element it will replace the old one. 
   *
   *  The rewriting is performed bottom-up (depth-first), therefore
   *  any element container passed to the rule only contains children which have already
   *  been processed.
   *
   *  The specified factory function will be invoked for each document contained in this
   *  tree and must return the rewrite rules for that particular document.
   */
  def rewrite (rules: DocumentCursor => RewriteRules): DocumentTree = TreeCursor(this).rewriteTarget(rules)

}

/** Represents the root of a tree of documents. In addition to the recursive structure of documents,
  * usually obtained by parsing text markup, it holds additional items like styles and static documents,
  * which may contribute to the rendering of a site or an e-book.
  * 
  * The `styles` property of this type is currently only populated and processed when rendering PDF or XSL-FO.
  * Styles for HTML or EPUB documents are part of the `staticDocuments` property instead and will be integrated
  * into the final output, but not interpreted.
  * 
  * @param tree the recursive structure of documents, usually obtained from parsing text markup 
  * @param coverDocument the cover document (usually used with e-book formats like EPUB and PDF)            
  * @param styles the styles to apply when rendering this tree, only populated for PDF or XSL-FO output
  * @param staticDocuments the paths of documents that were neither identified as text markup, config or templates, and will be copied as is to the final output
  * @param sourcePaths the paths this document tree has been built from or an empty list if this ast does not originate from the file system
  */
case class DocumentTreeRoot (tree: DocumentTree,
                             coverDocument: Option[Document] = None,
                             styles: Map[String, StyleDeclarationSet] = Map.empty.withDefaultValue(StyleDeclarationSet.empty), 
                             staticDocuments: Seq[Path] = Nil,
                             sourcePaths: Seq[String] = Nil) {

  /** The configuration associated with the root of the tree.
    * 
    * Like text markup documents and templates, configurations form a tree
    * structure and sub-trees may override and/or add properties that have
    * only an effect in that sub-tree.
    */
  val config: Config = tree.config

  /** The title of this tree, obtained from configuration.
    */
  val title: Seq[Span] = tree.title

  /** The title document for this tree, if present.
    *
    * At the root level the title document, if present, will be rendered
    * after the cover document.
    */
  val titleDocument: Option[Document] = tree.titleDocument

  /** All documents contained in this tree, fetched recursively, depth-first.
    */
  lazy val allDocuments: Seq[Document] = {

    def collect (tree: DocumentTree): Seq[Document] = tree.titleDocument.toSeq ++ tree.content.flatMap {
      case doc: Document     => Seq(doc)
      case sub: DocumentTree => collect(sub)
    }
    
    coverDocument.toSeq ++ collect(tree)
  }

  /** Returns a new tree, with all the document models contained in it
    *  rewritten based on the specified rewrite rules.
    *
    *  If the rule is not defined for a specific element or the rule returns
    *  a `Retain` action as a result the old element remains in the tree unchanged. 
    *
    *  If it returns `Remove` then the node gets removed from the ast,
    *  if it returns `Replace` with a new element it will replace the old one. 
    *
    *  The rewriting is performed bottom-up (depth-first), therefore
    *  any element container passed to the rule only contains children which have already
    *  been processed.
    *
    *  The specified factory function will be invoked for each document contained in this
    *  tree and must return the rewrite rules for that particular document.
    */
  def rewrite (rules: DocumentCursor => RewriteRules): DocumentTreeRoot = RootCursor(this).rewriteTarget(rules)

}
