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

import com.typesafe.config.{Config, ConfigFactory}
import laika.collection.TransitionalCollectionOps._
import laika.io.Input
import laika.rewrite.TemplateRewriter
import laika.rewrite.link.LinkTargetProvider
import laika.rewrite.link.LinkTargets._
import laika.rewrite.nav.AutonumberConfig

import scala.annotation.tailrec


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
    if (config.hasPath("title")) {
      val title = List(Text(config.getString("title")))
      val autonumberConfig = AutonumberConfig.fromConfig(config)
      val autonumberEnabled = autonumberConfig.documents && position.depth < autonumberConfig.maxDepth
      if (autonumberEnabled) Some(position.toSpan +: title)
      else Some(title)
    }
    else None
  }

}


/** Content within the document tree that is
  * neither titled nor positional. These are usually
  * helper documents that do not show up in the main
  * navigation for the document tree.
  */
sealed trait AdditionalContent extends Navigatable

/** A static document that might get copied to the
  * target document tree as is.
  */
case class StaticDocument (input: Input) extends AdditionalContent {
  val path: Path = input.path
}

/** A dynamic document that has been obtained from a template
  * not associated with any markup document.
  */
case class DynamicDocument (path: Path, content: RootElement) extends AdditionalContent

/** A template document containing the element tree of a parsed template and its extracted
 *  configuration section (if present).
 */
case class TemplateDocument (path: Path, content: TemplateRoot, config: Config = ConfigFactory.empty) extends AdditionalContent {

  /** Applies this template to the specified document, replacing all
   *  span and block resolvers in the template with the final resolved element.
   */
  def applyTo (document: Document): Document = TemplateRewriter.applyTemplate(DocumentCursor(document), this)

}

/** Captures information about a document section, without its content.
 */
case class SectionInfo (id: String, title: TitleInfo, content: Seq[SectionInfo]) extends Element with ElementContainer[SectionInfo, SectionInfo]

/** Represents a section title.
 */
case class TitleInfo (content: Seq[Span]) extends SpanContainer[TitleInfo]

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
  def depth = toSeq.size

  /** Creates a position instance for a child of this element.
    */
  def forChild(childPos: Int) = TreePosition(toSeq :+ childPos)

  def compare (other: TreePosition): Int = {

    @tailrec
    def compare (pos1: Seq[Int], pos2: Seq[Int]): Int = (pos1.headOption, pos2.headOption) match {
      case (Some(a), Some(b)) => a.compare(b) match {
        case 0 => compare(pos1.tail, pos2.tail)
        case other => other
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
      case RootElement(TemplateRoot(_,_) :: Nil) => false
      case RootElement(_) => true
      case _ => false
    }).headOption map { case RootElement(content) => content } getOrElse Nil
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
  lazy val globalLinkTargets = linkTargets.global

}

/** The structure of a document tree.
  */
trait TreeStructure { this: TreeContent =>

  import Path.Current

  /** The content of this tree structure, containing
    * all markup documents and subtrees.
    */
  def content: Seq[TreeContent]

  /** All templates on this level of the tree hierarchy that might
    * get applied to a document when it gets rendered.
    */
  def templates: Seq[TemplateDocument]

  /** The actual document tree that this ast structure represents.
    */
  def targetTree: DocumentTree

  /** The title of this tree, obtained from configuration.
   */
  lazy val title: Seq[Span] = titleFromConfig.getOrElse(Nil)

  /** The title document for this tree, if present.
    *
    * A document with the base name `title` and the corresponding
    * suffix for the input markup, e.g. `title.md` for Markdown,
    * can be used as an introductory section for a chapter represented
    * by a directory tree.
    */
  def titleDocument: Option[Document] = content.collectFirst {
    case doc: Document if doc.path.basename == "title" => doc
  }

  private def toMap [T <: Navigatable] (navigatables: Seq[T]): Map[String,T] = {
    navigatables groupBy (_.name) mapValuesStrict {
      case Seq(nav) => nav
      case multiple => throw new IllegalStateException("Multiple navigatables with the name " +
          s"${multiple.head.name} in tree $path")
    }
  }

  private val documentsByName = toMap(content collect {case d: Document => d})
  private val templatesByName = toMap(templates)
  private val subtreesByName = toMap(content collect {case t: DocumentTree => t})

  /** Selects a document from this tree or one of its subtrees by the specified path.
   *  The path needs to be relative.
   */
  def selectDocument (path: String): Option[Document] = selectDocument(Path(path))

  /** Selects a document from this tree or one of its subtrees by the specified path.
   *  The path needs to be relative.
   */
  def selectDocument (path: Path): Option[Document] = path match {
    case Current / name => documentsByName.get(name)
    case path / name => selectSubtree(path) flatMap (_.selectDocument(name))
    case _ => None
  }

  /** Selects a template from this tree or one of its subtrees by the specified path.
   *  The path needs to be relative.
   */
  def selectTemplate (path: String): Option[TemplateDocument] = selectTemplate(Path(path))

  /** Selects a template from this tree or one of its subtrees by the specified path.
   *  The path needs to be relative.
   */
  def selectTemplate (path: Path): Option[TemplateDocument] = path match {
    case Current / name => templatesByName.get(name)
    case path / name => selectSubtree(path) flatMap (_.selectTemplate(name))
    case _ => None
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
    case Current / name => subtreesByName.get(name)
    case path / name => selectSubtree(path) flatMap (_.selectSubtree(name))
    case _ => None
  }

  /** All link targets that can get referenced from anywhere
    * in the document tree.
    */
  lazy val globalLinkTargets: Map[Selector, TargetResolver] = {
    val all = content.foldLeft(List[(Selector,TargetResolver)]()) {
      case (list, content) => content.globalLinkTargets.toList ::: list
    }
    all.groupBy(_._1) collect {
      case (selector, ((_, target) :: Nil)) => (selector, target)
      case (s@UniqueSelector(name), conflicting) => (s, DuplicateTargetResolver(path, name))
    }
  }

}

/** Base type for all document type descriptors.
  */
sealed abstract class DocumentType

/** Provides all available DocumentTypes.
  */
object DocumentType {

  /** A configuration document in the syntax
    *  supported by the Typesafe Config library.
    */
  case object Config extends DocumentType

  /** A text markup document produced by a parser.
    */
  case object Markup extends DocumentType

  /** A template document that might get applied
    *  to a document when it gets rendered.
    */
  case object Template extends DocumentType

  /** A dynamic document that might contain
    *  custom directives that need to get
    *  processed before rendering.
    */
  case object Dynamic extends DocumentType

  /** A style sheet that needs to get passed
    *  to a renderer.
    */
  case class StyleSheet (format: String) extends DocumentType

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
                     config: Config = ConfigFactory.empty,
                     position: TreePosition = TreePosition(Seq())) extends DocumentStructure with TreeContent {

  /** Returns a new, rewritten document model based on the specified rewrite rule.
   *
   *  If the specified partial function is not defined for a specific element the old element remains
   *  in the tree unchanged. If it returns `None` then the node gets removed from the ast,
   *  if it returns an element it will replace the old one. Of course the function may
   *  also return the old element.
   *
   *  The rewriting is performed in a way that only branches of the tree that contain
   *  new or removed elements will be replaced. It is processed bottom-up, therefore
   *  any element container passed to the rule only contains children which have already
   *  been processed.
   */
  def rewrite (rule: RewriteRule): Document = DocumentCursor(this).rewriteTarget(rule)

}

/** Represents a tree with all its documents and subtrees.
 *
 *  @param path the full, absolute path of this (virtual) document tree
 *  @param content the markup documents and subtrees
 *  @param templates all templates on this level of the tree hierarchy that might get applied to a document when it gets rendered
 *  @param styles the styles to apply when rendering this tree
 *  @param additionalContent all dynamic or static documents that are not part of the main navigatable content of the tree
 *  @param config the configuration associated with this tree
 *  @param position the position of this tree inside a document ast hierarchy, expressed as a list of Ints
 *  @param sourcePaths the paths this document tree has been built from or an empty list if this ast does not originate from the file system
 */
case class DocumentTree (path:Path,
                         content: Seq[TreeContent],
                         templates: Seq[TemplateDocument] = Nil,
                         styles: Map[String,StyleDeclarationSet] = Map.empty.withDefaultValue(StyleDeclarationSet.empty),
                         additionalContent: Seq[AdditionalContent] = Nil,
                         config: Config = ConfigFactory.empty,
                         position: TreePosition = TreePosition.root,
                         sourcePaths: Seq[String] = Nil) extends TreeStructure with TreeContent {

  val targetTree = this

  /** Returns a new tree, with all the document models contained in it
   *  rewritten based on the specified rewrite rule.
   *
   *  If the specified partial function is not defined for a specific element the old element remains
   *  in the tree unchanged. If it returns `None` then the node gets removed from the ast,
   *  if it returns an element it will replace the old one. Of course the function may
   *  also return the old element.
   *
   *  The rewriting is performed in a way that only branches of the tree that contain
   *  new or removed elements will be replaced. It is processed bottom-up, therefore
   *  any element container passed to the rule only contains children which have already
   *  been processed.
   *
   *  The specified factory function will be invoked for each document contained in this
   *  tree and must return a partial function that represents the rewrite rules for that
   *  particular document.
   */
  def rewrite (rule: DocumentCursor => RewriteRule): DocumentTree = TreeCursor(this).rewriteTarget(rule)

}
