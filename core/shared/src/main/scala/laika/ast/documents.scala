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

import cats.data.NonEmptySet
import laika.ast.RelativePath.CurrentTree
import laika.ast.RewriteRules.RewriteRulesBuilder
import laika.config.Config.IncludeMap
import laika.config._
import laika.rewrite.nav.{ AutonumberConfig, TargetFormats }
import laika.rewrite.{ DefaultTemplatePath, OutputContext, TemplateRewriter }

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

  /** The title of this element which can originate from
    * the first header of a markup file or from configuration.
    */
  def title: Option[SpanSequence]

  /** The configuration associated with this element.
    */
  def config: Config

  /** The position of this element within the document tree.
    */
  def position: TreePosition

  protected def titleFromConfig: Option[SpanSequence] = {
    config.get[Traced[String]](LaikaKeys.title).toOption.flatMap { tracedTitle =>
      if (tracedTitle.origin.scope == configScope) {
        val title             = Seq(Text(tracedTitle.value))
        val autonumberConfig  = config.get[AutonumberConfig].getOrElse(AutonumberConfig.defaults)
        val autonumberEnabled =
          autonumberConfig.documents && position.depth < autonumberConfig.maxDepth
        if (autonumberEnabled) Some(SpanSequence(position.toSpan +: title))
        else Some(SpanSequence(title))
      }
      else None
    }
  }

  protected def configScope: Origin.Scope

  /** Creates the navigation structure for this instance up to the specified depth.
    * The returned instance can be used as part of a bigger navigation structure comprising of trees, documents and their sections.
    *
    * @param context captures the navigation depth, reference path and styles for the navigation tree being built
    * @return a navigation item that can be used as part of a bigger navigation structure comprising of trees, documents and their sections
    */
  def asNavigationItem(
      context: NavigationBuilderContext = NavigationBuilderContext()
  ): NavigationItem

  /** Extracts all runtime messages with the specified minimum level from this tree content.
    */
  def runtimeMessages(filter: MessageFilter): Seq[RuntimeMessage]

  /** Extracts all invalid elements with the specified minimum message level from this tree content.
    */
  def invalidElements(filter: MessageFilter): Seq[Invalid]

  /** The formats this tree content should be rendered to.
    */
  def targetFormats: TargetFormats = config.get[TargetFormats].getOrElse(TargetFormats.All)
}

/** A template document containing the element tree of a parsed template and its extracted
  *  configuration section (if present).
  */
case class TemplateDocument(
    path: Path,
    content: TemplateRoot,
    config: ConfigParser = ConfigParser.empty
) extends Navigatable {

  /** Applies this template to the specified document, replacing all
    *  span and block resolvers in the template with the final resolved element.
    */
  def applyTo(
      document: Document,
      rules: RewriteRules,
      outputContext: OutputContext
  ): Either[ConfigError, Document] =
    DocumentCursor(document, Some(outputContext))
      .flatMap(TemplateRewriter.applyTemplate(_, _ => Right(rules), this))

}

/** A pure descriptor for a static document, without the actual bytes.
  * Used for evaluating links and other AST transformation phases.
  */
case class StaticDocument(path: Path, formats: TargetFormats = TargetFormats.All)

object StaticDocument {

  def apply(path: Path, format: String, formats: String*): StaticDocument =
    StaticDocument(path, TargetFormats.Selected(NonEmptySet.of(format, formats: _*)))

}

/** A temporary structure usually not exposed to user code.
  * It holds a document with an empty Config instance and its actual config
  * (obtained from a header section if present) in unresolved form, as it
  * needs to be resolved based on a fallback configuration later.
  */
case class UnresolvedDocument(document: Document, config: ConfigParser)

/** Captures information about a document section, without its content.
  */
case class SectionInfo(
    id: String,
    title: SpanSequence,
    content: Seq[SectionInfo],
    options: Options = NoOpt
) extends Element with ElementContainer[SectionInfo] {

  type Self = SectionInfo
  def withOptions(options: Options): SectionInfo = copy(options = options)

  /** Creates the navigation structure for this section up to the specified depth.
    * The returned instance can be used as part of a bigger navigation structure comprising of documents and trees.
    *
    * @param context captures the navigation depth, reference path and styles for the navigation tree being built
    * @return a navigation item that can be used as part of a bigger navigation structure comprising of trees, documents and their sections
    */
  def asNavigationItem(
      docPath: Path,
      context: NavigationBuilderContext = NavigationBuilderContext()
  ): NavigationItem = {
    val children =
      if (context.isComplete) Nil else content.map(_.asNavigationItem(docPath, context.nextLevel))
    context.newNavigationItem(title, docPath.withFragment(id), children, TargetFormats.All)
  }

}

/** The structure of a markup document.
  */
trait DocumentStructure extends DocumentNavigation { this: TreeContent =>

  /** The tree model obtained from parsing the markup document.
    */
  def content: RootElement

  private def findRoot: Seq[Block] = {
    content.collect {
      case RootElement(TemplateRoot(_, _) :: Nil, _) => Nil
      case RootElement(content, _)                   => Seq(content)
      case EmbeddedRoot(content, _, _)               => Seq(content)
    }.flatten.headOption.getOrElse(Nil)
  }

  /** The title of this document, obtained from the document
    * structure or from the configuration. In case no title
    * is defined in either of the two places the result will
    * be `None`.
    */
  def title: Option[SpanSequence] = {

    def titleFromTree = (RootElement(findRoot) collect { case Title(content, _) =>
      SpanSequence(content)
    }).headOption

    titleFromConfig.orElse(titleFromTree)
  }

  /** The section structure of this document based on the hierarchy
    *  of headers found in the original text markup.
    */
  lazy val sections: Seq[SectionInfo] = {

    def extractSections(blocks: Seq[Block]): Seq[SectionInfo] = {
      blocks collect { case Section(Header(_, header, Id(id)), content, _) =>
        SectionInfo(id, SpanSequence(header), extractSections(content))
      }
    }
    extractSections(findRoot)
  }

  def runtimeMessages(filter: MessageFilter): Seq[RuntimeMessage] = filter match {
    case MessageFilter.None => Nil
    case _                  =>
      content.collect {
        case msg: RuntimeMessage if filter(msg) => msg
      }
  }

  def invalidElements(filter: MessageFilter): Seq[Invalid] = filter match {
    case MessageFilter.None => Nil
    case _                  =>
      content.collect {
        case inv: Invalid if filter(inv.message) => inv
      }
  }

}

/** The structure of a document tree.
  */
trait TreeStructure { this: TreeContent =>

  /** The actual document tree that this ast structure represents.
    */
  def targetTree: DocumentTree

  /** The content of this tree structure, containing
    * all markup documents and subtrees, except for the (optional) title document.
    */
  def content: Seq[TreeContent]

  /** The title of this tree, obtained from configuration.
    */
  lazy val title: Option[SpanSequence] = titleDocument.flatMap(_.title).orElse(titleFromConfig)

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

  /** All documents contained in this tree, fetched recursively, depth-first.
    */
  lazy val allDocuments: Seq[Document] = {

    def collect(tree: DocumentTree): Seq[Document] =
      tree.titleDocument.toSeq ++ tree.content.flatMap {
        case doc: Document     => Seq(doc)
        case sub: DocumentTree => collect(sub)
      }

    collect(targetTree)
  }

  /** Indicates whether this tree does not contain any markup document.
    * Template documents do not count, as they would be ignored in rendering
    * when there is no markup document.
    */
  lazy val isEmpty: Boolean = {

    def nonEmpty(tree: DocumentTree): Boolean = tree.titleDocument.nonEmpty || tree.content.exists {
      case _: Document       => true
      case sub: DocumentTree => nonEmpty(sub)
    }

    !nonEmpty(targetTree)
  }

  /** Selects a document from this tree or one of its subtrees by the specified path.
    * The path needs to be relative and not point to a parent tree (neither start
    * with `/` nor with `..`).
    */
  def selectDocument(path: String): Option[Document] = selectDocument(RelativePath.parse(path))

  /** Selects a document from this tree or one of its subtrees by the specified path.
    * The path must not point to a parent tree (start with `../`)
    * as this instance is not aware of its parents.
    */
  def selectDocument(path: RelativePath): Option[Document] = path.withoutFragment match {
    case CurrentTree / localName                     =>
      (titleDocument.toSeq ++: content).collectFirst {
        case d: Document if d.path.name == localName => d
      }
    case other / localName if path.parentLevels == 0 =>
      selectSubtree(other).flatMap(_.selectDocument(localName))
    case _                                           =>
      None
  }

  /** Appends the specified content to this tree and return a new instance.
    */
  def appendContent(content: TreeContent, contents: TreeContent*): DocumentTree = appendContent(
    content +: contents
  )

  /** Appends the specified content to this tree and return a new instance.
    */
  def appendContent(content: Seq[TreeContent]): DocumentTree =
    targetTree.copy(content = targetTree.content ++ content)

  /** Prepends the specified content to this tree and return a new instance.
    */
  def prependContent(content: TreeContent, contents: TreeContent*): DocumentTree = prependContent(
    content +: contents
  )

  /** Prepends the specified content to this tree and return a new instance.
    */
  def prependContent(content: Seq[TreeContent]): DocumentTree =
    targetTree.copy(content = content ++ targetTree.content)

  /** Selects a template from this tree or one of its subtrees by the specified path.
    * The path needs to be relative.
    */
  def selectTemplate(path: String): Option[TemplateDocument] = selectTemplate(
    RelativePath.parse(path)
  )

  /** Selects a template from this tree or one of its subtrees by the specified path.
    * The path must not point to a parent tree (start with `../`)
    * as this instance is not aware of its parents.
    */
  def selectTemplate(path: RelativePath): Option[TemplateDocument] = path match {
    case CurrentTree / localName                     => templates.find(_.path.name == localName)
    case other / localName if path.parentLevels == 0 =>
      selectSubtree(other).flatMap(_.selectTemplate(localName))
    case _                                           => None
  }

  /** Selects the template with the name `default.template.&lt;suffix&gt;` for the
    * specified format suffix from this level of the document tree.
    */
  def getDefaultTemplate(formatSuffix: String): Option[TemplateDocument] = {
    selectTemplate(DefaultTemplatePath.forSuffix(formatSuffix).relative)
  }

  /** Create a new document tree that contains the specified template as the default.
    */
  def withDefaultTemplate(template: TemplateRoot, formatSuffix: String): DocumentTree = {
    val defPath = path / DefaultTemplatePath.forSuffix(formatSuffix).relative
    targetTree.copy(templates =
      targetTree.templates.filterNot(_.path == defPath) :+
        TemplateDocument(defPath, template)
    )
  }

  /** Selects a subtree of this tree by the specified path.
    *  The path needs to be relative and it may point to a deeply nested
    *  subtree, not just immediate children.
    */
  def selectSubtree(path: String): Option[DocumentTree] = selectSubtree(RelativePath.parse(path))

  /** Selects a subtree of this tree by the specified path.
    * The path must not point to a parent tree (start with `../`)
    * as this instance is not aware of its parents.
    */
  def selectSubtree(path: RelativePath): Option[DocumentTree] = path match {
    case CurrentTree                                 => Some(targetTree)
    case CurrentTree / localName                     =>
      content.collectFirst { case t: DocumentTree if t.path.name == localName => t }
    case other / localName if path.parentLevels == 0 =>
      selectSubtree(other).flatMap(_.selectSubtree(localName))
    case _                                           => None
  }

  /** Creates the navigation structure for this tree up to the specified depth.
    * The returned instance can be used as part of a bigger navigation structure comprising of trees, documents and their sections.
    *
    * @param context captures the navigation depth, reference path and styles for the navigation tree being built
    * @return a navigation item that can be used as part of a bigger navigation structure comprising of trees, documents and their sections
    */
  def asNavigationItem(
      context: NavigationBuilderContext = NavigationBuilderContext()
  ): NavigationItem = {
    def hasLinks(item: NavigationItem): Boolean =
      item.link.nonEmpty || item.content.exists(hasLinks)
    val navContent                              = content
      .filterNot(_.path == context.refPath && context.excludeSelf)
      .filterNot(_.config.get[Boolean](LaikaKeys.excludeFromNavigation).getOrElse(false))
    val children                                =
      if (context.isComplete) Nil
      else navContent.map(_.asNavigationItem(context.nextLevel)).filter(hasLinks)
    val navTitle                                = title.getOrElse(SpanSequence(path.name))
    context.newNavigationItem(navTitle, titleDocument, children, targetFormats)
  }

  def runtimeMessages(filter: MessageFilter): Seq[RuntimeMessage] = filter match {
    case MessageFilter.None => Nil
    case _                  =>
      titleDocument.toSeq.flatMap(_.runtimeMessages(filter)) ++ content.flatMap(
        _.runtimeMessages(filter)
      )
  }

  def invalidElements(filter: MessageFilter): Seq[Invalid] = filter match {
    case MessageFilter.None => Nil
    case _                  =>
      titleDocument.toSeq.flatMap(_.invalidElements(filter)) ++ content.flatMap(
        _.invalidElements(filter)
      )
  }

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
case class Document(
    path: Path,
    content: RootElement,
    fragments: Map[String, Element] = Map.empty,
    config: Config = Config.empty,
    position: TreePosition = TreePosition.orphan
) extends DocumentStructure with TreeContent {

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
  def rewrite(rules: RewriteRules): Either[ConfigError, Document] =
    DocumentCursor(this).map(_.rewriteTarget(rules))

  protected val configScope: Origin.Scope = Origin.DocumentScope

  /** Appends the specified content to this tree and return a new instance.
    */
  def appendContent(content: Block, contents: Block*): Document = appendContent(content +: contents)

  /** Appends the specified content to this tree and return a new instance.
    */
  def appendContent(newContent: Seq[Block]): Document =
    copy(content = content.withContent(this.content.content ++ newContent))

  /** Prepends the specified content to this tree and return a new instance.
    */
  def prependContent(content: Block, contents: Block*): Document = prependContent(
    content +: contents
  )

  /** Prepends the specified content to this tree and return a new instance.
    */
  def prependContent(newContent: Seq[Block]): Document =
    copy(content = content.withContent(newContent ++ this.content.content))

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
case class DocumentTree(
    path: Path,
    content: Seq[TreeContent],
    titleDocument: Option[Document] = None,
    templates: Seq[TemplateDocument] = Nil,
    config: Config = Config.empty,
    position: TreePosition = TreePosition.root
) extends TreeStructure with TreeContent {

  val targetTree: DocumentTree = this

  /** Creates a new tree by applying the specified function to all documents in this tree recursively.
    */
  def mapDocuments(f: Document => Document): DocumentTree = {
    val newTitle   = titleDocument.map(f)
    val newContent = content.map {
      case d: Document     => f(d)
      case t: DocumentTree => t.mapDocuments(f)
    }
    copy(titleDocument = newTitle, content = newContent)
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
  def rewrite(rules: RewriteRulesBuilder): Either[TreeConfigErrors, DocumentTree] =
    TreeCursor(this).flatMap(_.rewriteTarget(rules))

  protected val configScope: Origin.Scope = Origin.TreeScope

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
  * @param staticDocuments the descriptors for documents that were neither identified as text markup, config or templates, and will be copied as is to the final output
  * @param includes the map of configuration includes that may be needed when resolving template configuration
  */
case class DocumentTreeRoot(
    tree: DocumentTree,
    coverDocument: Option[Document] = None,
    styles: Map[String, StyleDeclarationSet] =
      Map.empty.withDefaultValue(StyleDeclarationSet.empty),
    staticDocuments: Seq[StaticDocument] = Nil,
    includes: IncludeMap = Map.empty
) {

  /** The configuration associated with the root of the tree.
    *
    * Like text markup documents and templates, configurations form a tree
    * structure and sub-trees may override and/or add properties that have
    * only an effect in that sub-tree.
    */
  val config: Config = tree.config

  /** The title of this tree, obtained from configuration.
    */
  val title: Option[SpanSequence] = tree.title

  /** The title document for this tree, if present.
    *
    * At the root level the title document, if present, will be rendered
    * after the cover document.
    */
  val titleDocument: Option[Document] = tree.titleDocument

  /** All documents contained in this tree, fetched recursively, depth-first.
    */
  lazy val allDocuments: Seq[Document] = coverDocument.toSeq ++ tree.allDocuments

  /** Indicates whether this tree does not contain any markup document.
    * Template documents do not count, as they would be ignored in rendering
    * when there is no markup document.
    */
  lazy val isEmpty: Boolean = coverDocument.isEmpty && tree.isEmpty

  /** Creates a new tree by applying the specified function to all documents in this tree recursively.
    */
  def mapDocuments(f: Document => Document): DocumentTreeRoot = {
    val newCover = coverDocument.map(f)
    val newTree  = tree.mapDocuments(f)
    copy(coverDocument = newCover, tree = newTree)
  }

  /** Creates a new tree by replacing its root configuration with the specified config instance.
    */
  def withConfig(config: Config): DocumentTreeRoot = {
    copy(tree = tree.copy(config = config))
  }

  /** Creates a new instance by applying the specified function to the root tree.
    */
  def modifyTree(f: DocumentTree => DocumentTree): DocumentTreeRoot = copy(tree = f(tree))

  /** Returns a new tree, with all the document models contained in it rewritten based on the specified rewrite rules.
    *
    * If the rule is not defined for a specific element or the rule returns a `Retain` action as a result
    * the old element remains in the tree unchanged.
    *
    * If it returns `Remove` then the node gets removed from the ast,
    * if it returns `Replace` with a new element it will replace the old one.
    *
    * The rewriting is performed bottom-up (depth-first),
    * therefore any element container passed to the rule only contains children which have already been processed.
    *
    * The specified factory function will be invoked for each document contained in this tree
    * and must return the rewrite rules for that particular document.
    */
  def rewrite(rules: RewriteRulesBuilder): Either[TreeConfigErrors, DocumentTreeRoot] =
    RootCursor(this).flatMap(_.rewriteTarget(rules))

  /** Selects and applies the templates contained in this document tree for the specified output format to all documents
    * within this tree recursively.
    */
  def applyTemplates(
      rules: RewriteRulesBuilder,
      context: OutputContext
  ): Either[ConfigError, DocumentTreeRoot] =
    TemplateRewriter.applyTemplates(this, rules, context)

}
