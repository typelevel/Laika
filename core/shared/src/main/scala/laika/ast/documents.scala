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
import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentTree
import laika.ast.RewriteRules.RewriteRulesBuilder
import laika.config.Config.IncludeMap
import laika.config.*
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

/** Represents a single document and provides access to the document content and structure
  * as well as hooks for triggering rewrite operations.
  *
  * @param content the tree model obtained from parsing the markup document
  * @param fragments separate named fragments that had been extracted from the content
  */
class Document(
    context: TreeNodeContext,
    val content: RootElement,
    val fragments: Map[String, Element] = Map.empty
) extends DocumentNavigation with TreeContent {

  /** The full, absolute path of this document in the (virtual) document tree.
    */
  def path: Path = context.path

  /** The configuration associated with this document.
    */
  def config: Config = context.config

  /** Associates the specified config instance with this document.
    *
    * If you want to add values to the existing configuration of this instance,
    * use `modifyConfig` instead, which is more efficient than re-assigning
    * a full new instance which is based on the existing one.
    */
  def withConfig(config: Config): Document = {
    val newContext = context.copy(localConfig = config)
    new Document(newContext, content, fragments)
  }

  /** Modifies the existing config instance for this document by appending
    * one or more additional values.
    *
    * This method is much more efficient than `withConfig` when existing config values should be retained.
    */
  def modifyConfig(f: ConfigBuilder => ConfigBuilder): Document = {
    val builder = ConfigBuilder.withFallback(context.localConfig)
    withConfig(f(builder).build)
  }

  /** Replaces the entire content of this document with the specified new `RootElement`.
    */
  def withContent(content: RootElement): Document =
    new Document(context, content, fragments)

  /** Replaces the fragments of this document with the specified new map.
    */
  def withFragments(newFragments: Map[String, Element]): Document =
    new Document(context, content, newFragments)

  /** Adds the specified fragments to the existing map of fragments.
    */
  def addFragments(newFragments: Map[String, Element]): Document = withFragments(
    fragments ++ newFragments
  )

  /** The position of this document inside a document tree hierarchy, expressed as a list of Ints.
    */
  def position: TreePosition = context.position

  private[ast] def withPosition(index: TreeNodeIndex): Document =
    new Document(context.copy(index = index), content, fragments)

  private[laika] def withPosition(index: Int): Document =
    withPosition(TreeNodeIndex.Value(index))

  private[laika] def withContent(content: RootElement, fragments: Map[String, Element]): Document =
    new Document(context, content, fragments)

  private[laika] def withTemplateConfig(config: Config): Document =
    withConfig(config.withFallback(context.localConfig))

  private[ast] def withParent(parent: TreeNodeContext): Document =
    new Document(context.copy(parent = Some(parent)), content, fragments)

  private[laika] def withPath(path: Path): Document = {

    def contextFor(path: Path, oldContext: Option[TreeNodeContext]): TreeNodeContext = {
      val oldParent = oldContext.flatMap(_.parent)
      val parent    = path.parent match {
        case Root  => oldParent.map(_.copy(localPath = None))
        case other => Some(contextFor(other, oldParent))
      }
      oldContext match {
        case Some(existing) => existing.copy(localPath = Some(path.name), parent = parent)
        case None           => TreeNodeContext(localPath = Some(path.name), parent = parent)
      }
    }

    new Document(
      contextFor(path, Some(context)),
      content,
      fragments
    )
  }

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
    * of headers found in the original text markup.
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
    new Document(context, content.withContent(content.content ++ newContent), fragments)

  /** Prepends the specified content to this tree and return a new instance.
    */
  def prependContent(content: Block, contents: Block*): Document = prependContent(
    content +: contents
  )

  /** Prepends the specified content to this tree and return a new instance.
    */
  def prependContent(newContent: Seq[Block]): Document =
    new Document(context, content.withContent(newContent ++ content.content), fragments)

}

object Document {

  def apply(path: Path, content: RootElement): Document = apply(path, content, Map.empty)

  // TODO - M3 - this might be unnecessary
  def apply(path: Path, content: RootElement, fragments: Map[String, Element]): Document = {

    def contextFor(path: Path): TreeNodeContext = {
      val parent = path.parent match {
        case Root  => None
        case other => Some(contextFor(other))
      }
      TreeNodeContext(localPath = Some(path.name), parent = parent)
    }

    new Document(contextFor(path), content, fragments)
  }

}

private[ast] sealed trait TreeNodeIndex

private[ast] object TreeNodeIndex {
  case object Unassigned       extends TreeNodeIndex
  case object Inherit          extends TreeNodeIndex
  case class Value(index: Int) extends TreeNodeIndex
}

private[ast] case class TreeNodeContext(
    localPath: Option[String] = None,
    localConfig: Config = Config.empty,
    index: TreeNodeIndex = TreeNodeIndex.Unassigned,
    parent: Option[TreeNodeContext] = None
) {

  lazy val path: Path = parent.map(_.path) match {
    case Some(parentPath) => parentPath / localPath.getOrElse("")
    case None             => localPath.fold[Path](Root)(Root / _)
  }

  lazy val config: Config = localConfig.withFallback(parent.fold(Config.empty)(_.config))

  lazy val position: TreePosition = index match {
    case TreeNodeIndex.Value(idx) => parent.fold(TreePosition.root)(_.position).forChild(idx)
    case TreeNodeIndex.Inherit    => parent.fold(TreePosition.root)(_.position)
    case TreeNodeIndex.Unassigned => TreePosition.root
  }

  def child(localName: String, config: Config = Config.empty): TreeNodeContext =
    TreeNodeContext(Some(localName), localConfig = config, parent = Some(this))

}

/** Represents a virtual tree with all its documents, templates, configurations and subtrees.
  *
  * @param content the markup documents and subtrees except for the (optional) title document
  * @param titleDocument the optional title document of this tree
  * @param templates all templates on this level of the tree hierarchy that might get applied to a document when it gets rendered
  */
class DocumentTree private[ast] (
    context: TreeNodeContext,
    val content: Seq[TreeContent],
    val titleDocument: Option[Document] = None,
    val templates: Seq[TemplateDocument] = Nil
) extends TreeContent {

  /** The full, absolute path of this (virtual) document tree.
    */
  def path: Path = context.path

  /** The configuration associated with this tree.
    */
  def config: Config = context.config

  /** The position of this tree inside a document ast hierarchy, expressed as a list of Ints.
    */
  def position: TreePosition = context.position

  private def withContext(newContext: TreeNodeContext): DocumentTree = {
    // separate from copy since context changes need to be propagated to all children
    new DocumentTree(
      newContext,
      content.map {
        case d: Document     => d.withParent(newContext)
        case t: DocumentTree => t.withParent(newContext)
      },
      titleDocument.map(_.withParent(newContext)),
      templates
    )
  }

  private def copy(
      content: Seq[TreeContent] = this.content,
      titleDocument: Option[Document] = this.titleDocument,
      templates: Seq[TemplateDocument] = this.templates
  ): DocumentTree = new DocumentTree(context, content, titleDocument, templates)

  private[laika] def withPosition(index: Int): DocumentTree =
    withContext(context.copy(index = TreeNodeIndex.Value(index)))

  private[ast] def withParent(parent: TreeNodeContext): DocumentTree =
    withContext(context.copy(parent = Some(parent)))

  private[laika] def withoutTemplates: DocumentTree = copy(templates = Nil)

  /** Adds the specified document as the title document for this tree,
    * replacing the existing title document if present.
    */
  def withTitleDocument(doc: Document): DocumentTree = copy(titleDocument = Some(doc))

  /** Adds the specified document as the title document for this tree
    * replacing the existing title document if present or, if the parameter is empty,
    * removes any existing title document.
    */
  def withTitleDocument(doc: Option[Document]): DocumentTree = copy(titleDocument = doc)

  /** Associates the specified config instance with this document tree.
    *
    * If you want to add values to the existing configuration of this instance,
    * use `modifyConfig` instead, which is more efficient than re-assigning
    * a full new instance which is based on the existing one.
    */
  def withConfig(config: Config): DocumentTree = withContext(context.copy(localConfig = config))

  /** Modifies the existing config instance for this document tree by appending
    * one or more additional values.
    *
    * This method is much more efficient than `withConfig` when existing config values should be retained.
    */
  def modifyConfig(f: ConfigBuilder => ConfigBuilder): DocumentTree = {
    val builder = ConfigBuilder.withFallback(context.localConfig)
    withConfig(f(builder).build)
  }

  /** Adds the specified template document to this tree,
    * retaining any previously added templates.
    */
  def addTemplate(template: TemplateDocument): DocumentTree =
    copy(templates = templates :+ template)

  /** The title of this tree, obtained from configuration.
    */
  lazy val title: Option[SpanSequence] = titleDocument.flatMap(_.title).orElse(titleFromConfig)

  /** All documents contained in this tree, fetched recursively, depth-first.
    */
  lazy val allDocuments: Seq[Document] = {

    def collect(tree: DocumentTree): Seq[Document] =
      tree.titleDocument.toSeq ++ tree.content.flatMap {
        case doc: Document     => Seq(doc)
        case sub: DocumentTree => collect(sub)
      }

    collect(this)
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

    !nonEmpty(this)
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

  /** Removes all documents from this tree where the specified filter applies to its path.
    * Does not recurse into nested sub-trees and does not apply to templates or title documents.
    */
  def removeContent(filter: Path => Boolean): DocumentTree =
    copy(content = content.filterNot(c => filter(c.path)))

  /** Appends the specified content to this tree and return a new instance.
    */
  def appendContent(content: TreeContent, contents: TreeContent*): DocumentTree = appendContent(
    content +: contents
  )

  /** Appends the specified content to this tree and return a new instance.
    */
  def appendContent(newContent: Seq[TreeContent]): DocumentTree =
    copy(content = content ++ newContent)

  /** Prepends the specified content to this tree and return a new instance.
    */
  def prependContent(content: TreeContent, contents: TreeContent*): DocumentTree = prependContent(
    content +: contents
  )

  /** Prepends the specified content to this tree and return a new instance.
    */
  def prependContent(newContent: Seq[TreeContent]): DocumentTree =
    copy(content = newContent ++ content)

  /** Applies the specified function to all elements of the `content` property
    * and returns a new document tree.
    *
    * Applies the function to documents and document trees on this level of the hierarchy.
    * IF you want to modify documents recursively, use `modifyDocumentsRecursively` instead.
    */
  def modifyContent(f: TreeContent => TreeContent): DocumentTree =
    copy(content = content.map(f))

  /** Creates a new tree by applying the specified function to all documents in this tree recursively.
    */
  def modifyDocumentsRecursively(f: Document => Document): DocumentTree = {
    val newTitle   = titleDocument.map(f)
    val newContent = content.map {
      case d: Document     => f(d)
      case t: DocumentTree => t.modifyDocumentsRecursively(f)
    }
    new DocumentTree(context, newContent, newTitle, templates)
  }

  /** Replaces the contents of this document tree.
    * Consider using `modifyContent` instead when only intending to adjust existing content.
    */
  def replaceContent(newContent: Seq[TreeContent]): DocumentTree = copy(content = newContent)

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
    copy(templates =
      templates.filterNot(_.path == defPath) :+
        TemplateDocument(defPath, template)
    )
  }

  /** Selects a subtree of this tree by the specified path.
    * The path needs to be relative and it may point to a deeply nested
    * subtree, not just immediate children.
    */
  def selectSubtree(path: String): Option[DocumentTree] = selectSubtree(RelativePath.parse(path))

  /** Selects a subtree of this tree by the specified path.
    * The path must not point to a parent tree (start with `../`)
    * as this instance is not aware of its parents.
    */
  def selectSubtree(path: RelativePath): Option[DocumentTree] = path match {
    case CurrentTree                                 => Some(this)
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

    val navContent = content
      .filterNot(_.path == context.refPath && context.excludeSelf)
      .filterNot(_.config.get[Boolean](LaikaKeys.excludeFromNavigation).getOrElse(false))
    val children   =
      if (context.isComplete) Nil
      else navContent.map(_.asNavigationItem(context.nextLevel)).filter(hasLinks)
    val navTitle   = title.getOrElse(SpanSequence(path.name))
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

object DocumentTree {

  /** A new, empty builder for constructing a new `DocumentTree`.
    */
  val builder = new DocumentTreeBuilder()

  /** An empty `DocumentTree` without any documents, templates or configurations. */
  val empty: DocumentTree = new DocumentTree(TreeNodeContext(), Nil)
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
  def modifyDocumentsRecursively(f: Document => Document): DocumentTreeRoot = {
    val newCover = coverDocument.map(f)
    val newTree  = tree.modifyDocumentsRecursively(f)
    copy(coverDocument = newCover, tree = newTree)
  }

  /** Associates the specified config instance with this document tree.
    *
    * If you want to add values to the existing configuration of this instance,
    * use `modifyConfig` instead, which is more efficient than re-assigning
    * a full new instance which is based on the existing one.
    */
  def withConfig(config: Config): DocumentTreeRoot = copy(tree = tree.withConfig(config))

  /** Modifies the existing config instance for this document tree by appending
    * one or more additional values.
    *
    * This method is much more efficient than `withConfig` when existing config values should be retained.
    */
  def modifyConfig(f: ConfigBuilder => ConfigBuilder): DocumentTreeRoot =
    copy(tree = tree.modifyConfig(f))

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
