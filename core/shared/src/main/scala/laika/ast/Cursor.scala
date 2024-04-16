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

import cats.data.NonEmptyChain
import cats.syntax.all.*
import laika.api.bundle.{
  ConfigurablePathTranslator,
  PathTranslator,
  TargetLookup,
  TranslatorConfig
}
import laika.api.config.{ Config, ConfigEncoder, ConfigError, ConfigValue, Key, Origin }
import laika.ast.Path.Root
import laika.ast.RewriteRules.RewriteRulesBuilder
import laika.internal.collection.TransitionalCollectionOps.*
import laika.api.config.Config.ConfigResult
import laika.api.config.ConfigError.{ DocumentErrors, TreeErrors }
import laika.config.{ AutonumberConfig, LaikaKeys, LinkConfig, LinkValidation, TargetFormats }
import laika.internal.link
import laika.internal.link.LinkValidator
import laika.internal.nav.NavigationOrder
import laika.internal.rewrite.ReferenceResolver
import laika.parse.SourceFragment

/** A cursor provides the necessary context during a rewrite operation.
  * The stateless document tree cannot provide access to parent or sibling
  * nodes in the tree, therefore a temporary cursor instance is created
  * during a rewrite operation for this purpose.
  */
sealed trait Cursor {

  /** The type of the target this cursor points to.
    */
  type Target <: TreeContent

  /** The target within the document tree this cursor points to
    * (a markup document or a sub-tree).
    */
  def target: Target

  /** The full, absolute path of the target of this cursor in the (virtual) document tree
    */
  def path: Path

  /** The root cursor for this document tree.
    */
  def root: RootCursor

  /** The configuration associated with this node.
    */
  def config: Config

  /** The position of this node within the document tree.
    */
  def position: TreePosition

}

/** Cursor for the root node of a document tree, providing access to all
  * child cursors of this tree and allowing to trigger rewrite
  * operations.
  *
  * @param target the root of the document tree this cursor points to
  */
class RootCursor private (
    val target: DocumentTreeRoot,
    renderContext: Option[(OutputContext, TranslatorConfig)]
) {

  type Target = DocumentTreeRoot

  private[ast] lazy val targetLookup = new link.TargetLookup(this)

  /** The context for the output format when the cursor has been created for the final rewrite
    *  phase for a specific output format or empty in earlier rewrite phases that apply to all formats.
    */
  val outputContext: Option[OutputContext] = renderContext.map(_._1)

  /** The path translator to be used for translating internal links and output paths.
    *
    * The translator is specific for each output format and therefore this value is empty
    * for any cursor that has not been created for the final rewrite phase in the context
    * of a concrete renderer and its template.
    */
  lazy val pathTranslator: Option[PathTranslator] = renderContext.map {
    case (outputContext, translatorConfig) =>
      val lookup = new TargetLookup(this)
      ConfigurablePathTranslator(
        translatorConfig,
        outputContext,
        Root / "refPath",
        lookup
      )
  }

  val config: Config = target.config

  /** The cursor for the underlying tree structure of this root node.
    */
  val tree: TreeCursor = TreeCursor(this)

  /** The cursor for the cover document of this tree.
    */
  lazy val coverDocument: Option[DocumentCursor] = target.coverDocument.map { cover =>
    DocumentCursor(cover, tree)
  }

  /** All documents contained in this tree, fetched recursively, depth-first.
    */
  lazy val allDocuments: Seq[DocumentCursor] = coverDocument.toSeq ++ tree.allDocuments

  /** Returns a new tree root, with all the document models contained in it
    * rewritten based on the specified rewrite rule.
    */
  def rewriteTarget(rules: RewriteRulesBuilder): Either[TreeErrors, DocumentTreeRoot] = {
    val result = for {
      rewrittenCover <- coverDocument.traverse(_.rewriteTarget(rules).toEitherNec)
      rewrittenTree  <- tree.rewriteTarget(rules).leftMap(_.failures)
    } yield target.withCoverDocument(rewrittenCover).modifyTree(_ => rewrittenTree)
    result.leftMap(TreeErrors.apply)
  }

  /** Selects the tree configuration for the specified path,
    * or its nearest parent if that path does not have a configuration assigned.
    * Always succeeds as recursing upwards will eventually lead to this root cursor and return its configuration.
    */
  def selectTreeConfig(path: Path): Config = target.selectTreeConfig(path)

}

object RootCursor {

  /** Creates a new cursor for the specified document tree.
    */
  def apply(
      target: DocumentTreeRoot,
      outputContext: Option[OutputContext] = None
  ): Either[TreeErrors, RootCursor] = {

    /* Configuration values used by rewrite rules are validated in the respective builders for those rules.
       Here we only validate configuration used by the cursor implementation itself.
       We fail early here, as the actual access of these values happens in a lazily created, recursive structure,
       where it would not be beneficial to expose error handling in every step. */

    def validate(doc: Document): Option[DocumentErrors] = List(
      doc.config.getOpt[Boolean](LaikaKeys.versioned).toEitherNec,
      doc.config.getOpt[String](LaikaKeys.title).toEitherNec,
      doc.config.getOpt[TargetFormats].toEitherNec
    ).parSequence.left.toOption.map(DocumentErrors.apply(doc.path, _))

    def validateRoot: Seq[DocumentErrors] = List(
      target.config.getOpt[String](LaikaKeys.siteBaseURL).toEitherNec,
      target.config.getOpt[LinkConfig].toEitherNec,
      target.config.getOpt[LinkValidation].toEitherNec
    ).parSequence.fold(errs => Seq(DocumentErrors(Root, errs)), _ => Nil)

    val validations = NonEmptyChain
      .fromSeq(target.allDocuments.flatMap(validate) ++ validateRoot)
      .map(TreeErrors.apply)
      .toLeft(())

    def translatorConfig =
      TranslatorConfig.readFrom(target.config)
        .leftMap(err => TreeErrors(NonEmptyChain.one(DocumentErrors(Root, NonEmptyChain.one(err)))))

    for {
      _             <- validations
      renderContext <- outputContext.traverse(ctx => translatorConfig.map(cfg => (ctx, cfg)))
    } yield new RootCursor(target, renderContext)
  }

}

/** Cursor for an entire document tree, providing access to all child cursors of this tree
  * and allowing to trigger rewrite operations.
  *
  * @param target the document tree this cursor points to
  * @param parent the immediate parent of this tree or `None` if this is the root
  * @param root the root of this tree
  */
class TreeCursor private (
    val target: DocumentTree,
    val parent: Option[TreeCursor],
    val root: RootCursor
) extends Cursor {

  type Target = DocumentTree

  val path: Path = target.path

  val position: TreePosition = target.position

  val config: Config = target.config

  /** The cursor for the title document of this tree.
    */
  lazy val titleDocument: Option[DocumentCursor] = target.titleDocument.map { title =>
    DocumentCursor(
      title
        .modifyConfig(AutonumberConfig.withoutSectionNumbering(title.config))
        .withPosition(TreeNodeIndex.Inherit),
      this
    )
  }

  /** The cursors for all children of this node in the document tree.
    */
  lazy val children: Seq[Cursor] = {

    target.content.zipWithIndex map {
      case (doc: Document, index)      =>
        DocumentCursor(doc.withPosition(index + 1), this)
      case (tree: DocumentTree, index) =>
        new TreeCursor(tree.withPosition(index + 1), Some(this), root)
    }
  }

  /** All documents contained in this tree, fetched recursively, depth-first.
    * This method behaves differently than the `children` method which only returns the cursors on this level of the tree hierarchy.
    */
  lazy val allDocuments: Seq[DocumentCursor] = {

    def collect(tree: TreeCursor): Seq[DocumentCursor] =
      tree.titleDocument.toSeq ++ tree.children.flatMap {
        case doc: DocumentCursor => Seq(doc)
        case sub: TreeCursor     => collect(sub)
      }

    collect(this)
  }

  private[laika] def applyPosition(index: Int): TreeCursor =
    new TreeCursor(target.withPosition(index), parent, root)

  /** Returns a new tree, with all the document models contained in it
    * rewritten based on the specified rewrite rule.
    */
  def rewriteTarget(rules: RewriteRulesBuilder): Either[TreeErrors, DocumentTree] = {

    val sortedContent = NavigationOrder
      .applyTo(children, config)
      .leftMap(err => NonEmptyChain.one(DocumentErrors(path, err)))

    val rewrittenTitle = titleDocument
      .traverse { doc =>
        doc
          .rewriteTarget(rules)
          .toEitherNec
      }

    val rewrittenContent: Either[NonEmptyChain[DocumentErrors], Seq[TreeContent]] =
      sortedContent.flatMap(
        _.toList
          .map {
            case doc: DocumentCursor => doc.rewriteTarget(rules).toEitherNec
            case tree: TreeCursor    => tree.rewriteTarget(rules).leftMap(_.failures)
          }
          .parSequence
      )

    (rewrittenTitle, rewrittenContent).parMapN { (title, content) =>
      target.replaceContent(content).withTitleDocument(title)
    }
      .leftMap(TreeErrors.apply)
  }

}

object TreeCursor {

  private[ast] def apply(root: RootCursor): TreeCursor =
    new TreeCursor(root.target.tree, None, root)

  /** Creates a new cursor for the specified document tree.
    */
  def apply(
      root: DocumentTree,
      outputContext: Option[OutputContext] = None
  ): Either[TreeErrors, TreeCursor] =
    RootCursor(DocumentTreeRoot(root), outputContext).map(apply)

}

/** Cursor for a single document, its parent, siblings and root directories,
  *  its associated template and other context information that
  *  is required during a rewrite operation.
  *
  *  @param target the document this cursor points to
  *  @param parent the parent document tree of the referred document
  *  @param resolver the resolver for references in templates
  *  @param templatePath the path of the template that has been applied to this document
  */
class DocumentCursor private (
    val target: Document,
    val parent: TreeCursor,
    resolver: ReferenceResolver,
    val templatePath: Option[Path]
) extends Cursor { self =>

  type Target = Document

  val path: Path = target.path

  val position: TreePosition = target.position

  lazy val root: RootCursor = parent.root

  /** The configuration for this document cursor,
    * containing all config values of the underlying target document,
    * plus additional values for navigating parents and siblings.
    */
  val config: Config = resolver.config

  private lazy val validator = new LinkValidator(this, root.targetLookup)

  /** Returns a new, rewritten document model based on the specified rewrite rules.
    */
  def rewriteTarget(rules: RewriteRulesBuilder): Either[DocumentErrors, Document] =
    rules(this).map(rewriteTarget).leftMap(DocumentErrors(path, _))

  /** Returns a new, rewritten document model based on the specified rewrite rules.
    */
  def rewriteTarget(rules: RewriteRules): Document = {

    val rewrittenRoot = rules.rewriteBlock(target.content) match {
      case r: RootElement => r
      case b              => target.content.withContent(Seq(b))
    }

    val rewrittenFragments = target.fragments.mapValuesStrict {
      case r: RewritableContainer with Block => r.rewriteChildren(rules).asInstanceOf[Block]
      case block                             => block
    }

    val newFragments = DocumentFragment.collect(rewrittenRoot)

    target.withContent(
      content = rewrittenRoot.withContent(
        rewrittenRoot.content.filterNot(_.isInstanceOf[DocumentFragment])
      ),
      fragments = rewrittenFragments ++ newFragments
    )
  }

  class Siblings(documents: Vector[DocumentCursor]) {
    lazy val currentIndex: Int = documents.indexWhere(_.path == path)

    /** The previous document in a flattened tree view or None if this is the cursor points to the first document.
      */
    def previousDocument: Option[DocumentCursor] =
      if (currentIndex <= 0) None else Some(documents(currentIndex - 1))

    /** The next document in a flattened tree view or None if this is the cursor points to the last document.
      */
    def nextDocument: Option[DocumentCursor] =
      if (currentIndex + 1 == documents.size) None else Some(documents(currentIndex + 1))

  }

  /** Provides navigation capabilities to the siblings of this document where the entire
    * hierarchical tree view is flattened into a single book-like list of documents.
    */
  def flattenedSiblings: Siblings = new Siblings(root.allDocuments.toVector)

  private lazy val hierarchicalSiblings: Siblings = {

    def collectSiblings(tree: TreeCursor): Vector[DocumentCursor] = tree.children.toVector.flatMap {
      case d: DocumentCursor => Some(d)
      case t: TreeCursor     => t.titleDocument
    }

    val siblingDocs =
      if (parent.titleDocument.map(_.path).contains(path))
        parent.parent.toVector.flatMap(collectSiblings)
      else collectSiblings(parent)

    new Siblings(siblingDocs)
  }

  /** The previous document in a hierarchical tree view or None if this cursor points to the first document
    * in the current sub-tree.
    * Use `flattenedSiblings.previousDocument` for navigation beyond the current sub-tree in a book-like flattened
    * view.
    */
  def previousDocument: Option[DocumentCursor] = hierarchicalSiblings.previousDocument

  /** The next document in a hierarchical tree view or None if this cursor points to the last document
    * in the current sub-tree.
    * Use `flattenedSiblings.nextDocument` for navigation beyond the current sub-tree in a book-like flattened
    * view.
    */
  def nextDocument: Option[DocumentCursor] = hierarchicalSiblings.nextDocument

  /** Resolves the context reference with the specified path relative to
    *  this document. A reference `config.value` for example will first
    *  look up the value in the configuration of this document and then,
    *  if not found, recursively look it up in parent trees until the
    *  root tree is reached. If the value is not found `None` will
    *  be returned.
    */
  def resolveReference(key: Key): ConfigResult[Option[ConfigValue]] = resolver.resolve(key)

  /** Creates a copy of this cursor with a new root object
    *  for resolving references. This is useful for custom
    *  template directives which need to provide a new scope
    *  for a nested part inside the directive tags.
    */
  def withReferenceContext[T: ConfigEncoder](refValue: T): DocumentCursor =
    new DocumentCursor(
      target,
      parent,
      new ReferenceResolver(config.withValue("_", refValue).build),
      templatePath
    )

  private[laika] def applyPosition(index: Int): DocumentCursor =
    new DocumentCursor(target.withPosition(index), parent, resolver, templatePath)

  /** Creates a new cursor for this document where the specified template
    * is associated with the new cursor.
    */
  private[laika] def applyTemplate(
      template: TemplateDocument
  ): Either[ConfigError, DocumentCursor] =
    template.config.resolve(
      Origin(Origin.TemplateScope, template.path),
      config,
      root.target.includes
    ).map { templateConfig =>
      val mergedTarget = target.withTemplateConfig(templateConfig.withoutFallback)
      new DocumentCursor(
        mergedTarget,
        parent,
        resolver = ReferenceResolver.forDocument(
          mergedTarget,
          parent
        ),
        templatePath = Some(template.path)
      )
    }

  /** Validates the specified link, verifying that the target exists and supports a matching set of target formats.
    * The returned ADT provides one of three possible outcomes, apart from a valid or invalid result,
    * a target may also be a `RecoveredTarget` where the link target exists, but does not support all
    * the output formats of the link source.
    * In such a case some link types can switch to an external link, pointing to the location of the hosted
    * html result of this document tree.
    */
  def validate(target: InternalTarget): TargetValidation = validator.validate(target)

  /** Validates the specified link, verifying that the target exists and supports a matching set of target formats.
    * The returned link in case of successful validation might be a modified link with enhanced information for the
    * renderer, which might translate internal links to external links for some output formats.
    */
  def validate[L <: Link](link: L): Either[String, L] = validator.validate(link)

  /** Validates the specified link, verifying that the target exists and supports a matching set of target formats.
    * Performs the same checks as the `validate` method, but instead of returning potential errors in an Either
    * it replaces invalid links with instances of `InvalidSpan`.
    * Those types of AST nodes require access to the original source that produced the element which is either
    * obtained from a parser or from a directive combinator.
    */
  def validateAndRecover(link: Link, source: SourceFragment): Span =
    validator.validateAndRecover(link, source)

}

object DocumentCursor {

  /** Creates a cursor by placing the specified document as a sole node into an otherwise empty document tree.
    */
  def apply(
      document: Document,
      outputContext: Option[OutputContext] = None
  ): Either[TreeErrors, DocumentCursor] = {
    val orphanDoc = document.modifyConfig(_.withValue(LaikaKeys.orphan, true))
    TreeCursor(DocumentTree.builder.addDocument(orphanDoc).build, outputContext)
      .map(apply(orphanDoc, _))
  }

  /** Creates a cursor for a document and full context information:
    * its parent, configuration and position within the document tree.
    */
  private[ast] def apply(
      document: Document,
      parent: TreeCursor
  ): DocumentCursor =
    new DocumentCursor(
      document,
      parent,
      ReferenceResolver.forDocument(document, parent),
      None
    )

}
