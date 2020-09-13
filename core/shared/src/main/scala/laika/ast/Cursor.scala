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

import laika.ast.Path.Root
import laika.collection.TransitionalCollectionOps._
import laika.config.Config.ConfigResult
import laika.config.{Config, ConfigEncoder, ConfigValue, Key}
import laika.rewrite.ReferenceResolver
import laika.rewrite.nav.{AutonumberConfig, NavigationOrder}

/** A cursor provides the necessary context during a rewrite operation.
  * The stateless document tree cannot provide access to parent or sibling
  * nodes in the tree, therefore a temporary cursor instance is created
  * during a rewrite operation for this purpose.
  */
sealed trait Cursor {

  /** The type of the target this cursor points to.
    */
  type Target

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
  * @param targetFormat the format the cursor is applied for, empty in case of the 1st rewrite step which 
  *                     is applied to all formats.
  */
case class RootCursor(target: DocumentTreeRoot, targetFormat: Option[String] = None) {
  
  type Target = DocumentTreeRoot
  
  val config: Config = target.config

  /** The cursor for the underlying tree structure of this root node.
    */
  val tree: TreeCursor = TreeCursor(this)
  
  /** The cursor for the cover document of this tree.
    */
  lazy val coverDocument: Option[DocumentCursor] = target.coverDocument.map { cover =>
    DocumentCursor(cover, tree, cover.config, TreePosition.root)
  }

  /** All documents contained in this tree, fetched recursively, depth-first.
    */
  lazy val allDocuments: Seq[DocumentCursor] = coverDocument.toSeq ++ tree.allDocuments

  /** Returns a new tree root, with all the document models contained in it
    * rewritten based on the specified rewrite rule.
    */
  def rewriteTarget (rules: DocumentCursor => RewriteRules): DocumentTreeRoot = {

    val rewrittenCover = coverDocument.map(doc => doc.rewriteTarget(rules(doc)))
    val rewrittenTree = tree.rewriteTarget(rules)

    target.copy(coverDocument = rewrittenCover, tree = rewrittenTree)
  }
  
}

/** Cursor for an entire document tree, providing access to all
  * child cursors of this tree and allowing to trigger rewrite
  * operations.
  *
  * @param target the document tree this cursor points to
  * @param parent the immediate parent of this tree or `None` if this is the root
  * @param root the root of this tree              
  * @param config the configuration associated with this tree
  * @param position the position of this tree within the document tree
  */
case class TreeCursor(target: DocumentTree, 
                      parent: Option[TreeCursor],
                      root: RootCursor,
                      config: Config,
                      position: TreePosition) extends Cursor {

  type Target = DocumentTree

  val path: Path = target.path
  
  /** The cursor for the title document of this tree.
    */
  lazy val titleDocument: Option[DocumentCursor] = target.titleDocument.map { title =>
    DocumentCursor(title, this, title.config, position)
  }

  /** The cursors for all children of this node in the document tree.
    */
  lazy val children: Seq[Cursor] = {
    
    target.content.zipWithIndex map {
      case (doc: Document, index)      => DocumentCursor(doc, this, doc.config, position.forChild(index + 1))
      case (tree: DocumentTree, index) => TreeCursor(tree, Some(this), root, tree.config, position.forChild(index + 1))
    }
  }

  /** All documents contained in this tree, fetched recursively, depth-first.
    * This method behaves differently than the `children` method which only returns the cursors on this level of the tree hierarchy.
    */
  lazy val allDocuments: Seq[DocumentCursor] = {

    def collect (tree: TreeCursor): Seq[DocumentCursor] = tree.titleDocument.toSeq ++ tree.children.flatMap {
      case doc: DocumentCursor => Seq(doc)
      case sub: TreeCursor => collect(sub)
    }

    collect(this)
  }

  /** Returns a new tree, with all the document models contained in it
    * rewritten based on the specified rewrite rule.
    */
  def rewriteTarget (rules: DocumentCursor => RewriteRules): DocumentTree = {
      
    val sortedContent = NavigationOrder.applyTo(children, config, position)

    val rewrittenTitle = titleDocument.map { doc =>
      doc
        .copy(config = AutonumberConfig.withoutSectionNumbering(doc.config))
        .rewriteTarget(rules(doc))
    }
    
    val rewrittenContent = sortedContent map {
      case doc: DocumentCursor => doc.rewriteTarget(rules(doc))
      case tree: TreeCursor => tree.rewriteTarget(rules)
    }

    target.copy(content = rewrittenContent, titleDocument = rewrittenTitle, position = position)
  }

}

object TreeCursor {
  
  def apply (root: RootCursor): TreeCursor =
    apply(root.target.tree, None, root, root.config, TreePosition.root)

  def apply (root: DocumentTree, format: Option[String] = None): TreeCursor =
    apply(RootCursor(DocumentTreeRoot(root), format))

}

/** Cursor for a single document, its parent, siblings and root directories,
 *  its associated template and other context information that
 *  is required during a rewrite operation.
 *  
 *  @param target the document this cursor points to
 *  @param parent the parent document tree of the referred document
 *  @param resolver the resolver for references in templates
 *  @param config the configuration associated with the target document
 *  @param templatePath the path of the template that has been applied to this document
 *  @param position the position of the target document inside a tree hierarchy
 */
case class DocumentCursor (target: Document, 
                           parent: TreeCursor,
                           resolver: ReferenceResolver,
                           config: Config,
                           templatePath: Option[Path],
                           position: TreePosition) extends Cursor { self =>
                                 
  type Target = Document

  val path: Path = target.path

  lazy val root: RootCursor = parent.root

  /** Returns a new, rewritten document model based on the specified rewrite rules.
   */
  def rewriteTarget (rules: RewriteRules): Document = {
    
    val rewrittenRoot = rules.rewriteBlock(target.content) match {
      case r: RootElement => r
      case b => target.content.copy(content = Seq(b))
    }
       
    val rewrittenFragments = target.fragments mapValuesStrict {
      case r: RewritableContainer with Block => r.rewriteChildren(rules).asInstanceOf[Block]
      case block => block
    }
    
    val newFragments = rewrittenRoot.content.collect {
      case DocumentFragment(name, content, _) => (name, content)
    }.toMap
    
    target.copy(
      content = rewrittenRoot.copy(content = rewrittenRoot.content.filterNot(_.isInstanceOf[DocumentFragment])), 
      fragments = rewrittenFragments ++ newFragments, 
      position = position
    )
  }
  
  class Siblings (documents: Vector[DocumentCursor]) {
    lazy val currentIndex: Int = documents.indexWhere(_.path == path)

    /** The previous document in a flattened tree view or None if this is the cursor points to the first document.
      */
    def previousDocument: Option[DocumentCursor] = if (currentIndex <= 0) None else Some(documents(currentIndex - 1))

    /** The next document in a flattened tree view or None if this is the cursor points to the last document.
      */
    def nextDocument: Option[DocumentCursor] = if (currentIndex + 1 == documents.size) None else Some(documents(currentIndex + 1))
  }

  /** Provides navigation capabilities to the siblings of this document where the entire
    * hierarchical tree view is flattened into a single book-like list of documents.
    */
  def flattenedSiblings: Siblings = new Siblings(root.allDocuments.toVector)
  
  private lazy val hierarchicalSiblings: Siblings = {
    
    def collectSiblings (tree: TreeCursor): Vector[DocumentCursor] = tree.children.toVector.flatMap { 
      case d: DocumentCursor => Some(d)
      case t: TreeCursor => t.titleDocument
    }
    
    val siblingDocs =
      if (parent.titleDocument.map(_.path).contains(path)) parent.parent.toVector.flatMap(collectSiblings)
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
  def resolveReference (key: Key): ConfigResult[Option[ConfigValue]] = resolver.resolve(key)
  
  /** Creates a copy of this cursor with a new root object
   *  for resolving references. This is useful for custom
   *  template directives which need to provide a new scope
   *  for a nested part inside the directive tags.
   */
  def withReferenceContext[T: ConfigEncoder](refValue: T): DocumentCursor =
    copy(resolver = ReferenceResolver(resolver.config.withValue("_", refValue).build))
  
}

object DocumentCursor {

  /** Creates a cursor by placing the specified document as a sole node into an otherwise empty document tree. */
  def apply (document: Document, targetFormat: Option[String] = None): DocumentCursor =
    apply(document, TreeCursor(DocumentTree(Root, Seq(document)), targetFormat), document.config, document.position)

  /** Creates a cursor for a document and full context information:
    * its parent, configuration and position within the document tree.
    */
  def apply (document: Document, parent: TreeCursor, config: Config, position: TreePosition): DocumentCursor =
    apply(document, parent, ReferenceResolver.forDocument(document, parent, config, position), config, None, position)
    
}
