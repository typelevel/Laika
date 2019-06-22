/*
 * Copyright 2016 the original author or authors.
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

import com.typesafe.config.Config
import laika.ast.Path.Root
import laika.collection.TransitionalCollectionOps._
import laika.rewrite.ReferenceResolver
import laika.rewrite.nav.NavigationOrder

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
  */
case class RootCursor(target: DocumentTreeRoot) {
  
  type Target = DocumentTreeRoot
  
  val config: Config = target.config

  /** The cursor for the underlying tree structure of this root node.
    */
  val tree: TreeCursor = TreeCursor(this)
  
  /** The cursor for the cover document of this tree.
    */
  lazy val coverDocument: Option[DocumentCursor] = target.coverDocument.map { cover =>
    DocumentCursor(cover, tree, cover.config.withFallback(config).resolve, TreePosition.root)
  }

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
    DocumentCursor(title, this, title.config.withFallback(config).resolve, position)
  }

  /** The cursors for all children of this node in the document tree.
    */
  lazy val children: Seq[Cursor] = {
    
    def configForChild(childConfig: Config) = childConfig.withFallback(config).resolve
    
    target.content.zipWithIndex map {
      case (doc: Document, index) => 
        val config = configForChild(doc.config)
        DocumentCursor(doc, this, config, position.forChild(index + 1))
      case (tree: DocumentTree, index) => 
        val config = configForChild(tree.config)
        TreeCursor(tree, Some(this), root, config, position.forChild(index + 1))
    }
  }

  /** Returns a new tree, with all the document models contained in it
    * rewritten based on the specified rewrite rule.
    */
  def rewriteTarget (rules: DocumentCursor => RewriteRules): DocumentTree = {
      
    val sortedContent = NavigationOrder.applyTo(children, config, position)

    val rewrittenTitle = titleDocument.map(doc => doc.rewriteTarget(rules(doc)))
    
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

  def apply (root: DocumentTreeRoot): TreeCursor =
    apply(RootCursor(root))

  def apply (root: DocumentTree): TreeCursor =
    apply(DocumentTreeRoot(root))
    
}

/** Cursor for a single document, its parent and root directories,
 *  its associated template and other context information that
 *  is required during a rewrite operation.
 *  
 *  @param target the document this cursor points to
 *  @param parent the parent document tree of the referred document
 *  @param resolver the resolver for references in templates
 *  @param config the configuration associated with the target document
 *  @param position the position of the target document inside a tree hierarchy
 */
case class DocumentCursor (target: Document, 
                           parent: TreeCursor,
                           resolver: ReferenceResolver,
                           config: Config,
                           position: TreePosition) extends Cursor { self =>
                                 
  type Target = Document

  val path: Path = target.path

  lazy val root: RootCursor = parent.root

  /** Returns a new, rewritten document model based on the specified rewrite rules.
   */
  def rewriteTarget (rules: RewriteRules): Document = {
    
    val newRoot = rules.rewriteBlock(target.content) match {
      case r: RootElement => r
      case b => target.content.copy(content = Seq(b))
    }
       
    val newFragments = target.fragments mapValuesStrict {
      case r: RewritableContainer with Block => r.rewriteChildren(rules).asInstanceOf[Block]
      case block => block
    }
    
    target.copy(content = newRoot, fragments = newFragments, position = position)
  }
  
  /** Resolves the context reference with the specified path relative to 
   *  this document. A reference `config.value` for example will first
   *  look up the value in the configuration of this document and then,
   *  if not found, recursively look it up in parent trees until the
   *  root tree is reached. If the value is not found `None` will
   *  be returned.
   */
  def resolveReference (path: String): Option[Any] = 
    resolver.resolve(path.split("\\.").toList)
  
  /** Creates a copy of this cursor with a new root object
   *  for resolving references. This is useful for custom
   *  template directives which need to provide a new scope
   *  for a nested part inside the directive tags.
   */
  def withReferenceContext (refValue: Any): DocumentCursor =
    copy(resolver = ReferenceResolver(refValue, Some(self.resolver)))
  
}

object DocumentCursor {

  /** Creates a cursor by placing the specified document
    * as a sole node into an otherwise empty document tree.
    */
  def apply (document: Document): DocumentCursor = 
    apply(document, TreeCursor(DocumentTree(Root, Seq(document))), document.config, TreePosition.root)

  /** Creates a cursor for an empty document. The only use case within Laika is
    * for processing a so-called 'dynamic document' that consists of
    * a template that is not associated with any markup document.
    */
  def forEmptyDocument (name: String, parent: TreeCursor): DocumentCursor = {
    val emptyDoc = Document(parent.target.path / name, RootElement(Nil))
    apply(emptyDoc, parent, parent.config, TreePosition.root)
  }

  /** Creates a cursor for a document and full context information:
    * its parent, configuration and position within the document tree.
    */
  def apply (document: Document, parent: TreeCursor, config: Config, position: TreePosition): DocumentCursor =
    apply(document, parent, ReferenceResolver.forDocument(document, parent, config), config, position)
    
}
