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

package laika.rewrite

import com.typesafe.config.Config
import laika.tree.Documents._
import laika.tree.ElementTraversal
import laika.tree.Elements.{Block, RewriteRule, RootElement}
import laika.tree.Paths.Root

trait Cursor {

  type Target
  
  def target: Target

  def root: TreeCursor

  def config: Config
  
  def position: TreePosition
  
}


case class TreeCursor(target: DocumentTree, 
                      parent: Option[TreeCursor], 
                      config: Config,
                      position: TreePosition) extends Cursor {

  type Target = DocumentTree
  
  lazy val root: TreeCursor = parent.fold(this)(_.root)
  
  lazy val children: Seq[Cursor] = {
    
    def configForChild(childConfig: Config) = childConfig.withFallback(config).resolve
    
    target.content.zipWithIndex map {
      case (doc: Document, index) => 
        val config = configForChild(doc.config)
        DocumentCursor(doc, this, config, position.forChild(index + 1))
      case (tree: DocumentTree, index) => 
        val config = configForChild(tree.config)
        TreeCursor(tree, Some(this), config, position.forChild(index + 1))
    }
  }
  
  def rewriteTarget (rule: DocumentCursor => RewriteRule): DocumentTree = {
      
    val rewrittenContent = children map {
      case doc: DocumentCursor => doc.rewriteTarget(rule(doc))
      case tree: TreeCursor => tree.rewriteTarget(rule)   
    }
    
    val sortedContent = NavigationOrder.applyTo(rewrittenContent, config)
    
    target.copy(content = sortedContent, position = position)  
  }

}

object TreeCursor {
  
  def apply (tree: DocumentTree): TreeCursor =
    apply(tree, None, tree.config, TreePosition.root)
    
}

/** Represents a single document,its parent and root directories,
 *  its associated template and other context information that
 *  can not be obtained from the Document instance itself.
 *  
 *  @param target the document this cursor refers to
 *  @param parent the parent document tree of the referred document
 *  @param resolver the resolver for references in templates
 *  @param config the configuration of the referred document
 *  @param position the position of the target document inside a tree hierarchy
 */
case class DocumentCursor (target: Document, 
                           parent: TreeCursor,
                           resolver: ReferenceResolver,
                           config: Config,
                           position: TreePosition) extends Cursor { self =>
                                 
  type Target = Document   
  
  lazy val root: TreeCursor = parent.root                             
                                 
  def rewriteTarget (rule: RewriteRule): Document = {
    
    val newRoot = target.content rewrite rule
       
    val newFragments = target.fragments mapValues {
      case et: ElementTraversal[_] => (et rewrite rule).asInstanceOf[Block]
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
  
  def apply (document: Document): DocumentCursor = 
    apply(document, TreeCursor(DocumentTree(Root, Seq(document))), document.config, TreePosition.root)
    
  def forEmptyDocument (name: String, parent: TreeCursor): DocumentCursor = {
    val emptyDoc = Document(parent.target.path / name, RootElement(Nil))
    apply(emptyDoc, parent, parent.config, TreePosition.root)
  }
    
  def apply (document: Document, parent: TreeCursor, config: Config, position: TreePosition): DocumentCursor =
    apply(document, parent, ReferenceResolver.forDocument(document, parent, config), config, position)
    
}
