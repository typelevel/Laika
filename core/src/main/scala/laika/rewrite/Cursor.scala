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

import laika.tree.Documents2._
import laika.tree.Paths.Root
import laika.tree.Elements.RewriteRule
import laika.tree.Elements.Block
import laika.tree.ElementTraversal
import com.typesafe.config.ConfigFactory
import com.typesafe.config.Config

trait Cursor {

  type Target
  
  def target: Target

  def root: TreeCursor

  def config: Config
  
  def autonumbering: AutonumberCursor
  
  /* TODO - these are probably not needed in the base trait
  * def nextSibling: Option[Cursor] // nothing uses it
  * 
  * def parent // must be optional for trees but mandatory for docs
  * 
  * def children // only needed in trees
  * 
  * def position: Int // not needed when we have autonumbering
  */

}


case class TreeCursor(target: DocumentTree, 
                      parent: Option[TreeCursor] = None, 
                      config: Config = ConfigFactory.empty,
                      autonumbering: AutonumberCursor = AutonumberCursor.defaults) extends Cursor {

  type Target = DocumentTree
  
  lazy val root: TreeCursor = parent.fold(this)(_.root)
  
  lazy val children: Seq[Cursor] = {
    
    def configForChild(childConfig: Config) = childConfig.withFallback(config).resolve
    
    target.content.zipWithIndex map {
      case (doc: Document, index) => 
        val config = configForChild(doc.config)
        DocumentCursor(doc, this, config, autonumbering.forChild(index + 1, config))
      case (tree: DocumentTree, index) => 
        val config = configForChild(tree.config)
        TreeCursor(tree, Some(this), config, autonumbering.forChild(index + 1, config))
    }
  }
  
  def rewriteTarget (rule: DocumentCursor => RewriteRule): DocumentTree = {
      
    val rewrittenContent = children map {
      case doc: DocumentCursor => doc.rewriteTarget(rule(doc))
      case tree: TreeCursor => tree.rewriteTarget(rule)   
    }
    
    val sortedContent = NavigationOrder.applyTo(rewrittenContent, config)
    
    target.copy(content = sortedContent, docNumber = autonumbering.currentPosition)  
  }

}

/** Represents a single document,its parent and root directories,
 *  its associated template and other context information that
 *  can not be obtained from the Document instance itself.
 *  
 *  @param document the document this context refers to
 *  @param parent the parent document tree of the referred document
 *  @param autonumbering the context for autonumbering of document and sections
 *  @param config the configuration of the referred document
 */
case class DocumentCursor (target: Document, 
                           parent: TreeCursor,
                           resolver: ReferenceResolver,
                           config: Config = ConfigFactory.empty,
                           autonumbering: AutonumberCursor = AutonumberCursor.defaults) extends Cursor { self =>
                                 
  type Target = Document   
  
  lazy val root: TreeCursor = parent.root                             
                                 
  def rewriteTarget (rule: RewriteRule): Document = {
    
    val newRoot = target.content rewrite rule
       
    val newFragments = target.fragments mapValues {
      case et: ElementTraversal[_] => (et rewrite rule).asInstanceOf[Block]
      case block => block
    }
    
    target.copy(content = newRoot, fragments = newFragments, docNumber = autonumbering.currentPosition)
  }
  
  /** Resolves the context reference with the specified path relative to 
   *  this document. A reference `config.value` for example will first
   *  look up the value in the configuration of this document and then,
   *  if not found, recursively look it up in parent trees until the
   *  root tree is reached. If the value is not found `None` will
   *  be returned.
   */
  def resolveReference (path: String): Option[Any] = resolver.resolve(path.split("\\.").toList)
  
  /** Creates a copy of this context with a new root object
   *  for resolving references. This is useful for custom
   *  template directives which need to provide a new scope
   *  for a nested part inside the directive tags.
   */
  def withReferenceContext (refValue: Any): DocumentCursor =
    copy(resolver = new ReferenceResolver(refValue, Some(self.resolver)))
  
}

object DocumentCursor {
  
  def apply (document: Document): DocumentCursor =
    apply(document, TreeCursor(DocumentTree(Root, Seq(document))))
    
  def apply (document: Document, parent: TreeCursor): DocumentCursor =
    apply(document, parent, ConfigFactory.empty, AutonumberCursor.defaults)
    
  def apply (document: Document, parent: TreeCursor, config: Config, autonumbering: AutonumberCursor): DocumentCursor =
    apply(document, parent, ReferenceResolver.forDocument(document, parent, config), config, autonumbering)
    
}

/** Context for autonumbering of documents and sections, containing the current
 *  number and the general configuration for autonumbering.
 */
case class AutonumberCursor (config: AutonumberConfig, currentPosition: List[Int] = Nil) {
  
  val currentNumber: String = currentPosition.mkString(".")
  
  def forChild (childNum: Int, childConfig: Config): AutonumberCursor = {
    val autonumberConfig = AutonumberConfig.fromConfig(childConfig) // TODO - add back this fallback: getOrElse config
    
    if (autonumberConfig.documents) AutonumberCursor(autonumberConfig, currentPosition :+ childNum)
    else                            AutonumberCursor(autonumberConfig, Nil)
  }
  
}

object AutonumberCursor {
  def defaults: AutonumberCursor = AutonumberCursor(AutonumberConfig.defaults)
}
