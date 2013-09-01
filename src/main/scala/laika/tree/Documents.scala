/*
 * Copyright 2013 the original author or authors.
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

package laika.tree

import laika.tree.Elements.RootElement
import laika.tree.Elements.Span
import laika.tree.Elements.Element

/** 
 *  @author Jens Halm
 */
object Documents {
  
  case class Document (path: Path, 
                       title: Seq[Span], 
                       info: DocumentInfo, 
                       content: RootElement, 
                       rewriteRules: Seq[DocumentContext => PartialFunction[Element,Option[Element]]]) {
    
    val name = path.name
  
    // lazy val targets TODO - implement (alternatively only expose resolveReference method)
    
    // lazy val sections TODO - implement
  
    val isRewritten = rewriteRules.isEmpty
    
    
    def rewrite: Document = rewrite(Nil)
     
    def rewrite (customRule: PartialFunction[Element,Option[Element]]): Document = rewrite(List(customRule))
    
    def rewrite (customRules: Seq[PartialFunction[Element,Option[Element]]]): Document = {
      
      val defaultRules = (rewriteRules map { _(DocumentContext(this)) })      
      
      val allRules = RewriteRules chain (customRules ++ defaultRules)
      
      val newRoot = content rewrite allRules
      
      copy(content = newRoot, rewriteRules = Nil)
    }

    def removeRules = if (rewriteRules.isEmpty) this else copy(rewriteRules = Nil)
    
  }
  
  case class DocumentInfo (/* TODO - define */)
  
  case class DocumentContext (document: Document, parent: DocumentTree, root: DocumentTree)
  
  case object DocumentContext {
    def apply (document: Document) = {
      val tree = DocumentTree(Root, Seq(document), Nil)
      new DocumentContext(document, tree, tree)
    }
  }
  
  case class DocumentTree (path:Path, documents: Seq[Document], subtrees: Seq[DocumentTree]) {
    
    val name = path.name
    
    private val documentsByName = documents map {doc => (doc.name, doc)} toMap // TODO - handle duplicates
    private val subtreesByName = subtrees map {tree => (tree.name, tree)} toMap

    def selectDocument (path: String): Option[Document] = selectDocument(Path(path))
    def selectDocument (path: Path): Option[Document] = path match {
      case Root => None
      case Root / name => documentsByName.get(name)
      case path / name => selectSubtree(path) flatMap (_.selectDocument(name)) 
    }
    
    def selectSubtree (path: String): Option[DocumentTree] = selectSubtree(Path(path))
    def selectSubtree (path: Path): Option[DocumentTree] = path match {
      case Root => None
      case Root / name => subtreesByName.get(name)
      case path / name => selectSubtree(path) flatMap (_.selectSubtree(name)) 
    }
    
    def rewrite: DocumentTree = rewrite(Nil, this)
     
    def rewrite (customRule: DocumentContext => PartialFunction[Element,Option[Element]]): DocumentTree = rewrite(List(customRule), this)
    
    def rewrite (customRules: Seq[DocumentContext => PartialFunction[Element,Option[Element]]]): DocumentTree = rewrite(customRules, this)
    
    private def rewrite (customRules: Seq[DocumentContext => PartialFunction[Element,Option[Element]]], root: DocumentTree): DocumentTree = {
      val docs = documents map (doc => doc.rewrite(customRules map (_(DocumentContext(doc, this, root)))))
      val trees = subtrees map (_.rewrite(customRules, root))
      DocumentTree(path, docs, trees)  
    }
  }
  
  abstract class Path {
    def / (name: String) = new /(this, name)
    def components: List[String]
    def parent: Path
    def name: String
  }
 
  case class / (parent: Path, name: String) extends Path {
    lazy val components: List[String] = parent.components ++ List(name)
    override lazy val toString = components mkString "/"
  }
  
  case object Root extends Path {
    def components: List[String] = Nil
    def parent = this
    val name = ""
    override val toString = ""
  }

  object Path {
    def apply(str: String): Path = {
      val trimmed = str.trim.stripPrefix("/").stripSuffix("/")
      if (trimmed.isEmpty) Root
      else apply(trimmed.split("/").toList)
    }
  
    def apply(list: List[String]): Path = list.foldLeft(Root: Path)(_ / _)
  }
  
  
}