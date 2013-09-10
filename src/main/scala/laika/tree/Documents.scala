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
import laika.tree.Templates.TemplateDocument
import scala.annotation.tailrec

/** 
 *  @author Jens Halm
 */
object Documents {
  
  class Document (val path: Path, 
                  val title: Seq[Span], 
                  val info: DocumentInfo, 
                  val content: RootElement, 
                  val template: Option[TemplateDocument],
                  rewriteRules: Seq[DocumentContext => PartialFunction[Element,Option[Element]]] = Nil) {
    
    private lazy val linkResolver = LinkResolver(content)
    
    lazy val defaultRules = rewriteRules :+ (linkResolver.rewriteRules(_)) :+ {(_:DocumentContext) => SectionBuilder()}
    
    val name = path.name
  
    // lazy val targets TODO - implement (alternatively only expose resolveReference method)
    
    // lazy val sections TODO - implement
  
    val isRewritten = rewriteRules.isEmpty
    
    
    def rewrite: Document = rewrite(Nil)
     
    def rewrite (customRule: PartialFunction[Element,Option[Element]]): Document = rewrite(List(customRule))
    
    def rewrite (customRules: Seq[PartialFunction[Element,Option[Element]]]): Document = {
      
      val resolvedRules = (defaultRules map { _(DocumentContext(this)) })      
      
      val allRules = RewriteRules chain (customRules ++ resolvedRules)
      
      val newRoot = content rewrite allRules
      
      val newDoc = withRewrittenContent(newRoot)
      
      template map (_.rewrite(DocumentContext(newDoc))) getOrElse newDoc // TODO - ensure template only gets applied once
    }
    
    def withRewrittenContent (newContent: RootElement) = new Document(path, title, info, newContent, template) {
      override lazy val defaultRules = Nil
      override val removeRules = this
    }

    def removeRules: Document = withRewrittenContent(content)
    
  }
  
  case class DocumentInfo (/* TODO - define */)
  
  case class DocumentContext (document: Document, parent: DocumentTree, root: DocumentTree)
  
  case object DocumentContext {
    def apply (document: Document) = {
      val tree = DocumentTree(Root, Seq(document))
      new DocumentContext(document, tree, tree)
    }
  }
  
  case class DocumentTree (path:Path, documents: Seq[Document], subtrees: Seq[DocumentTree] = Nil, defaultTemplate: Option[TemplateDocument] = None) {
    
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
      val docs = documents map (doc => doc.rewrite(customRules map (_(DocumentContext(doc, this, root))))) // TODO - context is not getting passed down
      val trees = subtrees map (_.rewrite(customRules, root))
      DocumentTree(path, docs, trees, defaultTemplate)  
    }
  }
  
  sealed abstract class Path {
    def parent: Path
    def name: String
    def prefix: PathPrefix
    def components: List[String]
    def / (name: String) = new /(this, name)
    def / (path: Path): Path = path.prefix match {  
      case Root => path
      case Current => Path(prefix, components ::: path.components)
      case Parent(1) => parent / Path(Current, path.components)
      case Parent(i) => parent / Path(Parent(i-1), path.components)
    }
  }
 
  case class / (parent: Path, name: String) extends Path {
    lazy val components: List[String] = parent.components ++ List(name)
    lazy val prefix = parent.prefix
    override lazy val toString = components mkString "/"
  }
  
  abstract class PathPrefix (val name: String) extends Path {
    val components: List[String] = Nil
    val parent = this
    val prefix = this
    override val toString = name
  }
  
  case object Root extends PathPrefix ("/")

  case object Current extends PathPrefix ("")
  
  case class Parent (levels: Int) extends PathPrefix("../" * levels) {
    require(levels > 0)
  }

  object Path {
    def apply(str: String): Path = {
      val trimmed = str.trim.stripSuffix("/")
      val (parent, rest) = 
        if (trimmed.startsWith("/")) (Root, trimmed.drop(1))
        else if (trimmed.startsWith("../")) (Parent(1), trimmed.drop(3))
        else (Current, trimmed)
       apply(parent, rest.split("/").toList)
    }
  
    @tailrec def apply (parent: Path, rest: List[String]): Path = (parent, rest) match {
      case (Parent(level), ".." :: rest) => apply(Parent(level+1), rest)
      case (parent, rest) => rest.foldLeft(parent)(_ / _)
    } 
  }
  
  
}