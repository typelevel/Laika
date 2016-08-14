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

package laika.tree

import laika.io.InputProvider
import laika.io.Input
import laika.parse.css.Styles.StyleDeclarationSet
import laika.tree.Paths.Path
import laika.tree.Paths.Root
import laika.tree.Paths.Current
import laika.tree.Paths./
import laika.tree.Documents.AutonumberConfig
import laika.tree.Elements._
import laika.tree.Templates.TemplateDocument
import laika.tree.Templates.TemplateRoot
import laika.tree.Templates.TemplateContextReference
import laika.rewrite.LinkTargets._
import laika.rewrite.LinkResolver
import laika.rewrite.DocumentCursor
import laika.rewrite.TreeCursor
import laika.rewrite.TreeUtil
import scala.annotation.tailrec
import scala.util.Try
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory

/*
 * TODO - after merge:
 * 
 * - check which parts insert default rules into the document instance
 * - convenient API for accessing/applying default rules for MD and RST
 * 
 */


/** Provides the API for Documents and DocumentTrees.
 *  
 *  @author Jens Halm
 */
object Documents2 {
  
  val defaultTemplate: TemplateDocument = new TemplateDocument(Root / "default.template", TemplateRoot(List(TemplateContextReference("document.content"))))
  
  
  /** A navigatable object is anything that has an associated path.
   */
  trait Navigatable {
    
    def path: Path
    
    /** The local name of this navigatable.
     */
    lazy val name: String = path.name
    
  }
  
  
  sealed trait TreeContent extends Navigatable {
    
    def title: Seq[Span]
    
    def config: Config
    
    def docNumber: List[Int]
    
    def linkTargets: Map[Selector, TargetResolver]
    
    /** Selects a link target by the specified selector
     *  if it is defined somewhere in a document inside this document tree.
     */
    def selectTarget (selector: Selector): Option[TargetResolver] = linkTargets.get(selector)
    
  }
  
  
  sealed trait AdditionalContent extends Navigatable
  
  case class StaticDocument (input: Input) extends AdditionalContent {
    val path: Path = input.path
  }
  
  case class DynamicTemplate (path: Path, content: TemplateRoot, config: Config = ConfigFactory.empty) extends AdditionalContent
  
  case class DynamicDocument (path: Path, 
                              content: RootElement, 
                              fragments: Map[String, Element] = Map.empty,
                              config: Config = ConfigFactory.empty) extends AdditionalContent
  
  
  /** Captures information about a document section, without its content.
   */
  case class SectionInfo (position: List[Int], id: String, title: TitleInfo, content: Seq[SectionInfo]) extends Element with ElementContainer[SectionInfo,SectionInfo] {
    val level: Int = position.length
  }

  /** Represents a section title.
   */
  case class TitleInfo (content: Seq[Span]) extends SpanContainer[TitleInfo] {
    lazy val text: String = TreeUtil.extractText(content)
  }

  trait DocumentStructure { this: TreeContent =>
  
    def content: RootElement
    
    private def findRoot: Seq[Block] = {
      (content select {
        case RootElement(TemplateRoot(_,_) :: Nil) => false
        case RootElement(_) => true
        case _ => false
      }).headOption map { case RootElement(content) => content } getOrElse Nil
    }
    
    /** The title of this document, obtained from the document
     *  structure or from the configuration.
     */
    def title: Seq[Span] = {
      // TODO - after merge: put docNumber into config, making the docNumber property obsolete in this object
      if (config.hasPath("title")) docNumber match { // TODO - this is duplicated with DocumentTree, move to TreeContent
        case Nil => List(Text(config.getString("title")))
        case _ => Text(docNumber.mkString("","."," "), Styles("titleNumber")) +: List(Text(config.getString("title")))
      }
      else (findRoot collect {
        case Title(content,_) => content
      }).headOption getOrElse List(Text("")) // TODO - after merge: could this be Nil?
    }
  
    /** The section structure of this document based on the hierarchy
     *  of headers found in the original text markup.
     */
    lazy val sections: Seq[SectionInfo] = {
      
      def extractSections (parentPos: List[Int], blocks: Seq[Block]): Seq[SectionInfo] = {
        val positions = Stream.from(1).iterator
        blocks collect {
          case Section(Header(_,header,Id(id)), content, _) => {
            val pos = parentPos :+ positions.next 
            SectionInfo(pos, id, TitleInfo(header), extractSections(pos, content)) 
          }
        }
      }
      // TODO - after merge: new class SectionNumber should be embedded right in the tree, making the docNumber property obsolete in this object
      extractSections(docNumber, findRoot)
    } 
    
    private lazy val linkResolver: LinkResolver = LinkResolver(path, content)
    
    lazy val linkTargets: Map[Selector, TargetResolver] = linkResolver.globalTargets ++ (linkResolver.globalTargets collect {
      case (UniqueSelector(name), target) => (PathSelector(path, name), target)
    })
    
  }
  
  trait TreeStructure { this: TreeContent =>
    
    def content: Seq[TreeContent]
    
    def templates: Seq[TemplateDocument]
    
    def targetTree: DocumentTree
    
    /** The title of this tree, obtained from configuration.
     */
    lazy val title: Seq[Span] = {
      if (config.hasPath("title")) docNumber match {
        case Nil => List(Text(config.getString("title")))
        case _ => Text(docNumber.mkString("","."," "), Styles("titleNumber")) +: List(Text(config.getString("title")))
      } else Nil 
    }
    
    private def toMap [T <: Navigatable] (navigatables: Seq[T]): Map[String,T] = {
      navigatables groupBy (_.name) mapValues {
        case Seq(nav) => nav
        case multiple => throw new IllegalStateException("Multiple navigatables with the name " +
            s"${multiple.head.name} in tree $path")
      }
    }
    
    private val documentsByName = toMap(content collect {case d: Document => d})
    private val templatesByName = Map[String, TemplateDocument]() // toMap(templates) - TODO - after merge: TemplateDocument must extend this file's Navigatable
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
    
    lazy val linkTargets: Map[Selector, TargetResolver] = {
      val all = (List[(Selector,TargetResolver)]() /: content) { 
        case (list, tree: DocumentTree) => tree.linkTargets.toList ::: list
        case (list, doc: Document)      => doc.linkTargets.toList ::: list
      }
      (all.groupBy (_._1) collect {
        case (selector, ((_,target) :: Nil)) => (selector, target)
        case (s @ UniqueSelector(name), conflicting) => (s, DuplicateTargetResolver(path, name))
      }).toMap
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
   *  @param docNumber the number of this document inside a document tree hierarchy, expressed as a list of Ints
   */
  case class Document (path: Path, 
                       content: RootElement, 
                       fragments: Map[String, Element] = Map.empty,
                       config: Config = ConfigFactory.empty,
                       docNumber: List[Int] = Nil) extends DocumentStructure with TreeContent {
    
    /** Returns a new, rewritten document model based on the specified rewrite rule.
     *  
     *  If the specified partial function is not defined for a specific element the old element remains
     *  in the tree unchanged. If it returns `None` then the node gets removed from the tree, 
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
   *  @param docNumber the number of this tree inside a document tree hierarchy, expressed as a list of Ints
   *  @param sourcePaths the paths this document tree has been built from or an empty list if this tree does not originate from the file system
   */
  case class DocumentTree (path:Path, 
                           content: Seq[TreeContent],
                           templates: Seq[TemplateDocument] = Nil, 
                           styles: Map[String,StyleDeclarationSet] = Map.empty.withDefaultValue(StyleDeclarationSet.empty),
                           additionalContent: Seq[AdditionalContent] = Nil,
                           config: Config = ConfigFactory.empty,
                           docNumber: List[Int] = Nil,
                           sourcePaths: Seq[String] = Nil) extends TreeStructure with TreeContent {
    
    val targetTree = this
    
    /** Returns a new tree, with all the document models contained in it 
     *  rewritten based on the specified rewrite rule.
     *  
     *  If the specified partial function is not defined for a specific element the old element remains
     *  in the tree unchanged. If it returns `None` then the node gets removed from the tree, 
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
  
  /** Factory for creating the navigation order for the markup
   *  documents and subtrees of a tree based on its configuration.
   */
  object NavigationOrder {
    import scala.collection.JavaConversions.iterableAsScalaIterable
    
    def defaults: Navigatable => String = _.path.name
    
    def fromConfig (config: Config): Option[Navigatable => Int] = {
      if (config.hasPath("navigationOrder")) {
        val list = iterableAsScalaIterable(config.getList("navigationOrder").unwrapped).collect{case s:String => s}.toIndexedSeq
        Some(nav => list.indexOf(nav.path.name) match { case -1 => Int.MaxValue; case other => other })
      }
      else None
    }
    
  }
  
  
}
