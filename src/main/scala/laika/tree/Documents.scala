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

import laika.tree.Elements._
import laika.tree.Templates.TemplateDocument
import laika.tree.Elements.Reference
import laika.tree.LinkTargets._
import scala.annotation.tailrec
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import laika.io.InputProvider
import laika.io.Input
import scala.util.Try
import laika.tree.Templates.TemplateRoot

/** 
 *  @author Jens Halm
 */
object Documents {
  
  class Document (val path: Path, 
                  val content: RootElement, 
                  val fragments: Map[String, Block] = Map.empty,
                  val config: Config = ConfigFactory.empty,
                  docNumber: List[Int] = Nil,
                  rewriteRules: Seq[DocumentContext => PartialFunction[Element,Option[Element]]] = Nil) extends Navigatable {
    
    private lazy val linkResolver = LinkResolver(path,content)
    
    lazy val defaultRules = rewriteRules :+ (linkResolver.rewriteRules(_)) :+ (SectionBuilder(_))
    
    private[Documents] lazy val targets = linkResolver.globalTargets ++ (linkResolver.globalTargets collect {
      case (UniqueSelector(name), target) => (PathSelector(path, name), target)
    })
    
    val name = path.name
    
    private def findRoot = {
      (content select {
        case RootElement(TemplateRoot(_,_) :: Nil) => false
        case RootElement(_) => true
        case _ => false
      }).headOption map { case RootElement(content) => content } getOrElse Nil
    }
    
    def title = {
      if (config.hasPath("title")) 
        Text(docNumber.mkString("."), Styles("titleNumber")) +: List(Text(config.getString("title")))
      else (findRoot collect {
        case Header(_,content,Styles("title")) => content
      }).headOption getOrElse List(Text(""))
    }
    
    lazy val sections = {
      
      def extractSections (parentPos: List[Int], blocks: Seq[Block]): Seq[SectionInfo] = {
        val positions = Stream.from(1).iterator
        blocks collect {
          case Section(Header(_,header,Id(id)), content, _) => {
            val pos = positions.next :: parentPos 
            SectionInfo(pos.reverse, id, TitleInfo(header), extractSections(pos, content)) 
          }
        }
      }
      
      extractSections(Nil, findRoot)
    } 

    
    val isRewritten = rewriteRules.isEmpty
    
    def rewrite: Document = rewrite(Nil)
     
    def rewrite (customRule: PartialFunction[Element,Option[Element]]): Document = rewrite(List(customRule))
    
    def rewrite (customRules: Seq[PartialFunction[Element,Option[Element]]]): Document = rewrite(customRules, DocumentContext(this))
      
    private[Documents] def rewrite (customRules: Seq[PartialFunction[Element,Option[Element]]], context: DocumentContext): Document = {
      
      val resolvedRules = (defaultRules map { _(context) })      
      
      val allRules = RewriteRules chain (customRules ++ resolvedRules)
      
      val newRoot = content rewrite allRules
      
      val newFragments = TreeUtil.extractFragments(BlockSequence(fragments.values.toSeq).rewrite(allRules).content) 
      
      val newDoc = withRewrittenContent(newRoot, newFragments, context.autonumbering.number)
      
      context.template map (_.rewrite(context.withDocument(newDoc))) getOrElse newDoc
    }
    
    def withRewrittenContent (newContent: RootElement, fragments: Map[String,Block], docNumber: List[Int] = Nil): Document = new Document(path, newContent, fragments, config) {
      override lazy val defaultRules = Nil
      override val removeRules = this
    }

    def removeRules: Document = withRewrittenContent(content,fragments)
    
  }
  
  case class SectionInfo (position: List[Int], id: String, title: TitleInfo, content: Seq[SectionInfo]) extends Element with ElementContainer[SectionInfo,SectionInfo] {
    val level = position.length
  }
  
  case class TitleInfo (content: Seq[Span]) extends SpanContainer[TitleInfo] {
    lazy val text = TreeUtil.extractText(content)
  }
  
  class ReferenceResolver (root: Any, parent: Option[ReferenceResolver] = None) {
    
    /* These are all dynamic, non-typesafe lookups for values where often both,
     * the path from the template and the actual target value (e.g. from a config
     * file) originate from text resources, so the dynamic lookup is justifiable here
     * TODO - think about improvements for the error handling */
    def resolve (target: Any, path: List[String], root: Boolean = false): (Option[Any], List[String]) = {
      val result = target match {
        case m: Map[_, _] => (m.asInstanceOf[Map[Any,Any]].get(path.head), path.tail)
        case c: Config    => (Try{ c.getAnyRef(path.mkString(".")) }.toOption, Nil)
        case other        => (Try{ target.getClass.getMethod(path.head).invoke(target) }.toOption, path.tail)
      }
      result match {
        case (None, _) if (root && parent.isDefined) => parent.get.resolve(target, path, root)
        case (None, _)            => (None, Nil)
        case (Some(value), Nil)   => (Some(value), Nil)
        case (Some(value), path)  => resolve(value, path)
      }
    }
    
    def resolve (path: List[String]): Option[Any] = resolve(root, path, true)._1
    
  }

  class DocumentContext private (val document: Document, 
                                 val parent: DocumentTree, 
                                 val root: DocumentTree,
                                 val autonumbering: AutonumberContext,
                                 baseConfig: Option[Config] = None) { self =>
    
    lazy val parents = {
      @tailrec def collect (path: Path, acc: List[DocumentTree]): Seq[DocumentTree] = {
         val newAcc = root.selectSubtree(path) match {
           case Some(tree) => tree :: acc
           case None => acc
         }
         if (path.parent == path) newAcc
         else collect(path.parent, newAcc)
      }
      collect(parent.path, Nil).reverse
    }
    
    private def mergeTreeConfigs (config: Config) = ((config /: parents) { case (config, tree) =>
      tree.config.map(c => config.withFallback(c)).getOrElse(config)
    }).resolve
    
    lazy val config = {
      val base = baseConfig getOrElse (template map (t => document.config.withFallback(t.config)) getOrElse document.config)
      mergeTreeConfigs(base)
    }
      
    lazy val template = {
      val tempConf = mergeTreeConfigs(document.config)
      if (tempConf.hasPath("template")) {
        val value = tempConf.getValue("template")
        val desc = value.origin().description()
        val basePath = if (desc.startsWith("path:")) Path(desc.take(desc.lastIndexOf(":")).drop(5)) else Root
        val templatePath = Path(value.unwrapped().toString)
        val tree = root.selectSubtree(basePath / templatePath.parent)
        tree flatMap (_.templates.find(_.path.name == templatePath.name)) // TODO - error handling when template not found
      }
      else {
        val filename = "default.template.html" // TODO - should be configurable and suffix dependent on renderer
        parents collectFirst {
          case tree if tree.templates.exists(_.path.name == filename) => tree.templates.find(_.path.name == filename).get
        }
      }
    }
    
    protected lazy val resolver = new ReferenceResolver(Map[String,Any](
      "config" -> config,
      "document" -> document,
      "parent" -> parent,
      "root" -> root
    ))
    
    def resolveReference (path: String): Option[Any] = resolver.resolve(path.split("\\.").toList)
    
    def withReferenceContext (target: Any) = new DocumentContext(document, parent, root, autonumbering, baseConfig) {
      override lazy val parents = self.parents
      override lazy val config = self.config
      override lazy val template = self.template
      override protected lazy val resolver = new ReferenceResolver(target, Some(self.resolver))
    }
    
    def withDocument (newDoc: Document) = new DocumentContext(newDoc, parent, root, autonumbering, baseConfig)
    
  }
  
  case object DocumentContext {
    
    def apply (document: Document): DocumentContext = {
      val tree = new DocumentTree(Root, Seq(document))
      new DocumentContext(document, tree, tree, AutonumberContext.defaults)
    }
    
    def apply (document: Document, parent: DocumentTree, root: DocumentTree, autonumbering: AutonumberContext): DocumentContext 
      = new DocumentContext(document, parent, root, autonumbering)
    
    def apply (path: Path, parent: DocumentTree, root: DocumentTree, config: Config): DocumentContext 
      = new DocumentContext(new Document(path, RootElement(Nil)), parent, root, AutonumberContext.defaults, Some(config))
  }
  
  sealed abstract class DocumentType
  
  case object Config extends DocumentType
  case object Markup extends DocumentType
  case object Template extends DocumentType
  case object Dynamic extends DocumentType
  case object Static extends DocumentType
  case object Ignored extends DocumentType
  
  class DefaultDocumentTypeMatcher (markupSuffixes: Set[String], ignorePatterns: Seq[String]) extends (Path => DocumentType) {
    
    private def suffix (name: String) = name.lastIndexOf(".") match {
      case -1    => ""
      case index => name.drop(index+1)
    }  
    
    val IgnoredName = ignorePatterns.map(_.replaceAll("\\.","\\\\.").replaceAll("\\*",".*")).mkString("^","|","$").r
    
    val TemplateName = """.+\.template\.[^\.]+$""".r
    val DynamicName = """.+\.dynamic\.[^\.]+$""".r
    val ConfigName = """.+\.conf$""".r
    
    def apply (path: Path) = path.name match {
      case IgnoredName()  => Ignored
      case name if markupSuffixes(suffix(name)) => Markup
      case ConfigName()   => Config
      case TemplateName() => Template
      case DynamicName()  => Dynamic
      case _              => Static
    }
    
  }
  
  trait Navigatable {
    
    def path: Path
    
  }
  
  class DocumentTree (val path:Path, 
                      val documents: Seq[Document], 
                      private[tree] val templates: Seq[TemplateDocument] = Nil, 
                      private[tree] val dynamicTemplates: Seq[TemplateDocument] = Nil, 
                      val dynamicDocuments: Seq[Document] = Nil, 
                      val staticDocuments: Seq[Input] = Nil,
                      val subtrees: Seq[DocumentTree] = Nil, 
                      private[tree] val config: Option[Config] = None) extends Navigatable {
    
    val name = path.name
    
    lazy val navigatables = documents ++ subtrees // TODO - allow for sorting by config (default to sorting by name)
    
    private val documentsByName = documents map {doc => (doc.name, doc)} toMap // TODO - handle duplicates
    private val subtreesByName = subtrees map {tree => (tree.name, tree)} toMap

    def selectDocument (path: String): Option[Document] = selectDocument(Path(path))
    def selectDocument (path: Path): Option[Document] = path match {
      case Current / name => documentsByName.get(name)
      case path / name => selectSubtree(path) flatMap (_.selectDocument(name))
      case _ => None
    }
    
    def selectSubtree (path: String): Option[DocumentTree] = selectSubtree(Path(path))
    def selectSubtree (path: Path): Option[DocumentTree] = path match {
      case Current => Some(this)
      case Current / name => subtreesByName.get(name)
      case path / name => selectSubtree(path) flatMap (_.selectSubtree(name)) 
      case _ => None
    }
    
    private lazy val targets: Map[Selector, TargetResolver] = {
      val sub = (List[(Selector,TargetResolver)]() /: subtrees) { 
        case (list, tree) => tree.targets.toList ::: list
      }
      val all = (sub /: documents) { 
        case (list, doc) => doc.targets.toList ::: list
      }
      (all.groupBy (_._1) collect {
        case (selector, ((_,target) :: Nil)) => (selector, target)
        case (s @ UniqueSelector(name), conflicting) => (s, DuplicateTargetResolver(path, name))
      }).toMap
    }
    
    def selectTarget (selector: Selector) = targets.get(selector)
    
    def rewrite: DocumentTree = rewrite(Nil, AutonumberContext.defaults)
    
    def rewrite (autonumbering: AutonumberContext): DocumentTree = rewrite(Nil, autonumbering)
     
    def rewrite (customRule: DocumentContext => PartialFunction[Element,Option[Element]]): DocumentTree = 
      rewrite(List(customRule), AutonumberContext.defaults)
        
    def rewrite (customRule: DocumentContext => PartialFunction[Element,Option[Element]], 
        autonumbering: AutonumberContext): DocumentTree = rewrite(List(customRule), autonumbering)
    
    def rewrite (customRules: Seq[DocumentContext => PartialFunction[Element,Option[Element]]], 
        autonumbering: AutonumberContext): DocumentTree = rewrite(RewriteContext(this, customRules, autonumbering))
    
    private def rewrite (rewriteContext: RewriteContext): DocumentTree = {
      
      val autonumberConfig = config map (AutonumberConfig.fromConfig(_)) getOrElse rewriteContext.autonumbering.config
      
      def autonumberContextForChild (num: Int) =
          if (autonumberConfig.documents) AutonumberContext(autonumberConfig, rewriteContext.autonumbering.number :+ num)
          else                            AutonumberContext(autonumberConfig, Nil)
      
      def rewriteContextForChild (num: Int) = rewriteContext.copy(autonumbering = autonumberContextForChild(num))
        
      val (_, docs) = ((documents :\ (documents.length, List[Document]())) { case (doc, (num, acc)) =>
        val context = DocumentContext(doc, this, rewriteContext.root, autonumberContextForChild(num))
        (num - 1, doc.rewrite(rewriteContext.rules map (_(context)), context) :: acc) 
      })
      
      val dynamicDocs = dynamicTemplates map (doc => {
        val context = DocumentContext(doc.path, this, rewriteContext.root, doc.config)
        doc.rewrite(context)
      })
      
      val (_, trees) = ((subtrees :\ (subtrees.length + documents.length, List[DocumentTree]())) { case (subtree, (num, acc)) =>
        (num - 1, subtree.rewrite(rewriteContextForChild(num)) :: acc) 
      })
      
      new DocumentTree(path, docs, Nil, Nil, dynamicDocs, staticDocuments, trees)  
    }
  }
  
  case class RewriteContext (root: DocumentTree, rules: Seq[DocumentContext => PartialFunction[Element,Option[Element]]], autonumbering: AutonumberContext)
    
  
  sealed abstract class Path {
    def parent: Path
    def name: String
    def prefix: PathPrefix
    def components: List[String]
    def isAbsolute = prefix == Root
    def / (name: String) = new /(this, name)
    def / (path: Path): Path = path.prefix match {  
      case Root => path
      case Current => Path(prefix, components ::: path.components)
      case Parent(1) => parent / Path(Current, path.components)
      case Parent(i) => parent / Path(Parent(i-1), path.components)
    }
    def relativeTo (path: Path): Path = {
      def buildRelativePath = {
        def removeCommonParts (a: List[String], b: List[String]): (List[String],List[String]) = (a,b) match {
            case (p1 :: rest1, p2 :: rest2) if p1 == p2 => removeCommonParts(rest1,rest2)
            case _ => (a,b)
          }
          val (a,b) = removeCommonParts(path.components, components)
          val pref = a match {
            case Nil => Current
            case list => Parent(list.length)
          }
          Path(pref, b)
      }
      (isAbsolute, path.isAbsolute) match {
        case (true, true) | (false, false) => buildRelativePath
        case (true, false) => this // there is no other sensible result for this case
        case (false, true) => path / this
      }
    }
    def suffix = ""
    def basename = name
  }
 
  case class / (parent: Path, name: String) extends Path {
    lazy val components: List[String] = parent.components ++ List(name)
    lazy val prefix = parent.prefix
    override lazy val basename = if (name.contains('.')) name.take(name.lastIndexOf(".")) else name
    override lazy val suffix = if (name.contains('.')) name.drop(name.lastIndexOf(".")+1) else ""
    override lazy val toString = prefix + (components mkString "/")
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
      val trimmed = str.trim match {
        case "/" | "../" => str.trim 
        case other => other.stripSuffix("/")
      }
      val (parent, rest) = 
        if (trimmed.startsWith("/")) (Root, trimmed.drop(1))
        else if (trimmed.startsWith("../")) (Parent(1), trimmed.drop(3))
        else if (trimmed == "..") (Parent(1), "")
        else (Current, trimmed)
       if (rest.isEmpty) parent else apply(parent, rest.split("/").toList)
    }
  
    @tailrec def apply (parent: Path, rest: List[String]): Path = (parent, rest) match {
      case (Parent(level), ".." :: rest) => apply(Parent(level+1), rest)
      case (parent, rest) => rest.foldLeft(parent)(_ / _)
    } 
  }
  
  
  case class AutonumberConfig (documents: Boolean, sections: Boolean, maxDepth: Int)

  case class AutonumberContext (config: AutonumberConfig, number: List[Int] = Nil)
  
  object AutonumberConfig {
    
    def fromConfig (config: Config) = {
      if (config.hasPath("autonumbering")) {
        val nConf = config.getObject("autonumbering").toConfig
        val (documents, sections) = if (nConf.hasPath("scope")) nConf.getString("scope") match {
          case "documents" => (true,  false)
          case "sections"  => (false, true)
          case "all"       => (true,  true)
          case "none"      => (false, false)
          case _           => (false, false) // TODO - error handling
        } else                (false, false)
        val depth = if (nConf.hasPath("depth")) nConf.getInt("depth") else Int.MaxValue 
        AutonumberConfig(documents, sections, depth)
      }
      else defaults
    }
    
    def defaults = AutonumberConfig(false, false, 0)
  }
  
  object AutonumberContext {
    def defaults = AutonumberContext(AutonumberConfig.defaults)
  }
  
  
}