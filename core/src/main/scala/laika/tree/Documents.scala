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
import laika.tree.Templates.TemplateRoot
import laika.tree.Templates.TemplateContextReference
import scala.util.Try
import laika.parse.css.Styles.StyleDeclarationSet

/** Provides the API for Documents and DocumentTrees as well as the Path API.
 *  
 *  @author Jens Halm
 */
object Documents {
  
  val defaultTemplate = new TemplateDocument(Root / "default.template", TemplateRoot(List(TemplateContextReference("document.content"))))
  
  /** Represents a single document and provides access
   *  to the document content and structure as well
   *  as hooks for triggering rewrite operations.
   *  
   *  @param path the full, absolute path of this document in the (virtual) document tree
   *  @param content the tree model obtained from parsing the markup document
   *  @param fragments separate named fragments that had been extracted from the content
   *  @param config the configuration for this document
   *  @param docNumber the number of this document inside a document tree hierarchy, expressed as a list of Ints
   *  @param rewriteRules a list of rewrite rules that have not yet been applied to the document 
   */
  class Document (val path: Path, 
                  val content: RootElement, 
                  val fragments: Map[String, Element] = Map.empty,
                  val config: Config = ConfigFactory.empty,
                  docNumber: List[Int] = Nil,
                  rewriteRules: Seq[DocumentContext => RewriteRule] = Nil) extends Navigatable {
    
    private lazy val linkResolver = LinkResolver(path,content)
    
    protected lazy val defaultRules = rewriteRules :+ (linkResolver.rewriteRules(_)) :+ (SectionBuilder(_))
    
    private[Documents] lazy val targets = linkResolver.globalTargets ++ (linkResolver.globalTargets collect {
      case (UniqueSelector(name), target) => (PathSelector(path, name), target)
    })
    
    private def findRoot = {
      (content select {
        case RootElement(TemplateRoot(_,_) :: Nil) => false
        case RootElement(_) => true
        case _ => false
      }).headOption map { case RootElement(content) => content } getOrElse Nil
    }
    
    /** The title of this document, obtained from the document
     *  structure or from the configuration.
     */
    def title = {
      if (config.hasPath("title")) docNumber match {
        case Nil => List(Text(config.getString("title")))
        case _ => Text(docNumber.mkString("","."," "), Styles("titleNumber")) +: List(Text(config.getString("title")))
      }
      else (findRoot collect {
        case Title(content,_) => content
      }).headOption getOrElse List(Text(""))
    }

    /** The section structure of this document based on the hierarchy
     *  of headers found in the original text markup.
     */
    lazy val sections = {
      
      def extractSections (parentPos: List[Int], blocks: Seq[Block]): Seq[SectionInfo] = {
        val positions = Stream.from(1).iterator
        blocks collect {
          case Section(Header(_,header,Id(id)), content, _) => {
            val pos = parentPos :+ positions.next 
            SectionInfo(pos, id, TitleInfo(header), extractSections(pos, content)) 
          }
        }
      }
      extractSections(docNumber, findRoot)
    } 

    /** Indicates whether all rewrite rules have already been applied
     *  to this document.
     */
    val isRewritten = defaultRules.isEmpty

    /** Returns a new, rewritten document model based on the default rewrite rules.
     */
    def rewrite: Document = rewriteWith(Nil)
     
    /** Returns a new, rewritten document model based on the specified rewrite rules.
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
    def rewrite (customRule: RewriteRule): Document = rewriteWith(List(customRule))

    /** Returns a new, rewritten document model based on the specified rewrite rules.
     *  
     *  If none of the specified partial functions is defined for a specific element the old element remains
     *  in the tree unchanged. If one of them returns `None` then the node gets removed from the tree, 
     *  if one of them returns an element it will replace the old one. Of course the function may
     *  also return the old element.
     *  
     *  The rewriting is performed in a way that only branches of the tree that contain
     *  new or removed elements will be replaced. It is processed bottom-up, therefore
     *  any element container passed to the rule only contains children which have already
     *  been processed. 
     */
    def rewriteWith (customRules: Seq[RewriteRule]): Document = {
      val context = DocumentContext(this)
      rewriteDocument(customRules, context)
    }
      
    private[Documents] def rewriteDocument (customRules: Seq[RewriteRule], context: DocumentContext): Document = {
      
      val resolvedRules = (defaultRules map { _(context) })      
      
      val allRules = RewriteRules chain (customRules ++ resolvedRules)
      
      val newRoot = content rewrite allRules
       
      val newFragments = fragments mapValues {
        case et: ElementTraversal[_] => (et rewrite allRules).asInstanceOf[Block]
        case block => block
      }
      
      withRewrittenContent(newRoot, newFragments, context.autonumbering.number)
    }

    /** Applies the template for the specified context to this document.
     */
    def applyTemplate (context: DocumentContext): Document =
      context.template.getOrElse(defaultTemplate).rewrite(context)
    
    private[tree] def withRewrittenContent (newContent: RootElement, fragments: Map[String,Element], docNumber: List[Int] = docNumber): Document = new Document(path, newContent, fragments, config, docNumber) {
      override lazy val defaultRules = Nil
      override val removeRules = this
    }

    /** Removes all rewrite rules that have not yet been applied
     *  from this document.
     */
    def removeRules: Document = withRewrittenContent(content,fragments)
    
  }
  
  /** Captures information about a document section, without its content.
   */
  case class SectionInfo (position: List[Int], id: String, title: TitleInfo, content: Seq[SectionInfo]) extends Element with ElementContainer[SectionInfo,SectionInfo] {
    val level = position.length
  }

  /** Represents a section title.
   */
  case class TitleInfo (content: Seq[Span]) extends SpanContainer[TitleInfo] {
    lazy val text = TreeUtil.extractText(content)
  }

  /** A resolver for context references in templates or markup documents.
   */
  class ReferenceResolver (root: Any, parent: Option[ReferenceResolver] = None) {
    import java.util.{Map => JMap}
    def fromJavaMap (m: JMap[Any,Any], key: Any) = if (m.containsKey(key)) Some(m.get(key)) else None
    /* These are all dynamic, non-typesafe lookups for values where often both,
     * the path from the template and the actual target value (e.g. from a config
     * file) originate from text resources, so the dynamic lookup is justifiable here */
    def resolve (target: Any, path: List[String], root: Boolean = false): (Option[Any], List[String]) = {
      val result = target match {
        case m: JMap[_, _]=> (fromJavaMap(m.asInstanceOf[JMap[Any,Any]], path.head), path.tail)
        case m: Map[_, _] => (m.asInstanceOf[Map[Any,Any]].get(path.head), path.tail)
        case c: Config    => (Try { c.getAnyRef(path.mkString(".")) } toOption, Nil)
        case d: Document if path.head == "title" => (Some(SpanSequence(d.title)), path.tail)  
        case other        => (Try { target.getClass.getMethod(path.head).invoke(target) } toOption, path.tail)
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

  /** Represents a single document,its parent and root directories,
   *  its asssociated template and other context information that
   *  can not be obtained from the Document instance itself.
   *  
   *  @param document the document this context refers to
   *  @param parent the parent document tree of the referred document
   *  @param root the root document tree of the referred document
   *  @param autonumbering the context for autonumbering of document and sections
   *  @param baseConfig the configuation of the referred document without the fallbacks applied
   */
  class DocumentContext private (val document: Document, 
                                 val parent: DocumentTree, 
                                 val root: DocumentTree,
                                 val autonumbering: AutonumberContext,
                                 baseConfig: Option[Config] = None,
                                 format: Option[String] = None) { self =>
    
    protected lazy val parents = {
      @tailrec def collect (path: Path, acc: List[DocumentTree]): Seq[DocumentTree] = {
         val newAcc = root.selectSubtree(path.relativeTo(root.path)) match {
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
    
    /** The configuration for this document merged with
     *  the configurations for its parent trees.
     */
    lazy val config = {
      val base = baseConfig getOrElse (template map (t => document.config.withFallback(t.config)) getOrElse document.config)
      mergeTreeConfigs(base)
    }

    /** The (optional) template to use when rendering this document.
     */
    lazy val template = format flatMap (templateForFormat(_))
    
    private def templateForFormat (format: String) = {
      val tempConf = mergeTreeConfigs(document.config)
      if (tempConf.hasPath("template") || tempConf.hasPath(format + ".template")) {
        val key = if (tempConf.hasPath(format + ".template")) format+".template" else "template" 
        val value = tempConf.getValue(key)
        val desc = value.origin().description()
        val basePath = if (desc.startsWith("path:")) Path(desc.take(desc.lastIndexOf(":")).drop(5)).parent else Root
        val templatePath = (basePath / Path(value.unwrapped().toString)).relativeTo(Root)
        root.selectTemplate(templatePath).orElse(throw new IllegalStateException(s"Template not found: $templatePath"))
      }
      else {
        val filename = "default.template." + format // TODO - should be configurable
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
    def withReferenceContext (target: Any) = new DocumentContext(document, parent, root, autonumbering, baseConfig, format) {
      override lazy val parents = self.parents
      override lazy val config = self.config
      override lazy val template = self.template
      override protected lazy val resolver = new ReferenceResolver(target, Some(self.resolver))
    }
    
    /** Creates a copy of this context for the specified document
     *  while keeping all the other information.
     */
    def withDocument (newDoc: Document) = new DocumentContext(newDoc, parent, root, autonumbering, baseConfig, format)
    
  }

  object DocumentContext {
    
    def apply (document: Document): DocumentContext = {
      val tree = new DocumentTree(Root, Seq(document))
      new DocumentContext(document, tree, tree, AutonumberContext.defaults)
    }
    
    def apply (document: Document, parent: DocumentTree, root: DocumentTree): DocumentContext 
      = new DocumentContext(document, parent, root, AutonumberContext.defaults)
    
    def apply (document: Document, parent: DocumentTree, root: DocumentTree, format: String): DocumentContext 
      = new DocumentContext(document, parent, root, AutonumberContext.defaults, format = Some(format))
    
    def apply (document: Document, parent: DocumentTree, root: DocumentTree, autonumbering: AutonumberContext): DocumentContext 
      = new DocumentContext(document, parent, root, autonumbering)
    
    /** Creates a context with an empty document for rewriting a dynamic document.
     */
    def apply (path: Path, parent: DocumentTree, root: DocumentTree, config: Config, format: String): DocumentContext 
      = new DocumentContext(new Document(path, RootElement(Nil)), parent, root, AutonumberContext.defaults, Some(config), Some(format))
  }

  /** Base type for all document type descriptors.
   */
  sealed abstract class DocumentType
  
  /** A configuation document in the syntax
   *  supported by the Typesafe Config library.
   */
  case object Config extends DocumentType
  
  /** A text markup document produced by a parser.
   */
  case object Markup extends DocumentType
  
  /** A template document that might get applied
   *  to a document when it gets rendered.
   */
  case object Template extends DocumentType

  /** A dynamic document that might contain
   *  custom directives that need to get
   *  processed before rendering.
   */
  case object Dynamic extends DocumentType

  /** A style sheet that needs to get passed
   *  to a renderer.
   */
  case class StyleSheet (format: String) extends DocumentType
  
  /** A static file that needs to get copied
   *  over to the output target.
   */
  case object Static extends DocumentType
  
  /** A document that should be ignored and neither
   *  get processed nor copied.
   */
  case object Ignored extends DocumentType
  
  
  /** The default implementation for determining the document type
   *  of the input based on its path.
   */
  class DefaultDocumentTypeMatcher (markupSuffixes: Set[String]) extends (Path => DocumentType) {
    
    private def suffix (name: String) = name.lastIndexOf(".") match {
      case -1    => ""
      case index => name.drop(index+1)
    }  
    
    val TemplateName = """.+\.template\.[^\.]+$""".r
    val DynamicName = """.+\.dynamic\.[^\.]+$""".r
    val StylesheetName = """.+\.fo.css$""".r // stylesheets for HTML are treated as static documents
    val ConfigName = """.+\.conf$""".r
    
    def apply (path: Path) = path.name match {
      case name if markupSuffixes(suffix(name)) => Markup
      case ConfigName()     => Config
      case TemplateName()   => Template
      case DynamicName()    => Dynamic
      case StylesheetName() => StyleSheet("fo")
      case _                => Static
    }
    
  }
  
  /** A navigatable object is anything that has an associated path.
   */
  trait Navigatable {
    
    def path: Path
    
    /** The local name of this navigatable.
     */
    lazy val name = path.name
    
  }

  
  /** Represents a tree with all its documents and subtrees.
   *  
   *  @param path the full, absolute path of this (virtual) document tree
   *  @param documents all markup documents on this level of the tree hierarchy
   *  @param templates all templates on this level of the tree hierarchy that might get applied to a document when it gets rendered
   *  @param dynamicTemplates all dynamic documents that need to get processed before rendering on this level of the tree hierarchy
   *  @param dynamicDocuments all documents that were created by processing a dynamic template
   *  @param staticDocuments all static documents that need to be copied to the output
   *  @param config the configuration associated with this tree
   *  @param docNumber the number of this tree inside a document tree hierarchy, expressed as a list of Ints
   *  @param navigationOrder the markup documents and subtrees merged into a specific order, used for generating tables of contexts
   *  @param sourcePaths the paths this document tree has been built from or an empty list if this tree does not originate from the file system
   *  and autonumbering
   */
  class DocumentTree (val path:Path, 
                      val documents: Seq[Document], 
                      private[tree] val templates: Seq[TemplateDocument] = Nil, 
                      private[tree] val dynamicTemplates: Seq[TemplateDocument] = Nil, 
                      val dynamicDocuments: Seq[Document] = Nil, 
                      val styles: Map[String,StyleDeclarationSet] = Map.empty.withDefaultValue(StyleDeclarationSet.empty),
                      val staticDocuments: Seq[Input] = Nil,
                      val subtrees: Seq[DocumentTree] = Nil, 
                      private[laika] val config: Option[Config] = None,
                      docNumber: List[Int] = Nil,
                      navigationOrder: Option[Seq[Navigatable]] = None,
                      val sourcePaths: Seq[String] = Nil) extends Navigatable {
    
    /** The title of this tree, obtained from configuration.
     */
    lazy val title: Seq[Span] = {
      config map (c => if (c.hasPath("title")) docNumber match {
        case Nil => List(Text(c.getString("title")))
        case _ => Text(docNumber.mkString("","."," "), Styles("titleNumber")) +: List(Text(c.getString("title")))
      } else Nil) getOrElse Nil 
    }
    
    /** Markup documents and subtrees in a specific order based on the
     *  configuration for this tree. Before rewriting the configuration
     *  is not fully accessible yet and this list will be empty.
     */
    lazy val navigatables = navigationOrder getOrElse (Nil)
    
    private def toMap [T <: Navigatable] (navigatables: Seq[T]): Map[String,T] = {
      navigatables groupBy (_.name) mapValues {
        case Seq(nav) => nav
        case multiple => throw new IllegalStateException("Multiple navigatables with the name " +
            s"${multiple.head.name} in tree $path")
      }
    }
    
    private val documentsByName = toMap(documents)
    private val templatesByName = toMap(templates)
    private val subtreesByName = toMap(subtrees)

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
    
    /** Selects a link target by the specified selector
     *  if it is defined somewhere in a document inside this document tree.
     */
    def selectTarget (selector: Selector) = targets.get(selector)
    
    /** Creates a new tree with the specified template added to its existing templates.
     */
    def withTemplate (template: TemplateDocument) = 
      new DocumentTree(path, documents, template +: templates, dynamicTemplates, dynamicDocuments, styles, 
          staticDocuments, subtrees, config, docNumber, navigationOrder, sourcePaths)  
    
    /** Creates a new tree with all templates removed from the hierarchy.
     */
    def withoutTemplates: DocumentTree = {
      // TODO - this method becomes obsolete after the redesign for 0.7
      val newSubtrees = subtrees map (_.withoutTemplates)
      val newSubtreesByName = toMap(newSubtrees)
      val newNavigatables = navigationOrder map { _ map {
        case t: DocumentTree => newSubtreesByName(t.name)
        case d: Document => d
      }}
      new DocumentTree(path, documents, Seq(), dynamicTemplates, dynamicDocuments, styles, 
          staticDocuments, newSubtrees, config, docNumber, newNavigatables, sourcePaths)  
    }
    
    /** Creates a new tree with the specified document prepended to its existing documents.
     */
    def prependDocument (doc: Document) = // TODO - remove this method in the 0.7 redesign
      new DocumentTree(path, doc +: documents, templates, dynamicTemplates, dynamicDocuments, styles, 
          staticDocuments, subtrees, config, docNumber, navigationOrder map (doc +: _), sourcePaths)  
    
    /** Creates a new tree mapping all subtrees with the specified function.
     */
    def mapSubtrees (f: DocumentTree => DocumentTree) = {// TODO - remove this method in the 0.7 redesign
      val newSubtrees = subtrees map f
      val newSubtreesByName = toMap(newSubtrees)
      val newNavigatables = navigationOrder map { _ map {
        case t: DocumentTree => newSubtreesByName(t.name)
        case d: Document => d
      }}
      new DocumentTree(path, documents, templates, dynamicTemplates, dynamicDocuments, styles, 
          staticDocuments, newSubtrees, config, docNumber, newNavigatables, sourcePaths)  
    }
      
    /** Returns a new tree, with all the document models contained in it 
     *  rewritten based on the default rewrite rules.
     */
    def rewrite: DocumentTree = rewrite(Nil, AutonumberContext.defaults)
    
    /** Returns a new tree, with all the document models contained in it 
     *  rewritten based on the default rewrite rules and the specified context for autonumbering.
     */
    def rewrite (autonumbering: AutonumberContext): DocumentTree = rewrite(Nil, autonumbering)
     
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
    def rewrite (customRule: DocumentContext => RewriteRule): DocumentTree = 
      rewrite(List(customRule), AutonumberContext.defaults)
        
    /** Returns a new tree, with all the document models contained in it 
     *  rewritten based on the specified rewrite rule and autonumbering context.
     */
    def rewrite (customRule: DocumentContext => RewriteRule, 
        autonumbering: AutonumberContext): DocumentTree = rewrite(List(customRule), autonumbering)
    
    /** Returns a new tree, with all the document models contained in it 
     *  rewritten based on the specified rewrite rules and autonumbering context.
     */
    def rewrite (customRules: Seq[DocumentContext => RewriteRule], 
        autonumbering: AutonumberContext): DocumentTree = {
      rewriteDocuments(RewriteContext(this, customRules, autonumbering))
    }
    
    private def rewriteDocuments (rewriteContext: RewriteContext): DocumentTree = {
      
      val autonumberConfig = config map (AutonumberConfig.fromConfig(_)) getOrElse rewriteContext.autonumbering.config
      
      def autonumberContextForChild (num: Int) =
          if (autonumberConfig.documents) AutonumberContext(autonumberConfig, rewriteContext.autonumbering.number :+ num)
          else                            AutonumberContext(autonumberConfig, Nil)
      
      def rewriteContextForChild (num: Int) = rewriteContext.copy(autonumbering = autonumberContextForChild(num))
        
      val sortedNavigatables = (config flatMap (NavigationOrder.fromConfig(_)) match {
        case Some(f) => (documents ++ subtrees).sortBy(f)
        case None    => documents.sortBy(NavigationOrder.defaults) ++ subtrees.sortBy(NavigationOrder.defaults)
      }).zipWithIndex.map { case (nav,num) => (nav,num+1) }
      
      val sortedDocuments = sortedNavigatables collect { case (d: Document, num) => (d,num) }
      val sortedSubtrees =  sortedNavigatables collect { case (t: DocumentTree, num) => (t,num) }
      
      val rewrittenDocuments = for ((doc,num) <- sortedDocuments) yield {
        val context = DocumentContext(doc, this, rewriteContext.root, autonumberContextForChild(num))
        (doc.rewriteDocument(rewriteContext.rules map (_(context)), context), num) 
      }

      val rewrittenSubtrees = for ((tree,num) <- sortedSubtrees) yield {
        (tree.rewriteDocuments(rewriteContextForChild(num)), num)
      }
      
      val rewrittenNavigatables = (rewrittenDocuments ++ rewrittenSubtrees).sortBy(_._2).map(_._1)
      
      new DocumentTree(path, rewrittenDocuments.map(_._1), templates, dynamicTemplates, dynamicDocuments, styles, staticDocuments,
          rewrittenSubtrees.map(_._1), config, rewriteContext.autonumbering.number, Some(rewrittenNavigatables), sourcePaths)  
    }
    
    /** Applies the templates for the specified output format to all documents within this tree.
     */
    def applyTemplates (format: String): DocumentTree = applyTemplates(format, this)
    
    private def applyTemplates (format: String, root: DocumentTree): DocumentTree = {
      
      val newDocs = for (doc <- documents) yield doc.applyTemplate(DocumentContext(doc, this, root, format))
      
      val newDynamicDocs = for (doc <- dynamicTemplates) yield 
          doc.rewrite(DocumentContext(doc.path.parent / doc.path.name.replace(".dynamic.", "."), this, root, doc.config, format))
      
      val newSubtrees = for (tree <- subtrees) yield tree.applyTemplates(format, root)
      
      new DocumentTree(path, newDocs, Nil, Nil, dynamicDocuments ++ newDynamicDocs, styles, staticDocuments, newSubtrees, 
          docNumber = docNumber, navigationOrder = navigationOrder, sourcePaths = sourcePaths)  
    }
  }
  
  
  private[Documents] case class RewriteContext (root: DocumentTree, rules: Seq[DocumentContext => RewriteRule], autonumbering: AutonumberContext)
    
  
  /** Represents a path inside a virtual tree of 
   *  documents.
   */
  sealed abstract class Path {
    
    /** The parent of this path.
     *  Will return this if this path represents a root node.
     */
    def parent: Path
    
    /** The local name of this path.
     */
    def name: String
    
    /** The first element of this path.
     */
    def prefix: PathPrefix
    
    /** All components after the prefix as a list of strings. 
     */
    def components: List[String]
    
    def isAbsolute = prefix == Root
    
    /** Creates a new path with the specified name
     *  as an immediate child of this path.
     */
    def / (name: String) = new /(this, name)
    
    /** Combines this path with the specified path.
     *  If the specified path is a relative path it
     *  will be interpreted relative to this path, if it is absolute
     *  it will be returned unchanged.
     */
    def / (path: Path): Path = path.prefix match {  
      case Root => path
      case Current => Path(prefix, components ::: path.components)
      case Parent(1) => parent / Path(Current, path.components)
      case Parent(i) => parent / Path(Parent(i-1), path.components)
    }
    
    /** Interprets this path relative to some other path.
     */
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
  
  /** The root of an absolute path.
   */
  case object Root extends PathPrefix ("/")

  /** The root of a relative path.
   */
  case object Current extends PathPrefix ("")

  /** The prefix of a relative path pointing
   *  to a parent the specified number of levels 
   *  above this path.
   */
  case class Parent (levels: Int) extends PathPrefix("../" * levels) {
    require(levels > 0)
  }

  /** Factory methods for creating path instances.
   */
  object Path {
    
    /** Creates path from interpreting the specified string representation.
     *  If it starts with `/` it will be interpreted as an absolute path,
     *  if it starts with `../` as a relative path pointing to some parent
     *  path. Otherwise it will be interpreted as a relative path.
     */
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
  
    /** Creates a path from the specified prefix element and 
     *  string components which each of them representing one path element. 
     */
    @tailrec def apply (prefix: Path, components: List[String]): Path = (prefix, components) match {
      case (Parent(level), ".." :: rest) => apply(Parent(level+1), rest)
      case (parent, rest) => rest.foldLeft(parent)(_ / _)
    } 
  }
  
  
  /** Configuration for autonumbering of documents and sections.
   */
  case class AutonumberConfig (documents: Boolean, sections: Boolean, maxDepth: Int)
  
  case class ConfigurationException (msg: String) extends RuntimeException(msg)

  object AutonumberConfig {
    
    /** Tries to obtain the autonumbering configuration
     *  from the specified configuration instance or returns
     *  the default configuration if not found.
     */
    def fromConfig (config: Config) = {
      if (config.hasPath("autonumbering")) {
        val nConf = config.getObject("autonumbering").toConfig
        val (documents, sections) = if (nConf.hasPath("scope")) nConf.getString("scope") match {
          case "documents" => (true,  false)
          case "sections"  => (false, true)
          case "all"       => (true,  true)
          case "none"      => (false, false)
          case other       => throw new ConfigurationException("Unsupported value for key 'autonumbering.scope': "+other)
        } else                (false, false)
        val depth = if (nConf.hasPath("depth")) nConf.getInt("depth") else Int.MaxValue 
        AutonumberConfig(documents, sections, depth)
      }
      else defaults
    }
    
    /** The defaults for autonumbering with section
     *  and document numbering both switched off. 
     */
    def defaults = AutonumberConfig(false, false, 0)
  }
  
  /** Context for autonumbering of documents and sections, containing the current
   *  number and the general configuration for autonumbering.
   */
  case class AutonumberContext (config: AutonumberConfig, number: List[Int] = Nil)
  
  object AutonumberContext {
    def defaults = AutonumberContext(AutonumberConfig.defaults)
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