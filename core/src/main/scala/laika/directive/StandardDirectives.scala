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

package laika.directive

import Directives._
import laika.util.Builders._
import laika.tree.Elements._
import laika.tree.Templates._
import laika.tree.Documents._
import laika.tree.Templates.rewriteRules
import scala.collection.JavaConversions._
import laika.tree.TocGenerator

/** Provides the implementation for the standard directives included in Laika.
 *  
 *  These include:
 *  
 *  - `toc`: Generates a table of content from a specified root node.
 *  - `fragment`: Marks a block in a markup document as being separate from the main content, 
 *    so that it can be placed separately in templates.
 *  - `for`: Accesses a value from the context and sets it as the reference context for its
 *    body elements, executing the body if the referenced value is non-empty and executing
 *    it multiple times when it is a collection.
 *  - `if`: Accesses a value from the context and processes the body element only when
 *    it is a value recognized as true.
 *  
 *  @author Jens Halm
 */
trait StandardDirectives {

  
  /** Implementation of the `for` directive for templates.
   */
  lazy val templateFor = Templates.create("for") {
    import Templates.Combinators._
    import java.util.{Map => JMap, Collection => JCol}

    val emptyValues = Set("",false,null,None)
    
    (attribute(Default) ~ body(Default) ~ body("empty").optional ~ context) {
      (path, content, fallback, context) => {
        
        def rewriteContent (value: Any) =
          TemplateSpanSequence(content) rewrite rewriteRules(context.withReferenceContext(value))
        
        def rewriteFallback = 
          fallback map (TemplateSpanSequence(_) rewrite rewriteRules(context)) getOrElse (TemplateSpanSequence(Nil))

        context.resolveReference(path) match {
          case Some(m: Map[_,_])  => rewriteContent(m) 
          case Some(m: JMap[_,_]) => rewriteContent(m) 
          case Some(it: Iterable[_]) if it.isEmpty => rewriteFallback
          case Some(it: JCol[_])     if it.isEmpty => rewriteFallback
          case Some(it: Iterable[_]) => {
            val spans = for (value <- it) yield rewriteContent(value)
            TemplateSpanSequence(spans.toSeq)
          }
          case Some(it: JCol[_]) => {
            val spans = for (value <- iterableAsScalaIterable(it)) yield rewriteContent(value)
            TemplateSpanSequence(spans.toSeq)
          }
          case Some(value) if emptyValues(value) => rewriteFallback
          case Some(value)            => rewriteContent(value)
          case None                   => TemplateSpanSequence(Nil)
        }
      }
    }
  }
  
  /** Implementation of the `if` directive for templates.
   */
  lazy val templateIf = Templates.create("if") {
    import Templates.Combinators._
    import java.util.{Map => JMap, Collection => JCol}
    
    val trueStrings = Set("true","yes","on","enabled")

    (attribute(Default) ~ body(Default) ~ body("else").optional ~ context) {
      (path, content, fallback, context) => {
        
        def rewriteContent =
          TemplateSpanSequence(content) rewrite rewriteRules(context)
        
        def rewriteFallback = 
          fallback map (TemplateSpanSequence(_) rewrite rewriteRules(context)) getOrElse (TemplateSpanSequence(Nil))
        
        context.resolveReference(path) match {
          case Some(true) => rewriteContent
          case Some(s: String) if trueStrings(s) => rewriteContent
          case _ => rewriteFallback
        }
      }
    }
  }
  
  /** Creates the nodes for a table of content.
   *  
   *  @param depth the maximum depth to traverse when building the table, the depth is unlimited if the value is empty
   *  @param rootConfig the string identifier that specifies the tree that should serve as the root for the table
   *  @param title the title for the table
   *  @param context the context of the document the table of content will be placed in
   *  @return a block element containing the table and its title
   */
  def toc (depth: Option[Int], rootConfig: String, title: Option[String], context: DocumentContext): Block = {
    
    val maxLevel = depth getOrElse Int.MaxValue
    
    val root = rootConfig match {
      case "#rootTree"        => context.root
      case "#currentTree"     => context.parent
      case "#currentDocument" => context.document
      case pathString => {
        val configPath = Path(pathString)
        val path = 
          (if (configPath.isAbsolute) configPath
          else (context.parent.path / configPath)).relativeTo(context.root.path) 
        context.root.selectDocument(path).getOrElse(context.root.selectSubtree(path).getOrElse(context.root))
      }
    }
    
    val list = root match {
      case doc: Document      => TocGenerator.fromDocument(doc, maxLevel, context.document.path)
      case tree: DocumentTree => TocGenerator.fromTree(tree, maxLevel, context.document.path)
    }
    title match {
      case Some(text) => TitledBlock(List(Text(text)), list, Styles("toc"))
      case None       => BlockSequence(list, Styles("toc"))
    }
  }
  
  /** Implementation of the `toc` directive for templates.
   */
  lazy val templateToc = Templates.create("toc") {
    import Templates.Combinators._
    import Templates.Converters._
    
    (attribute("depth", positiveInt).optional ~ 
        attribute("root").optional ~ 
        attribute("title").optional ~ 
        context) {  
      (depth, rootConfig, title, context) =>
        TemplateElement(toc(depth, rootConfig.getOrElse("#rootTree"), title, context))
    }
  }
  
  /** Implementation of the `toc` directive for block elements in markup documents.
   */
  lazy val blockToc = Blocks.create("toc") {
    import Blocks.Combinators._
    import Blocks.Converters._
    
    (attribute("depth", positiveInt).optional ~ 
        attribute("root").optional ~ 
        attribute("title").optional ~ 
        context) {  
      (depth, rootConfig, title, context) =>
        toc(depth, rootConfig.getOrElse("#currentDocument"), title, context)
    }
  }
  
  
  /** Implementation of the `fragment` directive for block elements in markup documents.
   */
  lazy val blockFragment = Blocks.create("fragment") {
    import Blocks.Combinators._
    
    (attribute(Default) ~ body(Default)) {
      (name, content) => DocumentFragment(name, BlockSequence(content, Styles(name)))
    }
  }
  
  /** Implementation of the `fragment` directive for templates.
   */
  lazy val templateFragment = Templates.create("fragment") {
    import Templates.Combinators._
    
    (attribute(Default) ~ body(Default)) {
      (name, content) => TemplateElement(DocumentFragment(name, TemplateSpanSequence(content)))
    }
  }
  
  /** The complete list of standard directives for block
   *  elements in markup documents.
   */
  lazy val stdBlockDirectives = List(
    blockToc,
    blockFragment
  )

  /** The complete list of standard directives for templates.
   */
  lazy val stdTemplateDirectives = List(
    templateToc,
    templateFor,
    templateIf
  )
  
}