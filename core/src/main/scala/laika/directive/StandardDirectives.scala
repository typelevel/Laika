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

package laika.directive

import laika.directive.Directives._
import laika.rewrite.TemplateRewriter.rewriteRules
import laika.rewrite.{DocumentCursor, TocGenerator, TreeUtil}
import laika.tree.Documents._
import laika.tree.Elements._
import laika.tree.Paths.Path
import laika.tree.Templates._
import laika.util.Builders._

import scala.collection.JavaConverters._

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
 *  - `format`: Process the body element only when the output format matches the format
 *    specified in the directive (e.g. `pdf` or `html`).
 *  - `style`: Adds a style property to the body element.
 *  - `fragment`: Adds the body as a fragment to the target document, separate from the main
 *    content, to be rendered in different locations of the output, like headers, footers or sidebars.
 *  - `pageBreak`: Inserts a page break element into the tree (will only be rendered by page-based
 *    output, like XSL-FO or PDF.
 *  
 *  @author Jens Halm
 */
object StandardDirectives {

  
  /** Implementation of the `for` directive for templates.
   */
  lazy val templateFor: Templates.Directive = Templates.create("for") {
    import java.util.{Collection => JCol, Map => JMap}

    import Templates.Combinators._

    val emptyValues = Set("",false,null,None)
    
    (attribute(Default) ~ body(Default) ~ body("empty").optional ~ cursor) {
      (path, content, fallback, cursor) => {
        
        def rewriteContent (value: Any) =
          TemplateSpanSequence(content) rewrite rewriteRules(cursor.withReferenceContext(value))
        
        def rewriteFallback = 
          fallback map (TemplateSpanSequence(_) rewrite rewriteRules(cursor)) getOrElse TemplateSpanSequence(Nil)

        cursor.resolveReference(path) match {
          case Some(m: Map[_,_])  => rewriteContent(m) 
          case Some(m: JMap[_,_]) => rewriteContent(m) 
          case Some(it: Iterable[_]) if it.isEmpty => rewriteFallback
          case Some(it: JCol[_])     if it.isEmpty => rewriteFallback
          case Some(it: Iterable[_]) =>
            val spans = for (value <- it) yield rewriteContent(value)
            TemplateSpanSequence(spans.toSeq)
          case Some(it: JCol[_]) =>
            val spans = for (value <- it.asScala) yield rewriteContent(value)
            TemplateSpanSequence(spans.toSeq)
          case Some(value) if emptyValues(value) => rewriteFallback
          case Some(value)            => rewriteContent(value)
          case None                   => TemplateSpanSequence(Nil)
        }
      }
    }
  }
  
  /** Implementation of the `if` directive for templates.
   */
  lazy val templateIf: Templates.Directive = Templates.create("if") {

    import Templates.Combinators._
    
    val trueStrings = Set("true","yes","on","enabled")

    (attribute(Default) ~ body(Default) ~ body("else").optional ~ cursor) {
      (path, content, fallback, cursor) => {
        
        def rewriteContent =
          TemplateSpanSequence(content) rewrite rewriteRules(cursor)
        
        def rewriteFallback = 
          fallback map (TemplateSpanSequence(_) rewrite rewriteRules(cursor)) getOrElse TemplateSpanSequence(Nil)
        
        cursor.resolveReference(path) match {
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
   *  @param cursor the cursor of the document the table of content will be placed in
   *  @return a block element containing the table and its title
   */
  def toc (depth: Option[Int], rootConfig: String, title: Option[String], cursor: DocumentCursor): Block = {
    
    val maxLevel = depth getOrElse Int.MaxValue
    
    val root: TreeContent = rootConfig match {
      case "#rootTree"        => cursor.root.target
      case "#currentTree"     => cursor.parent.target
      case "#currentDocument" => cursor.target
      case pathString =>
        val configPath = Path(pathString)
        val root = cursor.root.target
        val path =
          (if (configPath.isAbsolute) configPath
          else cursor.parent.target.path / configPath).relativeTo(root.path)
        root.selectDocument(path).getOrElse(root.selectSubtree(path).getOrElse(cursor.root.target))
    }
    
    val list = root match {
      case doc: Document      => TocGenerator.fromDocument(doc, maxLevel, cursor.target.path)
      case tree: DocumentTree => TocGenerator.fromTree(tree, maxLevel, cursor.target.path)
    }
    title match {
      case Some(text) => TitledBlock(List(Text(text)), list, Styles("toc"))
      case None       => BlockSequence(list, Styles("toc"))
    }
  }
  
  /** Implementation of the `toc` directive for templates.
   */
  lazy val templateToc: Templates.Directive  = Templates.create("toc") {
    import Templates.Combinators._
    import Templates.Converters._
    
    (attribute("depth", positiveInt).optional ~ 
        attribute("root").optional ~ 
        attribute("title").optional ~ 
        cursor) {  
      (depth, rootConfig, title, cursor) =>
        TemplateElement(toc(depth, rootConfig.getOrElse("#rootTree"), title, cursor))
    }
  }
  
  /** Implementation of the `toc` directive for block elements in markup documents.
   */
  lazy val blockToc: Blocks.Directive  = Blocks.create("toc") {
    import Blocks.Combinators._
    import Blocks.Converters._
    
    (attribute("depth", positiveInt).optional ~ 
        attribute("root").optional ~ 
        attribute("title").optional ~ 
        cursor) {  
      (depth, rootConfig, title, cursor) =>
        toc(depth, rootConfig.getOrElse("#currentDocument"), title, cursor)
    }
  }
  
  
  private def asBlock (blocks: Seq[Block], options: Options = NoOpt): Block = blocks match {
    case block :: Nil => TreeUtil.modifyOptions(block, _ + options)
    case blocks => BlockSequence(blocks, options)
  }
  
  private def asSpan (spans: Seq[Span], options: Options = NoOpt): Span = spans match {
    case span :: Nil => TreeUtil.modifyOptions(span, _ + options)
    case spans => SpanSequence(spans, options)
  }
  
  
  /** Implementation of the `for` directive for block elements in markup documents.
   *  The content of such a block will only be rendered for the corresponding
   *  output format (e.g. `pdf` or `html`).
   */
  lazy val format: Blocks.Directive  = Blocks.create("format") {
    import Blocks.Combinators._
    
    (attribute(Default) ~ body(Default)) {
      (name, content) => 
        TargetFormat(name, asBlock(content))
    }
  }
  
  /** Implementation of the `style` directive for block elements in markup documents.
   */
  lazy val blockStyle: Blocks.Directive  = Blocks.create("style") {
    import Blocks.Combinators._
    
    (attribute(Default) ~ body(Default)) {
      (style, content) => asBlock(content, Styles(style))
    }
  }
  
  /** Implementation of the `style` directive for span elements in markup documents.
   */
  lazy val spanStyle: Spans.Directive  = Spans.create("style") {
    import Spans.Combinators._
    
    (attribute(Default) ~ body(Default)) {
      (style, content) => asSpan(content, Styles(style))
    }
  }
  
  /** Implementation of the `fragment` directive for block elements in markup documents.
   */
  lazy val blockFragment: Blocks.Directive  = Blocks.create("fragment") {
    import Blocks.Combinators._
    
    (attribute(Default) ~ body(Default)) {
      (name, content) => DocumentFragment(name, asBlock(content, Styles(name)))
    }
  }
  
  /** Implementation of the `fragment` directive for templates.
   */
  lazy val templateFragment: Templates.Directive  = Templates.create("fragment") {
    import Templates.Combinators._
    
    (attribute(Default) ~ body(Default)) {
      (name, content) => TemplateElement(DocumentFragment(name, TemplateSpanSequence(content)))
    }
  }
  
  /** Implementation of the `pageBreak` directive.
   */
  lazy val pageBreak: Blocks.Directive  = Blocks.create("pageBreak") {
    import Blocks.Combinators._
    
    empty(PageBreak())
  }
  
  /** The complete list of standard directives for block
   *  elements in markup documents.
   */
  lazy val stdBlockDirectives: Seq[Blocks.Directive] = List(
    blockToc,
    blockFragment,
    blockStyle,
    format,
    pageBreak
  )
  
  /** The complete list of standard directives for span
   *  elements in markup documents.
   */
  lazy val stdSpanDirectives: Seq[Spans.Directive] = List(
    spanStyle
  )

  /** The complete list of standard directives for templates.
   */
  lazy val stdTemplateDirectives: Seq[Templates.Directive] = List(
    templateToc,
    templateFor,
    templateIf
  )
  
}
