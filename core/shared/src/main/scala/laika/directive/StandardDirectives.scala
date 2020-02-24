/*
 * Copyright 2012-2020 the original author or authors.
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

import cats.implicits._
import laika.config.{ArrayValue, BooleanValue, ConfigValue, Key, NullValue, ObjectValue, StringValue}
import laika.ast.{SpanResolver, TemplateSpan, _}
import laika.bundle.BundleOrigin
import laika.rewrite.TemplateRewriter
import laika.rewrite.nav.TocGenerator

import scala.annotation.tailrec

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
 *  - `styleLink`: Adds link elements to HTML output for all CSS files found in the document tree
 *  - `fragment`: Adds the body as a fragment to the target document, separate from the main
 *    content, to be rendered in different locations of the output, like headers, footers or sidebars.
 *  - `pageBreak`: Inserts a page break element into the tree (will only be rendered by page-based
 *    output, like XSL-FO or PDF.
 *  
 *  @author Jens Halm
 */
object StandardDirectives extends DirectiveRegistry {

  override val description: String = "Laika's built-in directives"

  override val origin: BundleOrigin = BundleOrigin.Library

  /** Implementation of the `for` directive for templates.
   */
  lazy val templateFor: Templates.Directive = Templates.create("for") {

    import Templates.dsl._

    val emptyValues = Set[ConfigValue](StringValue(""), BooleanValue(false), NullValue)
    case class Empty (spans: Seq[TemplateSpan])
    val emptySeparator = Templates.separator("empty", max = 1)(parsedBody.map(Empty))
    
    (defaultAttribute.as[String], separatedBody(Seq(emptySeparator)), cursor).mapN { (ref, multipart, cursor) =>
        
      def rewrite (spans: Seq[TemplateSpan], childCursor: DocumentCursor): TemplateSpanSequence =
        TemplateSpanSequence(spans) rewriteChildren TemplateRewriter.rewriteRules(childCursor)
      
      def rewriteContent (value: ConfigValue): TemplateSpanSequence = rewrite(multipart.mainBody, cursor.withReferenceContext(value))

      def rewriteFallback = multipart.children.headOption.map(_.spans).map(rewrite(_, cursor)).getOrElse(TemplateSpanSequence.empty)
      
      cursor.resolveReference(Key.parse(ref)) match {
        case Right(Some(o: ObjectValue))             => rewriteContent(o) 
        case Right(Some(a: ArrayValue)) if a.isEmpty => rewriteFallback
        case Right(Some(a: ArrayValue))              => TemplateSpanSequence(a.values.map(rewriteContent))
        case Right(Some(simpleValue)) if emptyValues(simpleValue) => rewriteFallback
        case Right(Some(simpleValue))                => rewriteContent(simpleValue)
        case Right(None)                             => rewriteFallback
        case Left(error)                             => InvalidElement(s"Error retrieving reference '$ref': ${error.message}", "${"+ref+"}").asTemplateSpan
      }
    }
  }
  
  /** Implementation of the `if` directive for templates.
   */
  lazy val templateIf: Templates.Directive = Templates.create("if") {

    import Templates.dsl._
    
    val trueStrings = Set("true","yes","on","enabled")
    
    sealed trait IfSeparator extends Product with Serializable
    case class ElseIf (ref: String, body: Seq[TemplateSpan]) extends IfSeparator
    case class Else (body: Seq[TemplateSpan]) extends IfSeparator
    
    val elseIfSep = Templates.separator("elseIf") {
      (defaultAttribute.as[String], parsedBody).mapN(ElseIf) 
    }
    val elseSep = Templates.separator("else", max = 1) {
      parsedBody.map(Else)
    }
    val multipartBody = separatedBody(Seq(elseIfSep, elseSep))

    (defaultAttribute.as[String], multipartBody, cursor).mapN { (path, multipart, cursor) =>

      def rewrite (spans: Seq[TemplateSpan]): TemplateSpanSequence =
        TemplateSpanSequence(spans) rewriteChildren TemplateRewriter.rewriteRules(cursor)
      
      def rewriteFallback = multipart.children
        .collectFirst { case e: Else => e }
        .map(_.body).map(rewrite)
        .getOrElse(TemplateSpanSequence.empty)
      
      @tailrec
      def process (parts: Seq[ElseIf]): TemplateSpanSequence = 
        if (parts.isEmpty) rewriteFallback
        else cursor.resolveReference(Key.parse(parts.head.ref)) match {
          case Right(Some(BooleanValue(true)))               => rewrite(parts.head.body)
          case Right(Some(StringValue(s))) if trueStrings(s) => rewrite(parts.head.body)
          case _ => process(parts.tail)
        }
      
      val alternatives = ElseIf(path, multipart.mainBody) +: multipart.collect[ElseIf] 
      process(alternatives)
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
      case "<rootTree>" | "#rootTree" => cursor.root.target.tree // # syntax is legacy, clashes with HOCON spec
      case "<currentTree>" | "#currentTree"     => cursor.parent.target
      case "<currentDocument>" | "#currentDocument" => cursor.target
      case pathString =>
        val root = cursor.root.target.tree
        val configPath = PathBase.parse(pathString) match {
          case p: RelativePath => cursor.parent.target.path / p
          case p: Path => p
        }
        val lookupPath = configPath.relativeTo(root.path)
        root
          .selectDocument(lookupPath)
          .orElse(root.selectSubtree(lookupPath))
          .getOrElse(cursor.root.target.tree)
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

    import Templates.dsl._

    (attribute("depth").as[Int].optional, 
        attribute("root").as[String].optional, 
        attribute("title").as[String].optional, 
        cursor).mapN {
      (depth, rootConfig, title, cursor) =>
        TemplateElement(toc(depth, rootConfig.getOrElse("<rootTree>"), title, cursor))
    }
  }
  
  /** Implementation of the `toc` directive for block elements in markup documents.
   */
  lazy val blockToc: Blocks.Directive  = Blocks.create("toc") {

    import Blocks.dsl._
    
    (attribute("depth").as[Int].optional, 
        attribute("root").as[String].optional, 
        attribute("title").as[String].optional, 
        cursor).mapN {
      (depth, rootConfig, title, cursor) =>
        toc(depth, rootConfig.getOrElse("#currentDocument"), title, cursor)
    }
  }
  
  
  private def asBlock (blocks: Seq[Block], options: Options = NoOpt): Block = blocks match {
    case block :: Nil => block.mergeOptions(options)
    case multiple     => BlockSequence(multiple, options)
  }
  
  private def asSpan (spans: Seq[Span], options: Options = NoOpt): Span = spans match {
    case span :: Nil => span.mergeOptions(options)
    case multiple    => SpanSequence(multiple, options)
  }
  
  
  /** Implementation of the `for` directive for block elements in markup documents.
   *  The content of such a block will only be rendered for the corresponding
   *  output format (e.g. `pdf` or `html`).
   */
  lazy val format: Blocks.Directive  = Blocks.create("format") {
    import Blocks.dsl._
    
    (defaultAttribute.as[String], parsedBody.map(asBlock(_))).mapN(TargetFormat(_,_))
  }
  
  /** Implementation of the `style` directive for block elements in markup documents.
   */
  lazy val blockStyle: Blocks.Directive  = Blocks.create("style") {
    import Blocks.dsl._
    
    (parsedBody, defaultAttribute.as[String].map(Styles(_))).mapN(asBlock)
  }
  
  /** Implementation of the `style` directive for span elements in markup documents.
   */
  lazy val spanStyle: Spans.Directive  = Spans.create("style") {
    import Spans.dsl._

    (parsedBody, defaultAttribute.as[String].map(Styles(_))).mapN(asSpan)
  }
  
  /** Implementation of the `fragment` directive for block elements in markup documents.
   */
  lazy val blockFragment: Blocks.Directive  = Blocks.create("fragment") {
    import Blocks.dsl._
    
    (defaultAttribute.as[String], parsedBody).mapN { (name, content) =>
      DocumentFragment(name, asBlock(content, Styles(name)))
    }
  }
  
  /** Implementation of the `fragment` directive for templates.
   */
  lazy val templateFragment: Templates.Directive  = Templates.create("fragment") {
    import Templates.dsl._
    
    (defaultAttribute.as[String], parsedBody).mapN { (name, content) =>
      TemplateElement(DocumentFragment(name, TemplateSpanSequence(content)))
    }
  }
  
  /** Implementation of the `pageBreak` directive.
   */
  lazy val pageBreak: Blocks.Directive  = Blocks.create("pageBreak") {
    import Blocks.dsl._
    
    empty(PageBreak())
  }
  
  /** The complete list of standard directives for block
   *  elements in markup documents.
   */
  lazy val blockDirectives: Seq[Blocks.Directive] = List(
    blockToc,
    blockFragment,
    blockStyle,
    format,
    pageBreak
  )
  
  /** The complete list of standard directives for span
   *  elements in markup documents.
   */
  lazy val spanDirectives: Seq[Spans.Directive] = List(
    spanStyle
  )

  /** Template resolver that inserts links to all CSS inputs found in the document tree, using a path
    * relative to the currently processed document. 
    *
    * Only has an effect for HTML and EPUB output, will be ignored for PDF output.
    * 
    * This is an alternative to the `@styleLinks` directive that can be used where
    * template ASTs are created programmatically for extensions.
    */
  case object StyleLinks extends SpanResolver with TemplateSpan {
    type Self = this.type
    def withOptions (options: Options): this.type = this
    val options: Options = NoOpt

    def resolve (cursor: DocumentCursor): TemplateElement = {
      val refPath = cursor.parent.target.path
      val allLinks = cursor.root.target.staticDocuments.filter(_.suffix.contains("css")).map { staticPath =>
        val path = staticPath.relativeTo(refPath).toString
        s"""<link rel="stylesheet" type="text/css" href="$path" />"""
      }
      TemplateElement(RawContent(Seq("html","xhtml"), allLinks.mkString("\n    ")))
    }
  }

  /** Template directive that inserts links to all CSS inputs found in the document tree, using a path
    * relative to the currently processed document. 
    * 
    * Only has an effect for HTML and EPUB output, will be ignored for PDF output.
    */
  lazy val styleLinksDirective: Templates.Directive = Templates.create("styleLinks") {
    Templates.dsl.cursor.map(StyleLinks.resolve)
  }

  /** The complete list of standard directives for templates.
   */
  lazy val templateDirectives: Seq[Templates.Directive] = List(
    templateToc,
    templateFor,
    templateIf,
    styleLinksDirective
  )
  
}
