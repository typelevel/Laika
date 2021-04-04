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

package laika.directive.std

import cats.data.NonEmptySet
import cats.syntax.all._
import laika.ast._
import laika.bundle.BundleOrigin
import laika.config.{Key, SimpleConfigValue}
import laika.directive._
import laika.rewrite.link.{InvalidTarget, RecoveredTarget, ValidTarget}

import scala.collection.immutable.TreeSet

/** Provides the implementation for the standard directives included in Laika.
  *  
  * These include:
  * 
  * '''Navigation'''
  * 
  * - `navigationTree`: Generates a navigation tree either automatically from specified root nodes
  *    of the input tree or by specifying entries manually (or a combination of both).
  * - `breadcrumb`: Builds a navigation list from the root node of the input tree to the current document.
  * - `api`: Convenience directive that allows to reference an api documentation entry (e.g. scaladoc, javadoc)
  * - `source`: Convenience directive that allows to reference a hosted source (e.g. on GitHub)
  * 
  * '''Inclusions'''
  * 
  * - `include`: Includes one template or markup document inside another, with the options to pass attributes
  *   that can be referenced in the included document.
  * - `embed`: All the features of `include`, but also allows to pass a parsed directive body that can be
  *   referenced in the included document.
  * 
  * '''Applying Styles'''
  * 
  * - `style`: Adds one or more style properties to the body element (markup block or span).
  * 
  * '''Markup Blocks'''
  * 
  * - `image`: Alternative to native markup syntax for including images that supports additional attributes,
  *   like `intrinsicWidth` and `intrinsicHeight` to avoid layout shift, as well as `alt` and `title`.
  * - `callout`: A decorated block element that stands out from the surrounding paragraphs; 
  *   the default Helium theme renders the content with background color and icon.
  * - `select`: Allows to create alternative versions of the same documentation, 
  *   for example one with Scala code examples and one with Java.
  * - `fragment`: Marks a block in a markup document as being separate from the main content, 
  *   so that it can be placed independently in templates, e.g. in headers, footers or sidebars.
  * - `format`: Process the body element only when the output format matches the format
  *   specified in the directive (e.g. `pdf` or `html`).
  * 
  * '''HTML Templates'''
  * 
  * - `linkCSS`: Adds link elements to HTML/EPUB output for all or selected CSS files found in the document tree
  * - `linkJS`: Adds link elements to HTML/EPUB output for all or selected JavaScript files found in the document tree
  * - `relativePath`: Translates an absolute or relative path from the perspective of a template
  *   to a path relative to the document the template had been applied to
  * 
  * '''Conditionals and Loops'''
  * 
  * - `for`: Accesses a value from the context and sets it as the reference context for its body elements, 
  *   executing the body if the referenced value is non-empty and executing it multiple times when it is a collection.
  * - `if`: Accesses a value from the context and processes the body element only when it is a value recognized as true.
  * 
  * '''PDF Output'''
  * 
  * - `pageBreak`: Inserts a page break element into the tree (will only be rendered by page-based output, 
  *   like XSL-FO or PDF.
  * 
  * '''Comments'''
  * 
  * - `todo`: simple directive that accepts a string argument that will be ignored by renderers,
  *   overcoming the limitation that Markdown does not have a native comment syntax.
  * 
  * For extensive documentation see 
  * [[https://planet42.github.io/Laika/07-reference/01-standard-directives.html Standard Directives]] in the manual.
  * 
  *  @author Jens Halm
  */
object StandardDirectives extends DirectiveRegistry {

  override val description: String = "Laika's built-in directives"

  override val origin: BundleOrigin = BundleOrigin.Library

  private def asBlock (blocks: Seq[Block], options: Options = NoOpt): Block = blocks match {
    case block :: Nil => block.mergeOptions(options)
    case multiple     => BlockSequence(multiple, options)
  }
  
  private def asSpan (spans: Seq[Span], options: Options = NoOpt): Span = spans match {
    case span :: Nil => span.mergeOptions(options)
    case multiple    => SpanSequence(multiple, options)
  }

  /** Implementation of the `callout` directive for block elements in markup documents.
    * The body of such a directive will get two styles assigned: `callout` and the argument
    * passed to the directive (e.g. `@:callout(info)`).
    */
  lazy val callout: Blocks.Directive = Blocks.create("callout") {
    import Blocks.dsl._

    (attribute(0).as[String].widen, parsedBody).mapN { (style, body) =>
      BlockSequence(body, Styles("callout", style))
    }
  }

  lazy val path: Templates.Directive = Templates.eval("path") {
    import Templates.dsl._
    
    (attribute(0).as[Path], cursor).mapN { (path, cursor) =>
      val it = InternalTarget(path)
      cursor.validate(it) match {
        case ValidTarget                 => Right(TemplateString(path.relativeTo(cursor.path).toString))
        case InvalidTarget(message)      => Left(message)
        case RecoveredTarget(message, _) => Left(message)
      }
    }
  }
  
  lazy val attr: Templates.Directive = Templates.eval("attribute") {
    import Templates.dsl._

    (attribute(0).as[String], attribute(1).as[String], cursor).mapN { (name, ref, cursor) =>
      cursor.resolveReference(Key.parse(ref)).leftMap(_.message).flatMap {
        case None                                   => Right(TemplateString(""))
        case Some(value: SimpleConfigValue)         => Right(TemplateString(s"""$name="${value.render}""""))
        case Some(_) => 
          Left(s"value with key '$ref' is a structured value (Array, Object, AST) which is not supported by this directive")
      }
    }
  }
  
  /** Implementation of the `format` directive for block elements in markup documents.
   *  The content of such a block will only be rendered for the corresponding
   *  output format (e.g. `pdf` or `html`).
   */
  lazy val format: Blocks.Directive = Blocks.eval("format") {
    import Blocks.dsl._
    
    (positionalAttributes.as[String].widen, parsedBody.map(asBlock(_))).mapN { (formats, body) =>
      NonEmptySet
        .fromSet(TreeSet(formats:_*))
        .fold[Either[String, Block]](Left("no formats provided"))(set => Right(TargetFormat(set, body)))
    }
  }
  
  /** Implementation of the `style` directive for block elements in markup documents.
   */
  lazy val blockStyle: Blocks.Directive  = Blocks.create("style") {
    import Blocks.dsl._
    
    (parsedBody, positionalAttributes.as[String].map(Styles(_:_*))).mapN(asBlock)
  }
  
  /** Implementation of the `style` directive for span elements in markup documents.
   */
  lazy val spanStyle: Spans.Directive  = Spans.create("style") {
    import Spans.dsl._

    (parsedBody, positionalAttributes.as[String].map(Styles(_:_*))).mapN(asSpan)
  }
  
  /** Implementation of the `fragment` directive for block elements in markup documents.
   */
  lazy val blockFragment: Blocks.Directive  = Blocks.create("fragment") {
    import Blocks.dsl._
    
    (attribute(0).as[String], parsedBody).mapN { (name, content) =>
      DocumentFragment(name, asBlock(content, Styles(name)))
    }
  }
  
  /** Implementation of the `fragment` directive for templates.
   */
  lazy val templateFragment: Templates.Directive  = Templates.create("fragment") {
    import Templates.dsl._
    
    (attribute(0).as[String], parsedBody).mapN { (name, content) =>
      TemplateElement(DocumentFragment(name, TemplateSpanSequence(content)))
    }
  }
  
  /** Implementation of the `pageBreak` directive.
   */
  lazy val pageBreak: Blocks.Directive = Blocks.create("pageBreak") {
    import Blocks.dsl._
    
    empty(PageBreak())
  }

  /** Implementation of the `todo` directive for inline elements.
    */
  val todoSpan: Spans.Directive = Spans.create("todo") {
    import Spans.dsl._
    attribute(0).map { _ => SpanSequence(Nil) }
  }

  /** Implementation of the `todo` directive for block elements.
    */
  val todoBlock: Blocks.Directive = Blocks.create("todo") {
    import Blocks.dsl._
    attribute(0).map { _ => BlockSequence(Nil) }
  }
  
  /** The complete list of standard directives for block elements in markup documents.
    */
  lazy val blockDirectives: Seq[Blocks.Directive] = List(
    BreadcrumbDirectives.forBlocks,
    NavigationTreeDirectives.forBlocks,
    blockFragment,
    blockStyle,
    IncludeDirectives.blockInclude,
    IncludeDirectives.blockEmbed,
    ImageDirectives.forBlocks,
    SelectDirective.forBlocks,
    callout,
    format,
    pageBreak,
    todoBlock
  )

  /** The complete list of standard directives for span elements in markup documents.
    */
  lazy val spanDirectives: Seq[Spans.Directive] = List(
    ImageDirectives.forSpans,
    spanStyle,
    todoSpan
  )
  
  /** The complete list of standard directives for templates.
   */
  lazy val templateDirectives: Seq[Templates.Directive] = List(
    BreadcrumbDirectives.forTemplates,
    NavigationTreeDirectives.forTemplates,
    ControlFlowDirectives.templateFor,
    ControlFlowDirectives.templateIf,
    IncludeDirectives.templateInclude,
    IncludeDirectives.templateEmbed,
    HTMLHeadDirectives.linkCSS,
    HTMLHeadDirectives.linkJS,
    path,
    attr
  )

  /** The complete list of standard directives for links.
    */
  lazy val linkDirectives: Seq[Links.Directive] = Seq(
    LinkDirectives.api,
    LinkDirectives.source
  )
  
}
