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
import laika.directive._

import scala.collection.immutable.TreeSet

/** Provides the implementation for the standard directives included in Laika.
  *  
  * These include:
  * 
  * - `navigationTree`: Generates a navigation tree either automatically from specified root nodes
  *    of the input tree or by specifying entries manually (or a combination of both).
  * - `breadcrumb`: Builds a navigation list from the root node of the input tree to the current document.
  * - `ref`: Convenience directive that allows to reference a different section of the input tree
  *   via its headline text. The headline does not have to be unique, as long as the directive can identify
  *   a section that is closer than the others with the same headline.
  * - `api`: Convenience directive that allows to reference an api documentation entry (e.g. scaladoc, javadoc)
  * - `source`: Convenience directive that allows to reference a hosted source (e.g. on GitHub)
  * - `fragment`: Marks a block in a markup document as being separate from the main content, 
  *   so that it can be placed separately in templates.
  * - `for`: Accesses a value from the context and sets it as the reference context for its
  *   body elements, executing the body if the referenced value is non-empty and executing
  *   it multiple times when it is a collection.
  * - `if`: Accesses a value from the context and processes the body element only when
  *   it is a value recognized as true.
  * - `format`: Process the body element only when the output format matches the format
  *   specified in the directive (e.g. `pdf` or `html`).
  * - `style`: Adds a style property to the body element.
  * - `linkCSS`: Adds link elements to HTML/EPUB output for all or selected CSS files found in the document tree
  * - `linkJS`: Adds link elements to HTML/EPUB output for all or selected JavaScript files found in the document tree
  * - `fragment`: Adds the body as a fragment to the target document, separate from the main
  *   content, to be rendered in different locations of the output, like headers, footers or sidebars.
  * - `relativePath`: Translates an absolute or relative path from the perspective of a template
  *   to a path relative to the document the template had been applied to
  * - `pageBreak`: Inserts a page break element into the tree (will only be rendered by page-based
  *   output, like XSL-FO or PDF.
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

  lazy val relativePath: Templates.Directive = Templates.create("relativePath") {
    import Templates.dsl._
    
    (attribute(0).as[Path], cursor).mapN { (path, cursor) =>
      TemplateString(path.relativeTo(cursor.path).toString)
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
    
    (parsedBody, attribute(0).as[String].map(Styles(_))).mapN(asBlock)
  }
  
  /** Implementation of the `style` directive for span elements in markup documents.
   */
  lazy val spanStyle: Spans.Directive  = Spans.create("style") {
    import Spans.dsl._

    (parsedBody, attribute(0).as[String].map(Styles(_))).mapN(asSpan)
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
    relativePath
  )

  /** The complete list of standard directives for links.
    */
  lazy val linkDirectives: Seq[Links.Directive] = Seq(
    LinkDirectives.api,
    LinkDirectives.source
  )
  
}
