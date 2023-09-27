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

package laika.internal.directive

import cats.data.NonEmptySet
import cats.syntax.all.*
import laika.api.bundle.*
import laika.api.config.ConfigValue.SimpleValue
import laika.api.config.Key
import laika.ast.*
import laika.ast.TargetValidation.*
import laika.config.{ LaikaKeys, PlatformDateTime }

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
  * - `target`: Translates a link target.
  *   External targets will be rendered verbatim, internal targets (absolute or relative paths) will be resolved
  *   from the perspective of a template to a path relative to the document the template had been applied to.
  * - `date`: renders a date with a specified formatting pattern
  * - `attribute`: renders an HTML attribute if the specified config value is defined
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
  * [[https://typelevel.org/Laika/latest/07-reference/01-standard-directives.html Standard Directives]] in the manual.
  *
  *  @author Jens Halm
  */
private[laika] object StandardDirectives extends DirectiveRegistry {

  override val description: String = "Laika's built-in directives"

  override val origin: BundleOrigin = BundleOrigin.Library

  private def asBlock(blocks: Seq[Block], options: Options = Options.empty): Block = blocks match {
    case block :: Nil => block.mergeOptions(options)
    case multiple     => BlockSequence(multiple, options)
  }

  private def asSpan(spans: Seq[Span], options: Options): Span = spans match {
    case span :: Nil => span.mergeOptions(options)
    case multiple    => SpanSequence(multiple, options)
  }

  /** Implementation of the `callout` directive for block elements in markup documents.
    * The body of such a directive will get two styles assigned: `callout` and the argument
    * passed to the directive (e.g. `@:callout(info)`).
    */
  lazy val callout: BlockDirectives.Directive = BlockDirectives.create("callout") {
    import laika.api.bundle.BlockDirectives.dsl.*

    (attribute(0).as[String].widen, parsedBody).mapN { (style, body) =>
      BlockSequence(body, Styles("callout", style))
    }
  }

  lazy val target: TemplateDirectives.Directive = TemplateDirectives.eval("target") {
    import laika.api.bundle.TemplateDirectives.dsl.*

    (attribute(0).as[Target], attribute(0).as[String], cursor).mapN {
      (literalTarget, pathKey, cursor) =>
        def resolveTarget(target: Target): Either[String, TemplateSpan] = target match {
          case et: ExternalTarget => Right(TemplateElement(RawLink.external(et.url)))
          case it: InternalTarget =>
            cursor.validate(it) match {
              case ValidTarget => Right(TemplateElement(RawLink(it.relativeTo(cursor.path))))
              case InvalidTarget(message)      => Left(message)
              case RecoveredTarget(message, _) => Left(message)
            }
        }

        val config = cursor.config
        if (config.hasKey(pathKey))
          config.get[Target](pathKey).leftMap(_.message).flatMap(resolveTarget)
        else resolveTarget(literalTarget)
    }
  }

  lazy val date: TemplateDirectives.Directive = TemplateDirectives.eval("date") {
    import laika.api.bundle.TemplateDirectives.dsl.*

    (
      attribute(0).as[String],
      attribute(1).as[String],
      attribute(2).as[String].optional,
      cursor
    ).mapN { (refKey, pattern, localeAttr, cursor) =>
      val locale = localeAttr
        .orElse(cursor.config.get[String](LaikaKeys.metadata.child("language")).toOption)

      cursor.config.get[PlatformDateTime.Type](refKey).leftMap(_.message).flatMap { date =>
        PlatformDateTime
          .formatConstant(date, pattern, locale)
          .getOrElse(PlatformDateTime.format(date, pattern, locale))
          .map(TemplateString(_))
      }
    }
  }

  lazy val attr: TemplateDirectives.Directive = TemplateDirectives.eval("attribute") {
    import laika.api.bundle.TemplateDirectives.dsl.*

    (attribute(0).as[String], attribute(1).as[String], cursor).mapN { (name, ref, cursor) =>
      cursor.resolveReference(Key.parse(ref)).leftMap(_.message).flatMap {
        case None                     => Right(TemplateString(""))
        case Some(value: SimpleValue) => Right(TemplateString(s"""$name="${value.render}""""))
        case Some(_)                  =>
          Left(
            s"value with key '$ref' is a structured value (Array, Object, AST) which is not supported by this directive"
          )
      }
    }
  }

  /** Implementation of the `format` directive for block elements in markup documents.
    *  The content of such a block will only be rendered for the corresponding
    *  output format (e.g. `pdf` or `html`).
    */
  lazy val format: BlockDirectives.Directive = BlockDirectives.eval("format") {
    import laika.api.bundle.BlockDirectives.dsl.*

    (positionalAttributes.as[String].widen, parsedBody.map(asBlock(_))).mapN { (formats, body) =>
      NonEmptySet
        .fromSet(TreeSet(formats: _*))
        .fold[Either[String, Block]](Left("no formats provided"))(set =>
          Right(TargetFormat(set, body))
        )
    }
  }

  /** Implementation of the `style` directive for block elements in markup documents.
    */
  lazy val blockStyle: BlockDirectives.Directive = BlockDirectives.create("style") {
    import laika.api.bundle.BlockDirectives.dsl.*

    (parsedBody, positionalAttributes.as[String].map(Styles(_: _*))).mapN(asBlock)
  }

  /** Implementation of the `style` directive for span elements in markup documents.
    */
  lazy val spanStyle: SpanDirectives.Directive = SpanDirectives.create("style") {
    import laika.api.bundle.SpanDirectives.dsl.*

    (parsedBody, positionalAttributes.as[String].map(Styles(_: _*))).mapN(asSpan)
  }

  /** Implementation of the `icon` directive for span elements in markup documents.
    */
  lazy val iconSpan: SpanDirectives.Directive = SpanDirectives.create("icon") {
    (SpanDirectives.dsl.attribute(0).as[String], SpanDirectives.dsl.source).mapN { (ref, src) =>
      IconReference(ref, src, Styles(ref))
    }
  }

  /** Implementation of the `icon` directive for span elements in templates.
    */
  lazy val iconTemplate: TemplateDirectives.Directive = TemplateDirectives.create("icon") {
    (TemplateDirectives.dsl.attribute(0).as[String], TemplateDirectives.dsl.source).mapN {
      (ref, src) =>
        TemplateElement(IconReference(ref, src, Styles(ref)))
    }
  }

  /** Implementation of the `fragment` directive for block elements in markup documents.
    */
  lazy val blockFragment: BlockDirectives.Directive = BlockDirectives.create("fragment") {
    import laika.api.bundle.BlockDirectives.dsl.*

    (attribute(0).as[String], parsedBody).mapN { (name, content) =>
      DocumentFragment(name, asBlock(content, Styles(name)))
    }
  }

  /** Implementation of the `fragment` directive for templates.
    */
  lazy val templateFragment: TemplateDirectives.Directive = TemplateDirectives.create("fragment") {
    import laika.api.bundle.TemplateDirectives.dsl.*

    (attribute(0).as[String], parsedBody).mapN { (name, content) =>
      TemplateElement(DocumentFragment(name, TemplateSpanSequence(content)))
    }
  }

  /** Implementation of the `pageBreak` directive.
    */
  lazy val pageBreak: BlockDirectives.Directive = BlockDirectives.create("pageBreak") {
    import laika.api.bundle.BlockDirectives.dsl.*

    empty(PageBreak())
  }

  /** Implementation of the `todo` directive for inline elements.
    */
  val todoSpan: SpanDirectives.Directive = SpanDirectives.create("todo") {
    import laika.api.bundle.SpanDirectives.dsl.*
    attribute(0).map { _ => SpanSequence(Nil) }
  }

  /** Implementation of the `todo` directive for block elements.
    */
  val todoBlock: BlockDirectives.Directive = BlockDirectives.create("todo") {
    import laika.api.bundle.BlockDirectives.dsl.*
    attribute(0).map { _ => BlockSequence(Nil) }
  }

  /** The complete list of standard directives for block elements in markup documents.
    */
  lazy val blockDirectives: Seq[BlockDirectives.Directive] = List(
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
  lazy val spanDirectives: Seq[SpanDirectives.Directive] = List(
    ImageDirectives.forSpans,
    spanStyle,
    iconSpan,
    todoSpan
  )

  /** The complete list of standard directives for templates.
    */
  lazy val templateDirectives: Seq[TemplateDirectives.Directive] = List(
    BreadcrumbDirectives.forTemplates,
    NavigationTreeDirectives.forTemplates,
    ControlFlowDirectives.templateFor,
    ControlFlowDirectives.templateIf,
    IncludeDirectives.templateInclude,
    IncludeDirectives.templateEmbed,
    iconTemplate,
    target,
    date,
    attr
  )

  /** The complete list of standard directives for links.
    */
  lazy val linkDirectives: Seq[LinkDirectives.Directive] = Seq(
    StandardLinkDirectives.api,
    StandardLinkDirectives.source
  )

}
