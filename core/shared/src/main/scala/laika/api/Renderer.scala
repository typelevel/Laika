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

package laika.api

import cats.syntax.all.*
import laika.api.builder.{ OperationConfig, RendererBuilder, TwoPhaseRendererBuilder }
import laika.api.errors.RendererError
import laika.ast.Path.Root
import laika.ast.*
import laika.api.format.Formatter.Indentation
import laika.api.format.{ MarkupFormat, RenderContext, RenderFormat, TwoPhaseRenderFormat }
import laika.rewrite.OutputContext
import laika.rewrite.nav.PathTranslator

/** Performs a render operation from a document AST to a target format
  * as a string. The document AST may be obtained by a preceding parse
  * operation or constructed programmatically.
  *
  * In cases where a parse operation should precede immediately, it is more
  * convenient to use a [[laika.api.Transformer]] instead which
  * combines a parse and a render operation directly.
  *
  * Example for rendering HTML:
  *
  * {{{
  *  val doc: Document = ???
  *
  *  val res: String = Renderer
  *    .of(HTML)
  *    .build
  *    .render(doc)
  * }}}
  *
  * This is a pure API that does not perform any side-effects.
  * For additional options like File and Stream I/O, templating
  * or parallel processing, use the corresponding builders in
  * the laika-io module.
  *
  * @author Jens Halm
  */
abstract class Renderer private[laika] (val config: OperationConfig, skipRewrite: Boolean = false) {
  self =>

  type Formatter

  def format: RenderFormat[Formatter]

  def forInputFormat(markupFormat: MarkupFormat): Renderer =
    new RendererBuilder(format, config.withBundlesFor(markupFormat), skipRewrite).build

  private lazy val renderFunction: (Formatter, Element) => String = (fmt, element) =>
    config.renderOverridesFor(format).value.applyOrElse[(Formatter, Element), String](
      (fmt, element),
      { case (f, e) =>
        format.defaultRenderer(f, e)
      }
    )

  private val defaultPathTranslator: PathTranslator = PathTranslator.noOp

  /** Renders the specified document as a String.
    */
  def render(doc: Document): Either[RendererError, String] =
    render(doc, doc.content, defaultPathTranslator, StyleDeclarationSet.empty)

  /** Renders the specified document as a String, using the given path translator and styles.
    *
    * Currently only PDF/XSL-FO output processes styles, all other formats will ignore them.
    */
  def render(
      doc: Document,
      pathTranslator: PathTranslator,
      styles: StyleDeclarationSet
  ): Either[RendererError, String] =
    render(doc, doc.content, pathTranslator, styles)

  /** Renders the specified element as a String.
    */
  def render(element: Element): Either[RendererError, String] =
    render(element, (Root / "doc").withSuffix(format.fileSuffix))

  /** Renders the specified element as a String.
    *
    * The provided (virtual) path may be used by renderers for cross-linking between documents.
    */
  def render(element: Element, path: Path): Either[RendererError, String] =
    render(element, path, defaultPathTranslator, StyleDeclarationSet.empty)

  /** Renders the specified element as a String, using the given path translator and styles.
    *
    * Currently only PDF/XSL-FO output processes styles, all other formats will ignore them.
    *
    * The provided (virtual) path may be used by renderers for cross-linking between documents.
    */
  def render(
      element: Element,
      path: Path,
      pathTranslator: PathTranslator,
      styles: StyleDeclarationSet
  ): Either[RendererError, String] = {

    val rootElement = element match {
      case root: RootElement => root
      case block: Block      => RootElement(block)
      case span: Span        => RootElement(Paragraph(span))
      case other             => RootElement(Paragraph(TemplateElement(other)))
    }
    render(Document(path, rootElement), element, pathTranslator, styles)
  }

  private def render(
      doc: Document,
      targetElement: Element,
      pathTranslator: PathTranslator,
      styles: StyleDeclarationSet
  ): Either[RendererError, String] = {

    def rewrite: Either[RendererError, Element] = {
      config
        .rewriteRulesFor(doc, RewritePhase.Render(OutputContext(format)))
        .map(_.rewriteElement(targetElement))
        .leftMap(RendererError(_))
    }

    (if (skipRewrite) Right(targetElement) else rewrite).map { elementToRender =>
      val renderContext =
        new RenderContext[Formatter](
          renderFunction,
          elementToRender,
          Nil,
          styles,
          doc.path,
          pathTranslator,
          if (config.renderFormatted) Indentation.default else Indentation.none,
          config.renderMessages
        )

      val formatter = format.formatterFactory(renderContext)

      renderFunction(formatter, elementToRender)
    }
  }

  /** Creates a new instance that will skip the rewrite phase when rendering elements.
    *
    * Useful when rewriting has already been performed by a processing step external to the the library's core APIs.
    */
  def skipRewritePhase: Renderer = new Renderer(config, skipRewrite = true) {
    type Formatter = self.Formatter
    def format = self.format
  }

}

/** Entry point for building a Renderer instance.
  *
  * @author Jens Halm
  */
object Renderer {

  /** Returns a new builder instance for the specified render format.
    *
    * The format is usually an object provided by the library
    * or a plugin that is capable of producing a specific output format like HTML.
    */
  def of[FMT](format: RenderFormat[FMT]): RendererBuilder[FMT] =
    new RendererBuilder[FMT](format, OperationConfig.default)

  /** Returns a new builder instance for the specified two-phase render format.
    *
    * The format is usually an object provided by the library
    * or a plugin that is capable of producing a specific output format like EPUB or PDF.
    *
    * While the builder API for two-phase renderers is defined as part of the laika-core module, the concrete
    * implementations of this renderer type that this library provides (EPUB and PDF)
    * reside in sub-modules as they require the functionality of the laika-io module.
    */
  def of[FMT, PP](format: TwoPhaseRenderFormat[FMT, PP]): TwoPhaseRendererBuilder[FMT, PP] =
    new TwoPhaseRendererBuilder[FMT, PP](format, OperationConfig.default)

}
