/*
 * Copyright 2012-2019 the original author or authors.
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

import laika.api.builder.{OperationConfig, RendererBuilder, TwoPhaseRendererBuilder}
import laika.ast.Path.Root
import laika.ast._
import laika.factory.{RenderContext, RenderFormat, TwoPhaseRenderFormat}
import laika.rewrite.TemplateRewriter

abstract class Renderer (val config: OperationConfig) {

  type Formatter

  def format: RenderFormat[Formatter]

  private lazy val theme = config.themeFor(format)

  private lazy val renderFunction: (Formatter, Element) => String = (fmt, element) =>
    theme.customRenderer.applyOrElse[(Formatter, Element), String]((fmt, element), {
      case (f, e) => format.defaultRenderer(f, e)
    })


  def render (doc: Document): String = render(doc.content, doc.path, theme.defaultStyles)

  def render (doc: Document, styles: StyleDeclarationSet): String = render(doc.content, doc.path, styles)

  def render (element: Element): String = render(element, Root, theme.defaultStyles)

  def render (element: Element, path: Path): String = render(element, path, theme.defaultStyles)

  def render (element: Element, path: Path, styles: StyleDeclarationSet): String = {

    val renderContext = RenderContext(renderFunction, element, styles, path, config)

    val formatter = format.formatterFactory(renderContext)

    renderFunction(formatter, element)
  }
  
  def applyTheme (root: DocumentTreeRoot): DocumentTreeRoot = {
    val styles = theme.defaultStyles ++ root.styles(format.fileSuffix)
    
    val treeWithTpl: DocumentTree = root.tree.getDefaultTemplate(format.fileSuffix).fold(
      root.tree.withDefaultTemplate(theme.defaultTemplateOrFallback, format.fileSuffix)
    )(_ => root.tree)
    
    val preparedRoot = root.copy(tree = treeWithTpl, styles = root.styles + (format.fileSuffix -> styles))
    
    TemplateRewriter.applyTemplates(preparedRoot, format.fileSuffix)
  }
  
  def templateFor (root: DocumentTreeRoot): TemplateRoot = 
    root.tree.getDefaultTemplate(format.fileSuffix).fold(theme.defaultTemplateOrFallback)(_.content)

}

/** Serves as an entry point for building a Renderer instance.
  *
  *  @author Jens Halm
  */
object Renderer {

  /** Returns a new builder instance for the specified render format.
    *  The format is usually an object provided by the library
    *  or a plugin that is capable of producing a specific output format like HTML. 
    */
  def of [FMT] (format: RenderFormat[FMT]): RendererBuilder[FMT] =
    new RendererBuilder[FMT](format, OperationConfig.default)

  /** Returns a new builder instance for the specified two-phase render format.
    * 
    * The format is usually an object provided by the library
    * or a plugin that is capable of producing a specific output format like EPUB or PDF.
    * 
    * While the builder API is defined as part of the laika-core module, the concrete
    * implementations of this renderer type that this library provides (EPUB and PDF) 
    * reside in sub-modules as they require the functionality of the laika-io module.
    */
  def of [FMT, PP] (format: TwoPhaseRenderFormat[FMT, PP]): TwoPhaseRendererBuilder[FMT, PP] =
    new TwoPhaseRendererBuilder[FMT, PP](format, OperationConfig.default)

}
