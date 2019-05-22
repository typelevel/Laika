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

import laika.api.builder.RendererBuilder
import laika.ast.Path.Root
import laika.ast.{Document, Element, Path, StyleDeclarationSet}
import laika.config.OperationConfig
import laika.factory.{RenderContext, RenderFormat}

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

}

/** Serves as an entry point for building a Renderer instance.
  *
  *  @author Jens Halm
  */
object Renderer {

  /** Returns a new builder instance for the specified render format.
    *  The format is usually an object provided by the library
    *  or a plugin that is capable of parsing a specific markup
    *  format like Markdown or reStructuredText. 
    *
    *  @param format the renderer factory responsible for creating the final renderer
    */
  def of [FMT] (format: RenderFormat[FMT]): RendererBuilder[FMT] =
    new RendererBuilder[FMT](format, OperationConfig.default)

}
