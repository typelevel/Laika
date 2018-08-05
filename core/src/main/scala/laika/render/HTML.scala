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

package laika.render

import laika.api.config.RenderConfig
import laika.directive.DefaultTemplateParser
import laika.factory.RenderFormat
import laika.io.{Input, Output}
import laika.parse.css.Styles.StyleDeclarationSet
import laika.tree.Documents.TemplateDocument
import laika.tree.Elements.Element
import laika.tree.Paths.Root

/** A render format for HTML output. May be directly passed to the `Render` or `Transform` APIs:
 * 
 *  {{{
 *  Render as HTML from document toFile "hello.html"
 *  
 *  Transform from Markdown to HTML fromFile "hello.md" toFile "hello.html"
 *  }}}
 * 
 *  @author Jens Halm
 */
object HTML extends RenderFormat[HTMLWriter] {
  
  val fileSuffix = "html"
 
  def newRenderer (output: Output, root: Element, render: Element => Unit,
                   styles: StyleDeclarationSet, config: RenderConfig): (HTMLWriter, Element => Unit) = {

    val writer = new HTMLWriter(output.asFunction, render, root, formatted = config.renderFormatted)
    val renderer = new HTMLRenderer(writer, config.minMessageLevel)

    (writer, renderer.render)
  }

  override lazy val defaultTheme: Theme = Theme(defaultTemplate = Some(templateResource.content))

  private lazy val templateResource: TemplateDocument =
    DefaultTemplateParser.parse(Input.fromClasspath("/templates/default.template.html", Root / "default.template.html"))

}
