/*
 * Copyright 2014-2016 the original author or authors.
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

package laika.format

import laika.ast.{Element, Path, StyleDeclarationSet, TemplateDocument}
import laika.config.RenderConfig
import laika.directive.StandardDirectives
import laika.factory.RenderFormat
import laika.io.{Input, Output}
import laika.parse.combinator.Parsers
import laika.parse.css.CSSParsers
import laika.parse.directive.TemplateParsers
import laika.parse.markup.DocumentParser.ParserInput
import laika.parse.text.TextParsers.unsafeParserFunction
import laika.render.{FORenderer, FOWriter}

import scala.language.existentials

/** A renderer for XSL-FO output. May be directly passed to the `Render` or `Transform` APIs:
 * 
 *  {{{
 *  Render as XSLFO from document toFile "hello.fo"
 *  
 *  Transform from Markdown to XSLFO fromFile "hello.md" toFile "hello.fo"
 *  }}}
 *  
 *  This renderer is usually used as an interim format for producing a PDF, 
 *  where you do not deal with this format directly. But it can alternatively
 *  also be used as the final output and then get processed by external tools.
 * 
 *  @author Jens Halm
 */
object XSLFO extends RenderFormat[FOWriter] {
  
  
  val fileSuffix = "fo"
 
  def newRenderer (output: Output, root: Element, render: Element => Unit,
                   styles: StyleDeclarationSet, config: RenderConfig): (FOWriter, Element => Unit) = {

    val writer = new FOWriter(output.asFunction, render, root, output.path, styles, formatted = config.renderFormatted)
    val renderer = new FORenderer(writer, root, output.path, config.minMessageLevel)

    (writer, renderer.render)
  }
  
  override lazy val defaultTheme: Theme = Theme(
    defaultTemplate = Some(templateResource.content),
    defaultStyles = styleResource
  )

  private lazy val styleResource: StyleDeclarationSet = {
    val input = Input.fromClasspath("/styles/default.fo.css", Path.Root / "default.fo.css")
    Parsers.documentParserFunction(CSSParsers.styleDeclarationSet, StyleDeclarationSet.forPath)(ParserInput(input.path, input.asParserInput))
  }

  class XSLFOTemplateParser extends TemplateParsers(Map("for"->StandardDirectives.templateFor)) {
    def parse (input: Input): TemplateDocument = {
      val root = unsafeParserFunction(templateRoot)(input.asParserInput)
      TemplateDocument(input.path, root)
    }
  }
  private lazy val templateResource: TemplateDocument =
    new XSLFOTemplateParser().parse(Input.fromClasspath("/templates/default.template.fo", Path.Root / "default.template.fo"))

}
