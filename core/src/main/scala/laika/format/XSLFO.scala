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

package laika.format

import laika.ast.{Element, Path, StyleDeclarationSet, TemplateDocument}
import laika.directive.StandardDirectives
import laika.factory.{RenderContext, RenderFormat}
import laika.parse.css.CSSParsers
import laika.parse.directive.TemplateParsers
import laika.parse.markup.DocumentParser
import laika.parse.markup.DocumentParser.ParserInput
import laika.parse.text.TextParsers.unsafeParserFunction
import laika.render._

import scala.language.existentials

/** A renderer for XSL-FO output. May be directly passed to the `Render` or `Transform` APIs:
 * 
 *  {{{
 *  Renderer.of(XSLFO).build.render(document)
 *  
 *  Transformer.from(Markdown).to(XSLFO).build.transform(inputString)
 *  }}}
 *  
 *  This renderer is usually used as an interim format for producing a PDF, 
 *  where you do not deal with this format directly. But it can alternatively
 *  also be used as the final output and then get processed by external tools.
 * 
 *  @author Jens Halm
 */
object XSLFO extends RenderFormat[FOFormatter] {
  
  
  val fileSuffix = "fo"

  val defaultRenderer: (FOFormatter, Element) => String = FORenderer

  val formatterFactory: RenderContext[FOFormatter] => FOFormatter = FOFormatter

  override lazy val defaultTheme: Theme = Theme(
    defaultTemplate = Some(templateResource.content),
    defaultStyles = styleResource
  )

  private lazy val styleResource: StyleDeclarationSet = {
    val input = TempResourceProvider.classPathParserInput("/styles/default.fo.css", Path.Root / "default.fo.css")
    DocumentParser.forStyleSheets(CSSParsers.styleDeclarationSet)(input).right.get // TODO - 0.12 - temporary
  }

  class XSLFOTemplateParser extends TemplateParsers(Map("for"->StandardDirectives.templateFor)) {
    def parse (input: ParserInput): TemplateDocument = {
      val root = unsafeParserFunction(templateRoot)(input.context)
      TemplateDocument(input.path, root)
    }
  }
  private lazy val templateResource: TemplateDocument =
    new XSLFOTemplateParser().parse(TempResourceProvider.classPathParserInput("/templates/default.template.fo", Path.Root / "default.template.fo"))

}
