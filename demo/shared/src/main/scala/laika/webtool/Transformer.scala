/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.webtool

import laika.api.{MarkupParser, Renderer}
import laika.ast.{CodeBlock, Paragraph, Path, Style, Styles}
import laika.bundle.SyntaxHighlighter
import laika.factory.MarkupFormat
import laika.format.{AST, HTML, Markdown, ReStructuredText}
import laika.markdown.github.GitHubFlavor
import laika.parse.code.SyntaxHighlighting
import laika.parse.code.languages.{HTMLSyntax, LaikaASTSyntax}
import laika.parse.markup.DocumentParser.ParserError

/**
  * @author Jens Halm
  */
object Transformer {
  
  private lazy val parsers: Map[MarkupFormat, MarkupParser] = Map(
    Markdown -> MarkupParser.of(Markdown).using(GitHubFlavor, SyntaxHighlighting).build,
    ReStructuredText -> MarkupParser.of(ReStructuredText).using(SyntaxHighlighting).build
  )
  
  private val astRenderer = Renderer.of(AST).build
  
  private val htmlRenderer = Renderer.of(HTML).build
  
  private val htmlSourceRenderer = Renderer.of(HTML)
    .rendering {
      case (fmt, Paragraph(content,opt)) => 
        fmt.indentedElement("p", opt, content)
        
      case (fmt, cb@CodeBlock(lang,content,opt)) => 
        val codeStyles = if (cb.hasSyntaxHighlighting) Style.noHighlight else Styles(lang)
        "<pre>" + fmt.indentedElement("code", codeStyles, content) + "</pre>"
    }.build

  private def highlightAndRender (highlighter: SyntaxHighlighter, src: String): Either[ParserError, String] =
    highlighter.rootParser
      .parse(src).toEither
      .left.map(msg => ParserError(msg, Path.Root))
      .map(CodeBlock(highlighter.language.head, _))
      .map(htmlRenderer.render)

  private def transformToRenderedHTML (format: MarkupFormat, input: String, renderer: Renderer): Either[ParserError, String] =
    parsers(format).parse(input).map(renderer.render)
  
  def transformToRenderedHTML (format: MarkupFormat, input: String): Either[ParserError, String] =
    transformToRenderedHTML(format, input, htmlRenderer)
  
  def transformToHTMLSource (format: MarkupFormat, input: String): Either[ParserError, String] =
    transformToRenderedHTML(format, input, htmlSourceRenderer).flatMap(highlightAndRender(HTMLSyntax, _))
  
  def transformToUnresolvedAST (format: MarkupFormat, input: String): Either[ParserError, String] =
    parsers(format).parseUnresolved(input)
      .map(r => astRenderer.render(r.document.content))
      .flatMap(highlightAndRender(LaikaASTSyntax, _))
  
  def transformToResolvedAST (format: MarkupFormat, input: String): Either[ParserError, String] =
    parsers(format).parse(input)
      .map(r => astRenderer.render(r.content))
      .flatMap(highlightAndRender(LaikaASTSyntax, _))

}
