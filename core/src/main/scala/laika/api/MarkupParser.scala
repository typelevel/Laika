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

import laika.api.builder.ParserBuilder
import laika.ast.{Document, DocumentCursor, Path, UnresolvedDocument}
import laika.ast.Path.Root
import laika.api.builder.OperationConfig
import laika.factory.MarkupFormat
import laika.parse.ParserContext
import laika.parse.hocon.HoconParsers.Origin
import laika.parse.markup.DocumentParser
import laika.parse.markup.DocumentParser.{ParserError, ParserInput}
import laika.rewrite.TemplateRewriter

/** Performs a parse operation from text markup to a
  * document tree without a subsequent render operation. 
  *
  * In cases where a render operation should follow immediately, it is more 
  * convenient to use a [[laika.api.Transformer]] instead which 
  * combines a parse and a render operation directly.
  *
  * Example for parsing Markdown:
  *
  * {{{
  *  val res: Either[ParserError, Document] = MarkupParser
  *    .of(Markdown)
  *    .using(GitHubFlavor)
  *    .build
  *    .parse("hello *there*)
  * }}}
  *
  * This is a pure API that does not perform any side-effects.
  * For additional options like File and Stream I/O, templating 
  * or parallel processing, use the corresponding builders in 
  * the laika-io module.
  *
  * @author Jens Halm
  */
class MarkupParser (parser: MarkupFormat, val config: OperationConfig, val rewrite: Boolean) { // TODO - 0.12 - remove flag

  /** The file suffixes this parser will recognize
    * as a supported format.
    */
  val fileSuffixes: Set[String] = parser.fileSuffixes
  
  private val docParser = DocumentParser.forMarkup(parser, config.markupExtensions, config.configProvider)

  /** Parses the specified markup string into a document AST structure.
    */
  def parse (input: String): Either[ParserError, Document] = parse(ParserInput(Root, ParserContext(input)))

  /** Parses the specified markup string into a document AST structure.
    * The given (virtual) path will be assigned to the result.
    */
  def parse (input: String, path: Path): Either[ParserError, Document] = parse(ParserInput(path, ParserContext(input)))

  /** Parses the specified markup input into a document AST structure.
    */
  def parse (input: ParserInput): Either[ParserError, Document] = {
    for {
      unresolved     <- docParser(input)
      resolvedConfig <- unresolved.config.resolve(Origin(input.path), 
                          config.baseConfig).left.map(e => ParserError(e.toString, input.path)) // TODO - 0.12 - ConfigError to ParserError
    } yield {
      val resolvedDoc = unresolved.document.copy(config = resolvedConfig)
      if (rewrite) {
        val phase1 = resolvedDoc.rewrite(config.rewriteRulesFor(resolvedDoc))
        phase1.rewrite(TemplateRewriter.rewriteRules(DocumentCursor(phase1)))
      }
      else resolvedDoc
    }
  }

  def parseUnresolved (input: String, path: Path): Either[ParserError, UnresolvedDocument] = 
    parseUnresolved(ParserInput(path, ParserContext(input)))
  
  def parseUnresolved (input: ParserInput): Either[ParserError, UnresolvedDocument] = docParser(input)

}

/** Entry point for building a MarkupParser instance.
  *
  *  @author Jens Halm
  */
object MarkupParser {

  /** Returns a new builder instance for the specified markup format.
    * 
    * The format is usually an object provided by the library
    * or a plugin that is capable of parsing a specific markup
    * format like Markdown or reStructuredText. 
    *
    *  @param format the markup format to use for all parsing operations
    */
  def of (format: MarkupFormat): ParserBuilder = new ParserBuilder(
    format,
    OperationConfig.default.withBundlesFor(format),
    rewrite = true
  )

}
