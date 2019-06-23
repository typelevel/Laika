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
import laika.ast.{Document, Path}
import laika.ast.Path.Root
import laika.api.builder.OperationConfig
import laika.factory.MarkupFormat
import laika.parse.ParserContext
import laika.parse.markup.DocumentParser
import laika.parse.markup.DocumentParser.{ParserError, ParserInput}

class MarkupParser (parser: MarkupFormat, val config: OperationConfig, val rewrite: Boolean) {

  val fileSuffixes: Set[String] = parser.fileSuffixes
  
  private val docParser = DocumentParser.forMarkup(parser, config.markupExtensions, config.configHeaderParser)

  def parse (input: String): Either[ParserError, Document] = parse(ParserInput(Root, ParserContext(input)))

  def parse (input: String, path: Path): Either[ParserError, Document] = parse(ParserInput(path, ParserContext(input)))
  
  def parse (input: ParserInput): Either[ParserError, Document] = {
    val res = docParser(input)
    if (rewrite) res.map(doc => doc.rewrite(config.rewriteRulesFor(doc)))
    else res
  }

}

/** Serves as an entry point for building a MarkupParser instance.
  *
  *  @author Jens Halm
  */
object MarkupParser {

  /** Returns a new builder instance for the specified markup format.
    *  The format is usually an object provided by the library
    *  or a plugin that is capable of parsing a specific markup
    *  format like Markdown or reStructuredText. 
    *
    *  @param format the markup format to use for all subsequent operations
    */
  def of (format: MarkupFormat): ParserBuilder = new ParserBuilder(
    format,
    OperationConfig.default.withBundlesFor(format),
    rewrite = true
  )

}
