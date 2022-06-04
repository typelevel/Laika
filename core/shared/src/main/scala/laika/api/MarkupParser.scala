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

import cats.syntax.all._
import laika.api.builder.{OperationConfig, ParserBuilder}
import laika.ast.Path.Root
import laika.ast.{Document, DocumentCursor, EmbeddedConfigValue, Path, RewritePhase, RewriteRules, UnresolvedDocument}
import laika.config.Origin.DocumentScope
import laika.config.{Config, Origin}
import laika.factory.MarkupFormat
import laika.parse.directive.ConfigHeaderParser
import laika.parse.markup.DocumentParser
import laika.parse.markup.DocumentParser.{DocumentInput, InvalidDocument, ParserError}
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
class MarkupParser (val format: MarkupFormat, val config: OperationConfig) {

  /** The file suffixes this parser will recognize
    * as a supported format.
    */
  val fileSuffixes: Set[String] = format.fileSuffixes
  
  private val fallbackPath: Path = Root / "doc"
  
  private val docParser = DocumentParser.forMarkup(format, config.markupExtensions, config.configProvider)

  /** Parses the specified markup string into a document AST structure.
    */
  def parse (input: String): Either[ParserError, Document] = parse(DocumentInput(fallbackPath, input))

  /** Parses the specified markup string into a document AST structure.
    * The given (virtual) path will be assigned to the result.
    */
  def parse (input: String, path: Path): Either[ParserError, Document] = parse(DocumentInput(path, input))

  /** Parses the specified markup input into a document AST structure.
    */
  def parse (input: DocumentInput): Either[ParserError, Document] = {
    
    def resolveDocument (unresolved: UnresolvedDocument, docConfig: Config): Document = {
      val embeddedConfig = unresolved.document.content.collect {
        case c: EmbeddedConfigValue => (c.key, c.value)
      }
      unresolved.document.copy(
        config = ConfigHeaderParser.merge(docConfig, embeddedConfig)
      )
    }
    
    def rewritePhase (doc: Document, phase: RewritePhase): Either[ParserError, Document] = for {
      rules <- config.rewriteRulesFor(doc, phase).leftMap(ParserError(_, doc.path))
      result <- doc.rewrite(rules).leftMap(ParserError.apply(_, doc.path))
    } yield result
    
    def rewriteDocument (resolvedDoc: Document): Either[ParserError, Document] = for {
      phase1 <- rewritePhase(resolvedDoc, RewritePhase.Build)
      phase2 <- rewritePhase(phase1, RewritePhase.Resolve)
      phase3 <- rewritePhase(phase2, RewritePhase.Render) // TODO - remove this step from MarkupParser
      result <- InvalidDocument
                  .from(phase3, config.failOnMessages)
                  .map(ParserError(_))
                  .toLeft(phase3)
    } yield result
    
    for {
      unresolved     <- docParser(input)
      resolvedConfig <- unresolved.config
                          .resolve(Origin(DocumentScope, input.path), config.baseConfig)
                          .left.map(ParserError(_, input.path))
      resolvedDoc    =  resolveDocument(unresolved, resolvedConfig)
      result         <- rewriteDocument(resolvedDoc)
    } yield result
  }

  def parseUnresolved (input: String): Either[ParserError, UnresolvedDocument] = 
    parseUnresolved(DocumentInput(fallbackPath, input))

  def parseUnresolved (input: String, path: Path): Either[ParserError, UnresolvedDocument] = 
    parseUnresolved(DocumentInput(path, input))

  /** Returns an unresolved document without applying
    * the default rewrite rules and without resolving the configuration header (if present). 
    * 
    * The default rewrite rules resolve link and image references and 
    * rearrange the tree into a hierarchy of sections based on the sequence
    * of header instances found in the document.
    * 
    * The default configuration resolver allows for variable references in the HOCON
    * header of the document to be resolved against values defined in the base configuration.
    * 
    * This low-level hook is rarely used by application code.
    */
  def parseUnresolved (input: DocumentInput): Either[ParserError, UnresolvedDocument] = docParser(input)

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
    OperationConfig.default.withBundlesFor(format)
  )

}
