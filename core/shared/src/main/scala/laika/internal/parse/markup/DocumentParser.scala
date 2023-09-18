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

package laika.internal.parse.markup

import laika.api.bundle.{ ConfigProvider, MarkupExtensions }
import laika.api.config.ConfigParser
import laika.api.errors.ParserError
import laika.api.format.MarkupFormat
import laika.ast.*
import laika.ast.styles.{ StyleDeclaration, StyleDeclarationSet }
import laika.parse.combinator.Parsers
import laika.parse.syntax.*
import laika.parse.{ Parser, SourceCursor }

/** Responsible for creating the top level parsers for text markup and template documents,
  * by combining the parser for the root element with a parser for an (optional) configuration header.
  *
  * @author Jens Halm
  */
private[laika] object DocumentParser {

  private[laika] case class DocumentInput(path: Path, source: SourceCursor)

  private[laika] object DocumentInput {

    def apply(path: Path, input: String): DocumentInput =
      new DocumentInput(path, SourceCursor(input, path))

  }

  private def create[D, R <: ElementContainer[_]](
      rootParser: Parser[R],
      configParser: Parser[ConfigParser]
  )(docFactory: (Path, ConfigParser, R) => D): DocumentInput => Either[ParserError, D] = {

    forParser { path =>
      val configHeader = configParser | Parsers.success(ConfigParser.empty)
      (configHeader ~ rootParser).mapN(docFactory(path, _, _))
    }
  }

  /** Combines the specified markup parsers and extensions and the parser for (optional) configuration
    * headers to create a parser function for an entire text markup document.
    */
  def forMarkup(
      markupParser: MarkupFormat,
      markupExtensions: MarkupExtensions,
      configProvider: ConfigProvider
  ): DocumentInput => Either[ParserError, UnresolvedDocument] = {

    val rootParser = new RootParser(markupParser, markupExtensions).rootElement

    val preProcess: DocumentInput => DocumentInput = input => {
      val raw          = input.source.input
      val preprocessed = markupExtensions.parserHooks.preProcessInput(raw)
      input.copy(source = SourceCursor(preprocessed, input.path))
    }

    preProcess andThen
      forMarkup(rootParser, configProvider) andThen {
        _.map(markupExtensions.parserHooks.postProcessDocument)
      }
  }

  /** Combines the specified parsers for the root element and for (optional) configuration
    * headers to create a parser function for an entire text markup document.
    */
  def forMarkup(
      rootParser: Parser[RootElement],
      configProvider: ConfigProvider
  ): DocumentInput => Either[ParserError, UnresolvedDocument] =
    create(rootParser, configProvider.markupConfigHeader) { (path, config, root) =>
      val fragments = root.collect { case f: DocumentFragment => (f.name, f.root) }.toMap
      UnresolvedDocument(Document(path, root).withFragments(fragments), config)
    }

  /** Combines the specified parsers for the root element and for (optional) configuration
    * headers to create a parser function for an entire template document.
    */
  def forTemplate(
      rootParser: Parser[TemplateRoot],
      configProvider: ConfigProvider
  ): DocumentInput => Either[ParserError, TemplateDocument] =
    create(rootParser, configProvider.templateConfigHeader) { (path, config, root) =>
      TemplateDocument(path, root).withConfig(config)
    }

  /** Builds a document parser for CSS documents based on the specified parser for style declarations.
    */
  def forStyleSheets(
      parser: Parser[Set[StyleDeclaration]]
  ): DocumentInput => Either[ParserError, StyleDeclarationSet] =
    forParser { path => parser.map(res => StyleDeclarationSet.forPath(path, res)) }

  /** A document parser function for the specified parser that is expected to consume
    * all input.
    *
    * The specified function is invoked for each parsed document, so that a parser
    * dependent on the input path can be created.
    */
  def forParser[T](p: Path => Parser[T]): DocumentInput => Either[ParserError, T] = { in =>
    Parsers
      .consumeAll(p(in.path))
      .parse(in.source)
      .toEither
      .left.map(ParserError(_))
  }

}
