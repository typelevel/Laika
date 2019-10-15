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

package laika.parse.markup

import laika.api.config.{Config}
import laika.ast._
import laika.bundle.MarkupExtensions
import laika.factory.MarkupFormat
import laika.parse.combinator.Parsers
import laika.parse.{Parser, ParserContext}
import laika.parse.directive.ConfigHeaderParser
import laika.parse.hocon.HoconParsers.ConfigValue

/** Responsible for creating the top level parsers for text markup and template documents,
  * by combining the parser for the root element with a parser for an (optional) configuration
  * header.
  *
  * @author Jens Halm
  */
object DocumentParser {
  
  case class ParserInput (path: Path, context: ParserContext)
  
  case class ParserError (message: String, path: Path) extends 
    RuntimeException(s"Error parsing document '$path': $message")

  type ConfigHeaderParser = Path => Parser[Either[InvalidElement, Config]]

  private def create [D, R <: ElementContainer[_]] (rootParser: Parser[R], configHeaderParser: ConfigHeaderParser)
    (docFactory: (Path, Config, Option[InvalidElement], R) => D): ParserInput => Either[ParserError, D] = {

    def extractConfigValues (root: R): Seq[(String, ConfigValue)] = root.collect { 
      case c: EmbeddedConfigValue => (c.key, c.value) 
    }

    forParser { path =>
      configHeaderParser(path) ~ rootParser ^^ { case configHeader ~ root =>
        val config = configHeader.getOrElse(Config.empty)
        val message = configHeader.swap.toOption
        val processedConfig = ConfigHeaderParser.merge(config, extractConfigValues(root))
        docFactory(path, processedConfig, message, root)
      }
    }
  }

  /** Combines the specified markup parsers and extensions and the parser for (optional) configuration
    * headers to create a parser function for an entire text markup document.
    */
  def forMarkup (markupParser: MarkupFormat,
                 markupExtensions: MarkupExtensions,
                 configHeaderParser: ConfigHeaderParser): ParserInput => Either[ParserError, Document] = {

    val rootParser = new RootParser(markupParser, markupExtensions).rootElement

    markupExtensions.parserHooks.preProcessInput andThen
      forMarkup(rootParser, configHeaderParser) andThen
      { _.map(markupExtensions.parserHooks.postProcessDocument) }
  }

  /** Combines the specified parsers for the root element and for (optional) configuration
    * headers to create a parser function for an entire text markup document.
    */
  def forMarkup (rootParser: Parser[RootElement], configHeaderParser: ConfigHeaderParser): ParserInput => Either[ParserError, Document] =

    create(rootParser, configHeaderParser) { (path, config, invalid, root) =>

      val fragments = root.collect { case f: DocumentFragment => (f.name, f.root) }.toMap
      val content = invalid.fold(root) { inv =>
        root.copy(content = inv.asBlock +: root.content)
      }
      Document(path, content, fragments, config)
   }

  /** Combines the specified parsers for the root element and for (optional) configuration
    * headers to create a parser function for an entire template document.
    */
  def forTemplate (rootParser: Parser[TemplateRoot], configHeaderParser: ConfigHeaderParser): ParserInput => Either[ParserError, TemplateDocument] = {

    create(rootParser, configHeaderParser) { (path, config, invalid, root) =>

      val content = invalid.fold(root) { inv =>
        root.copy(content = inv.asTemplateSpan +: root.content)
      }
      TemplateDocument(path, content, config)
    }

  }

  /** Builds a document parser for CSS documents based on the specified parser for style declarations.
    */
  def forStyleSheets (parser: Parser[Set[StyleDeclaration]]): ParserInput => Either[ParserError, StyleDeclarationSet] = 
    forParser { path => parser.map(res => StyleDeclarationSet.forPath(path, res)) }

  /** A document parser function for the specified parser that is expected to consume
    * all input.
    *
    * The specified function is invoked for each parsed document, so that a parser
    * dependent on the input path can be created.
    */
  def forParser[T] (p: Path => Parser[T]): ParserInput => Either[ParserError, T] = { in =>
    Parsers
      .consumeAll(p(in.path))
      .parse(in.context)
      .toEither
      .left.map(ParserError(_, in.path))
  }

}
