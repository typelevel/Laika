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

import com.typesafe.config.{Config, ConfigFactory}
import laika.ast._
import laika.bundle.MarkupExtensions
import laika.factory.MarkupParser
import laika.io.Input
import laika.parse.Parser
import laika.parse.directive.ConfigHeaderParser
import laika.parse.text.TextParsers.unsafeParserFunction

/** Responsible for creating the top level parsers for text markup and template documents,
  * by combining the parser for the root element with a parser for an (optional) configuration
  * header.
  *
  * @author Jens Halm
  */
object DocumentParser {

  type ConfigHeaderParser = Path => Parser[Either[InvalidElement, Config]]

  private def create [D, R <: ElementContainer[_,_]](rootParser: Parser[R],
                                             configHeaderParser: ConfigHeaderParser)
                                            (docFactory: (Path, Config, Option[InvalidElement], R) => D): Input => D = { input =>

    def extractConfigValues (root: R): Map[String,AnyRef] =
      root.collect { case c: ConfigValue => (c.name, c.value) }.toMap

    val parser = configHeaderParser(input.path) ~ rootParser ^^ { case configHeader ~ root =>
      val config = configHeader.right.getOrElse(ConfigFactory.empty)
      val message = configHeader.left.toOption
      val processedConfig = ConfigHeaderParser.merge(config, extractConfigValues(root))
      docFactory(input.path, processedConfig, message, root)
    }

    unsafeParserFunction(parser)(input.asParserInput)
  }

  /** Combines the specified markup parsers and extensions and the parser for (optional) configuration
    * headers to create a parser function for an entire text markup document.
    */
  def forMarkup (markupParser: MarkupParser,
                 markupExtensions: MarkupExtensions,
                 configHeaderParser: ConfigHeaderParser): Input => Document = {

    val rootParser = new RootParser(markupParser, markupExtensions).rootElement

    markupExtensions.parserHooks.preProcessInput andThen
      forMarkup(rootParser, configHeaderParser) andThen
      markupExtensions.parserHooks.postProcessDocument
  }

  /** Combines the specified parsers for the root element and for (optional) configuration
    * headers to create a parser function for an entire text markup document.
    */
  def forMarkup (rootParser: Parser[RootElement], configHeaderParser: ConfigHeaderParser): Input => Document =

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
  def forTemplate (rootParser: Parser[TemplateRoot], configHeaderParser: ConfigHeaderParser): Input => TemplateDocument = {

    create(rootParser, configHeaderParser) { (path, config, invalid, root) =>

      val content = invalid.fold(root) { inv =>
        root.copy(content = inv.asTemplateSpan +: root.content)
      }
      TemplateDocument(path, content, config)
    }

  }


}
