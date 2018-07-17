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

package laika.parse.core.markup

import com.typesafe.config.{Config, ConfigFactory}
import laika.api.ext.{BlockParserBuilder, ParserBuilder, SpanParserBuilder}
import laika.directive.ConfigHeaderParser
import laika.io.Input
import laika.parse.core.Parser
import laika.parse.core.text.TextParsers.unsafeParserFunction
import laika.rewrite.TreeUtil
import laika.tree.Documents.{Document, TemplateDocument}
import laika.tree.Elements._
import laika.tree.Paths.Path
import laika.tree.Templates.{TemplateElement, TemplateRoot, TemplateSpan, TemplateString}
import laika.util.~

/**
  * @author Jens Halm
  */
object DocumentParser {

  type ConfigHeaderParser = Path => Parser[Either[InvalidElement, Config]]

  private def create [D, R <: ElementContainer[_,_]](rootParser: Parser[R],
                                             configHeaderParser: ConfigHeaderParser)
                                            (docFactory: (Path, Config, Option[InvalidElement], R) => D): Input => D = { input =>

    val parser = configHeaderParser(input.path) ~ rootParser ^^ { case configHeader ~ root =>
      val config = configHeader.right.getOrElse(ConfigFactory.empty)
      val message = configHeader.left.toOption
      val processedConfig = ConfigHeaderParser.merge(config, TreeUtil.extractConfigValues(root))
      docFactory(input.path, processedConfig, message, root)
    }

    unsafeParserFunction(parser)(input.asParserInput)
  }

  case class InvalidElement (message: SystemMessage, source: String) {

    def asBlock: InvalidBlock = InvalidBlock(message, LiteralBlock(source))

    def asSpan: InvalidSpan = InvalidSpan(message, Text(source))

    def asTemplateSpan: TemplateSpan = TemplateElement(InvalidSpan(message, TemplateString(source)))

  }// TODO - move to elements, implement traits (Element? Invalid?) {

  // TODO - use this shortcut in other parts, too
  object InvalidElement {
    def apply (message: String, source: String): InvalidElement = apply(SystemMessage(laika.tree.Elements.Error, message), source)
  }

  def forMarkup (rootParser: Parser[RootElement], configHeaderParser: ConfigHeaderParser): Input => Document =

    create(rootParser, configHeaderParser) { (path, config, invalid, root) =>

      val fragments = TreeUtil.extractFragments(root.content)
      val content = invalid.fold(root) { inv =>
        root.copy(content = inv.asBlock +: root.content)
      }
      Document(path, content, fragments, config)
   }

  def forTemplate (rootParser: Parser[TemplateRoot], configHeaderParser: ConfigHeaderParser): Input => TemplateDocument = {

    create(rootParser, configHeaderParser) { (path, config, invalid, root) =>

      val content = invalid.fold(root) { inv =>
        root.copy(content = inv.asTemplateSpan +: root.content)
      }
      TemplateDocument(path, content, config)
    }

  }


}

trait MarkupParser { // replacing ParserFactory and RootParserBase

  // stuff moving over from ParserFactory, fileSuffixes, extensions

  def blockParsers: Seq[BlockParserBuilder]

  def spanParsers: Seq[SpanParserBuilder]

  // this one is tricky as it is already needed in DefaultRecursiveParsers
  def createBlockListParser (parser: => Parser[Block]): Parser[List[Block]] = parser.rep // (p <~ opt(blankLines))*
  // rst -> parser depends on prev block result, md -> prepend insignificant spaces

  def postProcessBlocks (blocks: Seq[Block]): Seq[Block] = blocks
  // rst - simplified id processor

  def postProcessDocument (doc: Document): Document = doc
  // rst - insert text roles, etc.

  def preProcessInput (input: Input): Input = input
  // rst - process whitespace

}
