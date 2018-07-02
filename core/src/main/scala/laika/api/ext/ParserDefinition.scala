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

package laika.api.ext

import com.typesafe.config.Config
import laika.parse.core.Parser
import laika.parse.core.markup.DocumentParser.InvalidElement
import laika.parse.core.markup.{RecursiveParsers, RecursiveSpanParsers}
import laika.parse.css.Styles.StyleDeclaration
import laika.tree.Elements.{Block, Span}
import laika.tree.Paths.Path
import laika.tree.Templates.TemplateRoot

/**
  * @author Jens Halm
  */
case class ParserDefinition[T] (startChar: Option[Char],
                                parser: Parser[T],
                                isRecursive: Boolean,
                                useInRecursion: Boolean,
                                precedence: Precedence)

trait ParserDefinitionBuilder[T] {

  def createParser (recursiveParsers: RecursiveParsers): ParserDefinition[T]

}

case class ParserDefinitionBuilders(blockParsers: Seq[ParserDefinitionBuilder[Block]] = Nil,
                                    spanParsers: Seq[ParserDefinitionBuilder[Span]] = Nil,
                                    configHeaderParsers: Seq[Path => Parser[Either[InvalidElement, Config]]] = Nil,
                                    templateParser: Option[Parser[TemplateRoot]] = None,
                                    styleSheetParser: Option[Parser[Set[StyleDeclaration]]] = None) {

  def withBase(builders: ParserDefinitionBuilders): ParserDefinitionBuilders =
    ParserDefinitionBuilders(
      blockParsers ++ builders.blockParsers,
      spanParsers ++ builders.spanParsers,
      configHeaderParsers ++ builders.configHeaderParsers,
      templateParser.orElse(builders.templateParser),
      styleSheetParser.orElse(builders.styleSheetParser)
    )

  def markupParsers (recursiveParsers: RecursiveParsers): MarkupParsers =
    MarkupParsers(blockParsers.map(_.createParser(recursiveParsers)), spanParsers.map(_.createParser(recursiveParsers)))

}

case class MarkupParsers (blockParsers: Seq[ParserDefinition[Block]], spanParsers: Seq[ParserDefinition[Span]]) {

  def spanParserMap: Map[Char, Parser[Span]] = spanParsers.map(p => (p.startChar.get, p.parser)).toMap // TODO - handle empty startChar

}

sealed trait Precedence
object Precedence {
  object High extends Precedence
  object Low extends Precedence
}

class SpanParser (startChar: Char) {

  class DefinitionBuilder (parserFactory: RecursiveSpanParsers => Parser[Span],
                           recursive: Boolean) extends ParserDefinitionBuilder[Span] {

    override def createParser (recursiveParsers: RecursiveParsers): ParserDefinition[Span] =
      ParserDefinition(Some(startChar), parserFactory(recursiveParsers), recursive, true, Precedence.High)

  }

  def standalone (parser: Parser[Span]): ParserDefinitionBuilder[Span] = new DefinitionBuilder(_ => parser, false) // TODO - consider different name

  def recursive (factory: RecursiveSpanParsers => Parser[Span]): ParserDefinitionBuilder[Span] =
    new DefinitionBuilder(factory, true)

}

object SpanParser {

  def forStartChar (char: Char): SpanParser = new SpanParser(char)

}

class BlockParser (startChar: Option[Char] = None) {

  class DefinitionBuilder (parserFactory: RecursiveParsers => Parser[Block],
                           recursive: Boolean) extends ParserDefinitionBuilder[Block] {

    override def createParser (recursiveParsers: RecursiveParsers): ParserDefinition[Block] =
      ParserDefinition(startChar, parserFactory(recursiveParsers), recursive, true, Precedence.High)

  }

  def standalone (parser: Parser[Block]): ParserDefinitionBuilder[Block] = new DefinitionBuilder(_ => parser, false) // TODO - consider different name

  def recursive (factory: RecursiveParsers => Parser[Block]): ParserDefinitionBuilder[Block] =
    new DefinitionBuilder(factory, true)

}

object BlockParser {

  def forStartChar (char: Char): BlockParser = new BlockParser(Some(char))

  def withoutStartChar: BlockParser = new BlockParser()

}
