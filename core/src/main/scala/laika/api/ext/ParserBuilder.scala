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

import laika.parse.core.Parser
import laika.parse.core.markup.{EscapedTextParsers, RecursiveParsers, RecursiveSpanParsers}
import laika.tree.Elements.{Block, Span}

/**
  * @author Jens Halm
  */
sealed trait ParserBuilder[T <: ParserDefinition[_]] {

  def createParser (recursiveParsers: RecursiveParsers): T

}

trait BlockParserBuilder extends ParserBuilder[BlockParserDefinition]
trait SpanParserBuilder extends ParserBuilder[SpanParserDefinition]

class SpanParser (startChar: Char) {

  class DefinitionBuilder (parserFactory: RecursiveSpanParsers => Parser[Span],
                           recursive: Boolean,
                           precedence: Precedence) extends SpanParserBuilder {

    override def createParser (recursiveParsers: RecursiveParsers): SpanParserDefinition =
      SpanParserDefinition(startChar, parserFactory(recursiveParsers), recursive, precedence)

    def withLowPrecedence: DefinitionBuilder = new DefinitionBuilder(parserFactory, recursive, Precedence.Low)

  }

  def standalone (parser: Parser[Span]): DefinitionBuilder = new DefinitionBuilder(_ => parser, false, Precedence.High)

  def recursive (factory: RecursiveSpanParsers => Parser[Span]): DefinitionBuilder =
    new DefinitionBuilder(factory, true, Precedence.High)

}

object SpanParser {

  def forStartChar (char: Char): SpanParser = new SpanParser(char)

}

class BlockParser (startChar: Option[Char] = None) {

  case class DefinitionBuilder (parserFactory: RecursiveParsers => Parser[Block],
                                recursive: Boolean = false,
                                position: BlockPosition = BlockPosition.Any,
                                precedence: Precedence = Precedence.High) extends BlockParserBuilder {

    override def createParser (recursiveParsers: RecursiveParsers): BlockParserDefinition =
      BlockParserDefinition(startChar, parserFactory(recursiveParsers), recursive, position, precedence)

    def withLowPrecedence: DefinitionBuilder = copy(precedence = Precedence.Low)

    def rootOnly: DefinitionBuilder = copy(position = BlockPosition.RootOnly)
    def nestedOnly: DefinitionBuilder = copy(position = BlockPosition.NestedOnly)

  }

  def standalone (parser: Parser[Block]): DefinitionBuilder = new DefinitionBuilder(_ => parser)

  def recursive (factory: RecursiveParsers => Parser[Block]): DefinitionBuilder =
    new DefinitionBuilder(factory, recursive = true)

  def withSpans (factory: RecursiveSpanParsers => Parser[Block]): DefinitionBuilder =
    new DefinitionBuilder(factory)

  def withEscapedText (factory: EscapedTextParsers => Parser[Block]): DefinitionBuilder =
    new DefinitionBuilder(factory)

}

object BlockParser {

  def forStartChar (char: Char): BlockParser = new BlockParser(Some(char))

  def withoutStartChar: BlockParser = new BlockParser()

}
