/*
 * Copyright 2013-2017 the original author or authors.
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

import laika.api.ext._
import laika.factory.MarkupParser
import laika.parse.core._
import laika.parse.core.text.TextParsers._
import laika.tree.Elements._

/** Responsible of assembling all the block, inline, text and configuration parsers
  * supported by a text markup language.
  *
  * @author Jens Halm
  */
class RootParser (markupParser: MarkupParser, markupExtensions: MarkupExtensions) extends DefaultRecursiveParsers {

  /** Parses a full document, delegating most of the work to the `topLevelBlock` parser.
    */
  lazy val rootElement: Parser[RootElement] = opt(blankLines) ~> blockList(rootBlock) ^^ RootElement

  private lazy val sortedBlockParsers: Seq[BlockParserDefinition] =
    createParsers(markupParser.blockParsers, markupExtensions.blockParsers)

  protected lazy val rootBlock     = merge(sortedBlockParsers.filter(_.position != BlockPosition.NestedOnly))
  protected lazy val nestedBlock   = merge(sortedBlockParsers.filter(_.position != BlockPosition.RootOnly))
  protected lazy val fallbackBlock = merge(sortedBlockParsers.filterNot(_.isRecursive))

  protected lazy val spanParsers: Map[Char,Parser[Span]] = {
    val escapedText = SpanParser.forStartChar('\\').standalone(markupParser.escapedChar.map(Text(_))).withLowPrecedence

    createParsers(markupParser.spanParsers :+ escapedText, markupExtensions.spanParsers).groupBy(_.startChar).map {
      case (char, definitions) => (char, definitions.map(_.parser).reduceLeft(_ | _))
    }
  }

  def blockList (p: => Parser[Block]): Parser[Seq[Block]] =
    markupParser.createBlockListParser(p) ^^ markupExtensions.rootParserHooks.postProcessBlocks

  private def createParsers[T <: ParserDefinition] (mainParsers: Seq[ParserBuilder[T]],
                                                    extParsers: Seq[ParserBuilder[T]]): Seq[T] = {

    def createParsers (builders: Seq[ParserBuilder[T]]): (Seq[T],Seq[T]) =
      builders.map(_.createParser(this)).partition(_.precedence == Precedence.High)

    val (mainHigh, mainLow) = createParsers(mainParsers)
    val (extHigh, extLow) = createParsers(extParsers)
    extHigh ++ mainHigh ++ mainLow ++ extLow
  }

  private def merge (parserDefinitions: Seq[BlockParserDefinition]): Parser[Block] = {
    val grouped = parserDefinitions.groupBy(_.startChar).map {
      case (char, definitions) => (char, definitions.map(_.parser).reduceLeft(_ | _))
    }
    val decoratedBlockParserMap = grouped.collect {
      case (Some(char), definition) => (char, definition)
    }
    val undecoratedBlock = grouped.getOrElse(None, failure("No undecorated block parser available"))
    val startChars = anyOf(decoratedBlockParserMap.keySet.toSeq:_*).take(1) ^^ (_.charAt(0))
    val decoratedBlock = startChars >> decoratedBlockParserMap
    decoratedBlock | undecoratedBlock
  }

}
