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

import laika.api.ext.{BlockParserDefinition, Precedence, SpanParserDefinition}
import laika.parse.core._
import laika.parse.core.text.TextParsers._
import laika.tree.Elements._

/** Base implementation of a root parser, responsible of assembling all the
  * block, inline, text and configuration parsers supported by a text markup language.
  *
  * @author Jens Halm
  */
trait RootParserBase extends DefaultRecursiveParsers {

  /** Parses a full document, delegating most of the work to the `topLevelBlock` parser.
    */
  lazy val rootElement: Parser[RootElement] = opt(blankLines) ~> blockList(topLevelBlock) ^^ RootElement


  protected def toSpanParserMap (mainParsers: Seq[SpanParserDefinition],
                                 extParsers: Seq[SpanParserDefinition]): Map[Char, Parser[Span]] = {
    val (mainHigh, mainLow) = mainParsers.partition(_.precedence == Precedence.High)
    val (extHigh, extLow) = extParsers.partition(_.precedence == Precedence.High)
    (extHigh ++ mainHigh ++ mainLow ++ extLow).groupBy(_.startChar).map {
      case (char, definitions) => (char, definitions.map(_.parser).reduceLeft(_ | _))
    }
  }

  protected def toSortedList (mainParsers: Seq[BlockParserDefinition],
                              extParsers: Seq[BlockParserDefinition]): Seq[BlockParserDefinition] = {
    val (mainHigh, mainLow) = mainParsers.partition(_.precedence == Precedence.High)
    val (extHigh, extLow) = extParsers.partition(_.precedence == Precedence.High)
    extHigh ++ mainHigh ++ mainLow ++ extLow
  }

  protected def toBlockParser (parserDefinitions: Seq[BlockParserDefinition]): Parser[Block] = {
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

  /** Merges the two specified block parsers, trying them in the order they appear in the sequence.
    */
  protected def mergeBlockParsers (parsers: Seq[Parser[Block]]): Parser[Block] =
    if (parsers.isEmpty) failure("No block parsers specified")
    else parsers.reduceLeft(_ | _)

}
