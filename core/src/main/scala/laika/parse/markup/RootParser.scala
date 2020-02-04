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

package laika.parse.markup

import cats.implicits._
import cats.data.NonEmptySet
import laika.ast._
import laika.bundle._
import laika.factory.MarkupFormat
import laika.parse.Parser
import laika.parse.text.PrefixedParser
import laika.parse.text.TextParsers._

import scala.collection.immutable.TreeSet

/** Responsible of assembling all the block, inline, text and configuration parsers
  * supported by a text markup language.
  *
  * @author Jens Halm
  */
class RootParser (markupParser: MarkupFormat, markupExtensions: MarkupExtensions) extends DefaultRecursiveParsers {

  private lazy val highlighterMap: Map[String, Parser[Seq[Span]]] = 
    markupExtensions.syntaxHighlighters.flatMap { highlighter =>
      highlighter.language.toList.map(lang => (lang.toLowerCase, highlighter.rootParser))
    }.toMap

  def getSyntaxHighlighter (language: String): Option[Parser[Seq[Span]]] = highlighterMap.get(language.toLowerCase)

  override lazy val escapedChar: Parser[String] = markupParser.escapedChar
  
  /** Parses a full document, delegating most of the work to the `topLevelBlock` parser.
    */
  lazy val rootElement: Parser[RootElement] = opt(blankLines) ~> blockList(rootBlock) ^^ { RootElement(_) }

  private lazy val sortedBlockParsers: Seq[BlockParserDefinition] =
    createAndSortParsers(markupParser.blockParsers, markupExtensions.blockParsers)

  protected lazy val rootBlock     = merge(sortedBlockParsers.filter(_.position != BlockPosition.NestedOnly))
  protected lazy val nestedBlock   = merge(sortedBlockParsers.filter(_.position != BlockPosition.RootOnly))
  protected lazy val fallbackBlock = merge(sortedBlockParsers.filterNot(_.isRecursive))

  protected lazy val spanParsers: Seq[PrefixedParser[Span]] = {
    val escapedText = SpanParser.standalone(escapeSequence.map(Text(_))).withLowPrecedence
    val mainParsers = markupParser.spanParsers :+ escapedText
    
    createAndSortParsers(mainParsers, markupExtensions.spanParsers).map { parserDef =>
      PrefixedParser(parserDef.startChars)(parserDef.parser)
    }
  }

  def blockList (p: => Parser[Block]): Parser[Seq[Block]] =
    markupParser.createBlockListParser(p) ^^ markupExtensions.parserHooks.postProcessBlocks

  private def createAndSortParsers[T <: ParserDefinition[_]] (mainParsers: Seq[ParserBuilder[T]],
                                                              extParsers: Seq[ParserBuilder[T]]): Seq[T] = {

    def createParsers (builders: Seq[ParserBuilder[T]]): (Seq[T],Seq[T]) =
      builders.map(_.createParser(this)).partition(_.precedence == Precedence.High)

    val (mainHigh, mainLow) = createParsers(mainParsers)
    val (extHigh, extLow) = createParsers(extParsers)
    extHigh ++ mainHigh ++ mainLow ++ extLow
  }

  private def merge (parserDefinitions: Seq[BlockParserDefinition]): Parser[Block] = {
    
    val (prefixed, unprefixed) = parserDefinitions.partition(_.startChars.nonEmpty)
    
    val groupedPrefixed: Map[Char, Parser[Block]] = prefixed
      .flatMap { parserDef =>
        parserDef.startChars.toList.map(c => (c, parserDef))
      }
      .groupBy(_._1)
      .map {
        case (char, definitions) => (char, definitions.map(_._2.parser).reduceLeft(_ | _))
      }
    
    val unprefixedBlock = unprefixed
      .map(_.parser)
      .reduceLeftOption(_ | _)
      .getOrElse(failure("No undecorated block parser available"))
    
    val prefixedBlock = NonEmptySet
      .fromSet(TreeSet.empty[Char] ++ groupedPrefixed.keySet)
      .fold[Parser[Block]](failure("No decorated block parser available")) { set =>
        val startChars = lookAhead(anyOf(set).take(1)).map(_.charAt(0))
        startChars >> groupedPrefixed
      } 
    
    prefixedBlock | unprefixedBlock
  }

}
