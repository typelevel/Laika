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

import cats.data.NonEmptySet
import laika.ast._
import laika.bundle._
import laika.factory.MarkupFormat
import laika.parse.Parser
import laika.parse.text.PrefixedParser
import laika.parse.builders._

import scala.collection.immutable.TreeSet

/** Responsible of assembling all the block, inline, text and configuration parsers
  * supported by a text markup language.
  *
  * @author Jens Halm
  */
private[laika] class RootParser(markupParser: MarkupFormat, markupExtensions: MarkupExtensions)
    extends DefaultRecursiveParsers {

  private lazy val highlighterMap: Map[String, Parser[Seq[Span]]] =
    markupExtensions.syntaxHighlighters.flatMap { highlighter =>
      highlighter.language.toList.map(lang => (lang.toLowerCase, highlighter.rootParser))
    }.toMap

  def getSyntaxHighlighter(language: String): Option[RecursiveSpanParser] = highlighterMap
    .get(language.toLowerCase)
    .map(new RecursiveSpanParserDelegate(_))

  override lazy val escapedChar: Parser[String] = markupParser.escapedChar

  /** Parses a full document, delegating most of the work to the `topLevelBlock` parser.
    */
  lazy val rootElement: Parser[RootElement] =
    opt(blankLines) ~> blockList(rootBlock).map(RootElement(_))

  private lazy val sortedBlockParsers: Seq[BlockParserDefinition] =
    createAndSortParsers(markupParser.blockParsers, markupExtensions.blockParsers)

  protected lazy val rootBlock: Parser[Block] = merge(
    sortedBlockParsers.filter(_.position != BlockPosition.NestedOnly)
  )

  protected lazy val nestedBlock: Parser[Block] = merge(
    sortedBlockParsers.filter(_.position != BlockPosition.RootOnly)
  )

  protected lazy val fallbackBlock: Parser[Block] = merge(
    sortedBlockParsers.filterNot(_.isRecursive)
  )

  private lazy val rootInterruptions: Parser[Block] = mergeInterruptions(
    sortedBlockParsers.filter(_.position != BlockPosition.NestedOnly)
  )

  private lazy val nestedInterruptions: Parser[Block] = mergeInterruptions(
    sortedBlockParsers.filter(_.position != BlockPosition.RootOnly)
  )

  private lazy val allInterruptions: Parser[Block] = mergeInterruptions(sortedBlockParsers)

  protected lazy val spanParsers: Seq[PrefixedParser[Span]] = {
    val escapedText = SpanParserBuilder.standalone(escapeSequence.map(Text(_))).withLowPrecedence
    val mainParsers = markupParser.spanParsers :+ escapedText

    createAndSortParsers(mainParsers, markupExtensions.spanParsers).map { parserDef =>
      PrefixedParser(parserDef.startChars)(parserDef.parser)
    }
  }

  def blockList(p: => Parser[Block]): Parser[Seq[Block]] =
    markupParser.createBlockListParser(p).map(markupExtensions.parserHooks.postProcessBlocks)

  def paragraphInterruptions(position: BlockPosition = BlockPosition.Any): Parser[Block] =
    position match {
      case BlockPosition.Any        => lazily(allInterruptions)
      case BlockPosition.RootOnly   => lazily(rootInterruptions)
      case BlockPosition.NestedOnly => lazily(nestedInterruptions)
    }

  private def createAndSortParsers[T <: ParserDefinition[_]](
      mainParsers: Seq[ParserBuilder[T]],
      extParsers: Seq[ParserBuilder[T]]
  ): Seq[T] = {

    def createParsers(builders: Seq[ParserBuilder[T]]): (Seq[T], Seq[T]) =
      builders.map(_.createParser(this)).partition(_.precedence == Precedence.High)

    val (mainHigh, mainLow) = createParsers(mainParsers)
    val (extHigh, extLow)   = createParsers(extParsers)
    extHigh ++ mainHigh ++ mainLow ++ extLow
  }

  private def mergeInterruptions(parserDefinitions: Seq[BlockParserDefinition]): Parser[Block] = {
    val interruptions = parserDefinitions.flatMap { parserDef =>
      parserDef.paragraphLineCheck.map { lineCheck =>
        (lineCheck.startChars.toSortedSet.toList, lookAhead(lineCheck) ~> parserDef.parser)
      }
    }
    mergePrefixed(
      interruptions.flatMap { case (chars, parser) => chars.map((_, parser)) },
      "No interruptions available"
    )
  }

  private def mergePrefixed(definitions: Seq[(Char, Parser[Block])], msg: String): Parser[Block] = {
    val groupedPrefixed: Map[Char, Parser[Block]] = definitions
      .groupBy(_._1)
      .map { case (char, tuples) =>
        (char, tuples.map(_._2).reduceLeft(_ | _))
      }

    NonEmptySet
      .fromSet(TreeSet.empty[Char] ++ groupedPrefixed.keySet)
      .fold[Parser[Block]](failure(msg)) { set =>
        val startChars = lookAhead(oneOf(set)).map(_.charAt(0))
        startChars >> groupedPrefixed
      }
  }

  private def merge(parserDefinitions: Seq[BlockParserDefinition]): Parser[Block] = {

    val (prefixed, unprefixed) = parserDefinitions.partition(_.startChars.nonEmpty)

    val prefixedBlock = mergePrefixed(
      prefixed.flatMap { parserDef => parserDef.startChars.toList.map((_, parserDef.parser)) },
      "No decorated block parser available"
    )

    val unprefixedBlock = unprefixed
      .map(_.parser)
      .reduceLeftOption(_ | _)
      .getOrElse(failure("No undecorated block parser available"))

    prefixedBlock | unprefixedBlock
  }

}
