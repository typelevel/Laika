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

package laika.api.bundle

import laika.ast.{ Block, Span }
import laika.parse.Parser
import laika.parse.markup.{ EscapedTextParsers, RecursiveParsers, RecursiveSpanParsers }
import laika.parse.text.PrefixedParser

/** Base trait for `SpanParserBuilder` and `BlockParserBuilder` APIs.
  */
sealed trait ParserBuilder[T <: ParserDefinition[_]] {

  /** Builds a block parser definition lazily by passing the recursive parsers
    * of the host language.
    *
    * This indirection is needed when supplying parser implementations as
    * many parsers recursively parse child elements. A list parser for example
    * needs to be able to detect any other block or span element within a list
    * item. But since it has no way of knowing which extensions a user might
    * have added to the host language, those parsers are supplied from the
    * outside by the parser engine.
    */
  def createParser(recursiveParsers: RecursiveParsers): T

}

/** Builder API for span parsers.
  */
class SpanParserBuilder private (
    parserFactory: RecursiveSpanParsers => PrefixedParser[Span],
    recursive: Boolean,
    precedence: Precedence
) extends ParserBuilder[SpanParserDefinition] {

  def createParser(recursiveParsers: RecursiveParsers): SpanParserDefinition = {
    val p = parserFactory(recursiveParsers)
    new SpanParserDefinition(p.startChars, p.underlying, recursive, precedence)
  }

  /** Indicates that this parser should only be applied after all built-in
    * parsers have failed on a specific markup element.
    */
  def withLowPrecedence: SpanParserBuilder =
    new SpanParserBuilder(parserFactory, recursive, Precedence.Low)

}

/** Entry points for the builder API for span parsers.
  *
  * The two entry points are either `recursive` if your parser implementation
  * needs to recursively parse child spans defined by the host language
  * or `standalone` if it doesn't.
  */
object SpanParserBuilder {

  /** Creates a parser definition for a parser that is independent from the parsers
    * of the host languages.
    */
  def standalone(parser: PrefixedParser[Span]): SpanParserBuilder =
    new SpanParserBuilder(_ => parser, recursive = false, Precedence.High)

  /** Creates a parser definition for a parser that depends on the parsers
    * of the host languages for recursively parsing child elements.
    */
  def recursive(factory: RecursiveSpanParsers => PrefixedParser[Span]): SpanParserBuilder =
    new SpanParserBuilder(factory, recursive = true, Precedence.High)

}

/** Builder API for block parsers.
  *
  * Builds a span parser definition lazily by passing the recursive parsers
  * of the host language.
  *
  * This indirection is needed when supplying parser implementations as
  * many parsers recursively parse child elements. A list parser for example
  * needs to be able to detect any other block or span element within a list
  * item. But since it has no way of knowing which extensions a user might
  * have added to the host language, those parsers are supplied from the
  * outside by the parser engine.
  */
class BlockParserBuilder private (
    parserFactory: RecursiveParsers => Parser[Block],
    recursive: Boolean = false,
    position: BlockPosition = BlockPosition.Any,
    precedence: Precedence = Precedence.High,
    paragraphLineCheck: Option[PrefixedParser[Any]] = None
) extends ParserBuilder[BlockParserDefinition] {

  def createParser(recursiveParsers: RecursiveParsers): BlockParserDefinition = {
    val p          = parserFactory(recursiveParsers)
    val startChars = p match {
      case pp: PrefixedParser[_] => pp.startChars.toSortedSet
      case _                     => Set.empty[Char]
    }
    new BlockParserDefinition(startChars, p, recursive, position, precedence, paragraphLineCheck)
  }

  /** Indicates that this parser should only be applied after all built-in
    * parsers have failed on a specific markup element.
    */
  def withLowPrecedence: BlockParserBuilder =
    new BlockParserBuilder(parserFactory, recursive, position, Precedence.Low, paragraphLineCheck)

  /** Indicates that this parser should only be applied for top level block items,
    * but not for blocks nested within other blocks.
    */
  def rootOnly: BlockParserBuilder =
    new BlockParserBuilder(
      parserFactory,
      recursive,
      BlockPosition.RootOnly,
      precedence,
      paragraphLineCheck
    )

  /** Indicates that this parser should only be applied for blocks nested within other blocks.
    */
  def nestedOnly: BlockParserBuilder =
    new BlockParserBuilder(
      parserFactory,
      recursive,
      BlockPosition.NestedOnly,
      precedence,
      paragraphLineCheck
    )

  /** Provides a test for the start of each line in plain paragraphs that indicates whether the line might
    * be the start of a block identified by this parser.
    * Without providing such a test the type of block produced by this parser can only occur after a blank line.
    */
  def interruptsParagraphWith(lineCheck: PrefixedParser[Any]): BlockParserBuilder =
    new BlockParserBuilder(
      parserFactory,
      recursive,
      position,
      precedence,
      Some(lineCheck)
    )

}

/** Entry points for the builder API for block parsers.
  *
  * The entry points provide access to the parsers for child blocks (`recursive`),
  * child spans (`withSpans`) or escape sequences (`withEscapedText`). These
  * are methods with decreasing power, as the parser for recursive blocks does
  * also provide the span parsers.
  *
  * If your parser implementation is completely independent from the host markup
  * language you can use the `standalone` method.
  */
object BlockParserBuilder {

  /** Creates a parser definition for a parser that is independent from the parsers
    * of the host languages.
    */
  def standalone(parser: Parser[Block]): BlockParserBuilder = new BlockParserBuilder(_ => parser)

  /** Creates a parser definition for a parser that depends on the parsers
    * of the host languages for recursively parsing child blocks.
    */
  def recursive(factory: RecursiveParsers => Parser[Block]): BlockParserBuilder =
    new BlockParserBuilder(factory, recursive = true)

  /** Creates a parser definition for a parser that depends on the span parsers
    * of the host languages for recursively parsing spans inside block elements.
    */
  def withSpans(factory: RecursiveSpanParsers => Parser[Block]): BlockParserBuilder =
    new BlockParserBuilder(factory)

  /** Creates a parser definition for a parser that depends on the parsers for escape sequences
    * of the host languages for parsing text.
    */
  def withEscapedText(factory: EscapedTextParsers => Parser[Block]): BlockParserBuilder =
    new BlockParserBuilder(factory)

}
