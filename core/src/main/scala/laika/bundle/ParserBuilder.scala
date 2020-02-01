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

package laika.bundle

import laika.ast.{Block, Span}
import laika.parse.Parser
import laika.parse.markup.{EscapedTextParsers, RecursiveParsers, RecursiveSpanParsers}
import laika.parse.text.PrefixedParser
import laika.parse.text.TextParsers._

/** Builds a parser definition lazily by passing the recursive parsers
  * of the host language.
  *
  * This indirection is needed when supplying parser implementations as
  * many parsers recursively parse child elements. A list parser for example
  * needs to be able to detect any other block or span element within a list
  * item. But since it has no way of knowing which extensions a user might
  * have added to the host language, those parsers are supplied from the
  * outside by the parser engine.
  *
  * @author Jens Halm
  */
sealed trait ParserBuilder[T <: ParserDefinition[_]] {

  /** Builds a parser definition lazily by passing the recursive parsers
    * of the host language.
    */
  def createParser (recursiveParsers: RecursiveParsers): T

}

/** Builds a block parser definition lazily by passing the recursive parsers
  * of the host language. */
trait BlockParserBuilder extends ParserBuilder[BlockParserDefinition]

/** Builds a span parser definition lazily by passing the recursive parsers
  * of the host language. */
trait SpanParserBuilder extends ParserBuilder[SpanParserDefinition]

/** Builder API for span parsers that allows to set the parser precedence.
  */
case class SpanParserBuilderOps (parserFactory: RecursiveSpanParsers => PrefixedParser[Span],
                                 recursive: Boolean,
                                 precedence: Precedence) extends SpanParserBuilder {

    def createParser (recursiveParsers: RecursiveParsers): SpanParserDefinition = {
      val p = parserFactory(recursiveParsers)
      SpanParserDefinition(p.startChars, p.underlying, recursive, precedence)
    }

    /** Indicates that this parser should only be applied after all built-in
      * parsers have failed on a specific markup element.
      */
    def withLowPrecedence: SpanParserBuilderOps = copy(precedence = Precedence.Low)

}

/** Builder API for span parsers.
  * 
  * The two entry points are either `recursive` if your parser implementation
  * needs to recursively parse child spans defined by the host language
  * or `standalone` if it doesn't.
  */
object SpanParser {
  
  /** Creates a parser definition for a parser that is independent from the parsers
    * of the host languages.
    */
  def standalone (parser: PrefixedParser[Span]): SpanParserBuilderOps = 
    SpanParserBuilderOps(_ => parser, recursive = false, Precedence.High)

  /** Creates a parser definition for a parser that depends on the parsers
    * of the host languages for recursively parsing child elements.
    */
  def recursive (factory: RecursiveSpanParsers => PrefixedParser[Span]): SpanParserBuilderOps =
    SpanParserBuilderOps(factory, recursive = true, Precedence.High)

  
  @deprecated("use standalone or recursive methods directly", "0.14.0")
  def forStartChar (char: Char): LegacySyntax = new LegacySyntax(char)

  class LegacySyntax (startChar: Char) {
    def standalone (parser: Parser[Span]): SpanParserBuilderOps =
      SpanParserBuilderOps(_ => startChar ~> parser, recursive = false, Precedence.High)
    def recursive (factory: RecursiveSpanParsers => Parser[Span]): SpanParserBuilderOps =
      SpanParserBuilderOps(rec => startChar ~> factory(rec), recursive = true, Precedence.High)
  }
}

/** Builder API for block parsers that allows to set the parser precedence.
  */
case class BlockParserBuilderOps (parserFactory: RecursiveParsers => Parser[Block],
                                  recursive: Boolean = false,
                                  position: BlockPosition = BlockPosition.Any,
                                  precedence: Precedence = Precedence.High) extends BlockParserBuilder {

  def createParser (recursiveParsers: RecursiveParsers): BlockParserDefinition = {
    val p = parserFactory(recursiveParsers)
    val startChars = p match {
      case pp: PrefixedParser[_] => pp.startChars.toSortedSet
      case _                     => Set.empty[Char]
    }
    BlockParserDefinition(startChars, p, recursive, position, precedence)
  }

  /** Indicates that this parser should only be applied after all built-in
    * parsers have failed on a specific markup element.
    */
  def withLowPrecedence: BlockParserBuilderOps = copy(precedence = Precedence.Low)

  /** Indicates that this parser should only be applied for top level block items,
    * but not for blocks nested within other blocks.
    */
  def rootOnly: BlockParserBuilderOps = copy(position = BlockPosition.RootOnly)

  /** Indicates that this parser should only be applied for blocks nested within other blocks.
    */
  def nestedOnly: BlockParserBuilderOps = copy(position = BlockPosition.NestedOnly)

}

/** Builder API for block parsers.
  * 
  * The entry points provide access to the parsers for child blocks (`recursive`),
  * child spans (`withSpans`) or escape sequences (`withEscapedText`). These
  * are methods with decreasing power, as the parser for recursive blocks does
  * also provide the span parsers.
  * 
  * If your parser implementation is completely independent from the host markup
  * language you can use the `standalone` method.
  */
object BlockParser {

  /** Creates a parser definition for a parser that is independent from the parsers
    * of the host languages.
    */
  def standalone (parser: Parser[Block]): BlockParserBuilderOps = BlockParserBuilderOps(_ => parser)

  /** Creates a parser definition for a parser that depends on the parsers
    * of the host languages for recursively parsing child blocks.
    */
  def recursive (factory: RecursiveParsers => Parser[Block]): BlockParserBuilderOps =
    BlockParserBuilderOps(factory, recursive = true)

  /** Creates a parser definition for a parser that depends on the span parsers
    * of the host languages for recursively parsing spans inside block elements.
    */
  def withSpans (factory: RecursiveSpanParsers => Parser[Block]): BlockParserBuilderOps =
    BlockParserBuilderOps(factory)

  /** Creates a parser definition for a parser that depends on the parsers for escape sequences
    * of the host languages for parsing text.
    */
  def withEscapedText (factory: EscapedTextParsers => Parser[Block]): BlockParserBuilderOps =
    BlockParserBuilderOps(factory)

  @deprecated("use standalone, recursive, withSpans, withEscapedText methods directly", "0.14.0")
  def forStartChar (char: Char): LegacySyntax = new LegacySyntax(Some(char))

  @deprecated("use standalone, recursive, withSpans, withEscapedText methods directly", "0.14.0")
  def withoutStartChar: LegacySyntax = new LegacySyntax()

  class LegacySyntax (startChar: Option[Char] = None) {
    private def createParser(p: Parser[Block]): Parser[Block] =
      startChar.fold(p){ c => c ~> p }
    def standalone (parser: Parser[Block]): BlockParserBuilderOps =
      BlockParserBuilderOps(_ => createParser(parser))
    def recursive (factory: RecursiveParsers => Parser[Block]): BlockParserBuilderOps =
      BlockParserBuilderOps(rec => createParser(factory(rec)), recursive = true)
    def withSpans (factory: RecursiveSpanParsers => Parser[Block]): BlockParserBuilderOps =
      BlockParserBuilderOps(rec => createParser(factory(rec)))
    def withEscapedText (factory: EscapedTextParsers => Parser[Block]): BlockParserBuilderOps =
      BlockParserBuilderOps(rec => createParser(factory(rec)))
  }
}
