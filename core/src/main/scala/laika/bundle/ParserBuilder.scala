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

import cats.data.NonEmptySet
import laika.ast.{Block, Span}
import laika.parse.Parser
import laika.parse.markup.{EscapedTextParsers, RecursiveParsers, RecursiveSpanParsers}
import laika.parse.text.PrefixedParser

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

/** Builder API for span parsers.
  */
class SpanParserBuilderOps (parserFactory: RecursiveSpanParsers => PrefixedParser[Span],
                            recursive: Boolean,
                            precedence: Precedence) extends SpanParserBuilder {

    override def createParser (recursiveParsers: RecursiveParsers): SpanParserDefinition = {
      val p = parserFactory(recursiveParsers)
      SpanParserDefinition(p.startChars, p.underlying, recursive, precedence)
    }

    /** Indicates that this parser should only be applied after all built-in
      * parsers have failed on a specific markup element.
      */
    def withLowPrecedence: SpanParserBuilderOps = 
      new SpanParserBuilderOps(parserFactory, recursive, Precedence.Low)

}

/** Builder API for span parsers.
  */
object SpanParser {
  
  class LegacySyntax (startChars: NonEmptySet[Char]) {
    def standalone (parser: Parser[Span]): SpanParserBuilderOps = 
      new SpanParserBuilderOps(_ => PrefixedParser(parser, startChars), false, Precedence.High)
    def recursive (factory: RecursiveSpanParsers => Parser[Span]): SpanParserBuilderOps =
      new SpanParserBuilderOps(rec => PrefixedParser(factory(rec), startChars), true, Precedence.High)
  }
  
  import cats.implicits._

  /** Creates a parser definition for a parser that is independent from the parsers
    * of the host languages.
    */
  def standalone (parser: PrefixedParser[Span]): SpanParserBuilderOps = 
    new SpanParserBuilderOps(_ => parser, false, Precedence.High)

  /** Creates a parser definition for a parser that depends on the parsers
    * of the host languages for recursively parsing child elements.
    */
  def recursive (factory: RecursiveSpanParsers => PrefixedParser[Span]): SpanParserBuilderOps =
    new SpanParserBuilderOps(factory, true, Precedence.High)

  // TODO - deprecate
  def forStartChar (char: Char): LegacySyntax = new LegacySyntax(NonEmptySet.one(char))

}

/** Builder API for block parsers.
  */
class BlockParser (startChar: Option[Char] = None) {

  case class DefinitionBuilder (parserFactory: RecursiveParsers => Parser[Block],
                                recursive: Boolean = false,
                                position: BlockPosition = BlockPosition.Any,
                                precedence: Precedence = Precedence.High) extends BlockParserBuilder {

    override def createParser (recursiveParsers: RecursiveParsers): BlockParserDefinition =
      BlockParserDefinition(startChar, parserFactory(recursiveParsers), recursive, position, precedence)

    /** Indicates that this parser should only be applied after all built-in
      * parsers have failed on a specific markup element.
      */
    def withLowPrecedence: DefinitionBuilder = copy(precedence = Precedence.Low)

    /** Indicates that this parser should only be applied for top level block items,
      * but not for blocks nested within other blocks.
      */
    def rootOnly: DefinitionBuilder = copy(position = BlockPosition.RootOnly)

    /** Indicates that this parser should only be applied for blocks nested within other blocks.
      */
    def nestedOnly: DefinitionBuilder = copy(position = BlockPosition.NestedOnly)

  }

  /** Creates a parser definition for a parser that is independent from the parsers
    * of the host languages.
    */
  def standalone (parser: Parser[Block]): DefinitionBuilder = DefinitionBuilder(_ => parser)

  /** Creates a parser definition for a parser that depends on the parsers
    * of the host languages for recursively parsing child elements.
    */
  def recursive (factory: RecursiveParsers => Parser[Block]): DefinitionBuilder =
    DefinitionBuilder(factory, recursive = true)

  /** Creates a parser definition for a parser that depends on the span parsers
    * of the host languages for recursively parsing child elements.
    */
  def withSpans (factory: RecursiveSpanParsers => Parser[Block]): DefinitionBuilder =
    DefinitionBuilder(factory)

  /** Creates a parser definition for a parser that depends on the parsers for escape sequences
    * of the host languages for parsing text.
    */
  def withEscapedText (factory: EscapedTextParsers => Parser[Block]): DefinitionBuilder =
    DefinitionBuilder(factory)

}

/** Builder API for block parsers.
  */
object BlockParser {

  /** Creates a builder for a block element that starts with the specified character.
    *
    * For block elements a known start character is optional and can be used by the host
    * language for performance optimizations.
    */
  def forStartChar (char: Char): BlockParser = new BlockParser(Some(char))

  /** Creates a builder for a block element that does not start with a specific character.
    *
    * For parsers that rely on pattern detection or other mechanisms to recognize a matching block,
    * this method allows to avoid the definition of a distinct start character by occurring a little
    * performance penalty.
    */
  def withoutStartChar: BlockParser = new BlockParser()

}
