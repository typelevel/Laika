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
class SpanParser (startChar: Char) {

  class DefinitionBuilder (parserFactory: RecursiveSpanParsers => Parser[Span],
                           recursive: Boolean,
                           precedence: Precedence) extends SpanParserBuilder {

    override def createParser (recursiveParsers: RecursiveParsers): SpanParserDefinition =
      SpanParserDefinition(startChar, parserFactory(recursiveParsers), recursive, precedence)

    /** Indicates that this parser should only be applied after all built-in
      * parsers have failed on a specific markup element.
      */
    def withLowPrecedence: DefinitionBuilder = new DefinitionBuilder(parserFactory, recursive, Precedence.Low)

  }

  /** Creates a parser definition for a parser that is independent from the parsers
    * of the host languages.
    */
  def standalone (parser: Parser[Span]): DefinitionBuilder = new DefinitionBuilder(_ => parser, false, Precedence.High)

  /** Creates a parser definition for a parser that depends on the parsers
    * of the host languages for recursively parsing child elements.
    */
  def recursive (factory: RecursiveSpanParsers => Parser[Span]): DefinitionBuilder =
    new DefinitionBuilder(factory, true, Precedence.High)

}

/** Builder API for span parsers.
  */
object SpanParser {

  /** Creates a builder for a span element that starts with the specified character.
    *
    * For span elements a known start character is mandatory as parsing would be too expensive
    * otherwise when the engine has to try a parser on every character.
    */
  def forStartChar (char: Char): SpanParser = new SpanParser(char)

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
  def standalone (parser: Parser[Block]): DefinitionBuilder = new DefinitionBuilder(_ => parser)

  /** Creates a parser definition for a parser that depends on the parsers
    * of the host languages for recursively parsing child elements.
    */
  def recursive (factory: RecursiveParsers => Parser[Block]): DefinitionBuilder =
    new DefinitionBuilder(factory, recursive = true)

  /** Creates a parser definition for a parser that depends on the span parsers
    * of the host languages for recursively parsing child elements.
    */
  def withSpans (factory: RecursiveSpanParsers => Parser[Block]): DefinitionBuilder =
    new DefinitionBuilder(factory)

  /** Creates a parser definition for a parser that depends on the parsers for escape sequences
    * of the host languages for parsing text.
    */
  def withEscapedText (factory: EscapedTextParsers => Parser[Block]): DefinitionBuilder =
    new DefinitionBuilder(factory)

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
