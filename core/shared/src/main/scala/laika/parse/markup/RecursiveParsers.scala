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

import laika.ast.{Block, Span}
import laika.parse.text.{DelimitedText, PrefixedParser}
import laika.parse.{BlockSource, Parsed, Parser, SourceFragment}

/** Provides parsers for nested blocks, custom block parser implementations
  * can use these without knowing the available span types of the host
  * markup language.
  *
  * Includes the support for nested spans and escaped text.
  *
  * @author Jens Halm
  */
trait RecursiveParsers extends RecursiveSpanParsers {

  /** Lifts the specified text parser to parse the string result as a sequence of blocks.
    *
    * This type of span parser is usually used in block parsers,
    * that support the nesting of other blocks.
    */
  def recursiveBlocks (p: Parser[BlockSource]): Parser[Seq[Block]]

  /** Parses the input into a sequence of blocks based on the available block types of the host markup language.
    *
    * This parser always parses to the end of the input, therefore it is usually applied to
    * the result of a previous parser invocation.
    */
  def recursiveBlocks: RecursiveBlockParser

}

/** Provides parsers for nested spans, custom span parser implementations
  * can use these without knowing the available span types of the host
  * markup language.
  *
  * Includes the support for escaped text.
  */
trait RecursiveSpanParsers extends EscapedTextParsers {

  /** Lifts the specified text parser to parse the string result
    * as a sequence of spans.
    *
    * This type of span parser is usually either used in block parsers,
    * that need to process inline markup after the text for the block
    * has been parsed from the input string or for inline parsers,
    * that need to process inline markup for a span with
    * a delimiter while supporting nested spans. 
    * 
    * In the latter case the passed parser is usually of type `DelimitedText`
    * which is an optimized parser that parses text and recursive spans in one pass.
    * For other kinds of parsers the resulting parser will be a two-pass parser.
    * 
    * The returned parser allows to register parsers for child spans with its `embed` method
    * for parsing types of child spans in addition to the available span types of the
    * host markup language
    */
  def recursiveSpans (parser: DelimitedText): InlineParser[Span, List[Span]]

  /** Lifts the specified text parser to parse the string result
    * as a sequence of spans.
    *
    * This type of span parser is usually either used in block parsers,
    * that need to process inline markup after the text for the block
    * has been parsed from the input string or for inline parsers,
    * that need to process inline markup for a span with
    * a delimiter while supporting nested spans. 
    *
    * In the latter case the passed parser is usually of type `DelimitedText`
    * which is an optimized parser that parses text and recursive spans in one pass.
    * For other kinds of parsers the resulting parser will be a two-pass parser.
    *
    * The returned parser allows to register parsers for child spans with its `embed` method
    * for parsing types of child spans in addition to the available span types of the
    * host markup language
    */
  def recursiveSpans (parser: Parser[SourceFragment]): InlineParser[Span, List[Span]]

  /** Parses the input into a sequence of spans based on the available span types of the host markup language.
    *
    * This parser always parses to the end of the input, therefore it is usually applied to
    * the result of a previous parser invocation.
    */
  def recursiveSpans: RecursiveSpanParser

  /** Provides the syntax highlighter for the specified language if present.
    */
  def getSyntaxHighlighter (language: String): Option[RecursiveSpanParser]
  
}

/** Provides parsers for escaped text, custom span parser implementations
  * can use these without knowing the rules of the host markup language for
  * escaping text.
  */
trait EscapedTextParsers {

  /** Parses an escape sequence (usually a backslash followed by a single char).
    * The characters allowed in an escape sequence might differ between
    * markup languages, therefore custom parser implementations should
    * use this parser as it is always configured correctly for the current
    * host language.
    */
  def escapeSequence: PrefixedParser[String]

  /** Adds support for escape sequences to the specified text parser.
    */
  def escapedText(p: DelimitedText): Parser[String]

  /** Parses a span of text until one of the specified characters is seen
    * (unless it is escaped),
    * while also processing escaped characters, but no other nested
    * spans. The final character is not included in the result.
    */
  def escapedUntil(char: Char, chars: Char*): Parser[String]

}

/** Parses a sequence of blocks based on the available block types of the host markup language.
  *
  * This parser always parses to the end of the input, therefore it is usually applied to
  * the result of a previous parser invocation.
  * 
  * The API is restricted and not the standard `Parser` API to prevent invocations with
  * just a plain string which would lose all position tracking in error messages.
  * Instead both methods expect a full `SourceFragment` instance which carries the context
  * of the root input.
  */
trait RecursiveBlockParser {

  /** Parses the specified input while recovering from all errors by inserting instances
    * of `InvalidBlock` which leaves the error handling to the user's configuration.
    */
  def parseAndRecover (in: SourceFragment): Seq[Block]

  /** Parses the specified input while leaving all error handling to the call-site.
    * Use `parseAndRecover` instead if you want to allow the error handling to be controlled by user configuration.
    */
  def parse (in: SourceFragment): Parsed[Seq[Block]]
  
}

/** Parses a sequence of spans based on the available spans types of the host markup language.
  *
  * This parser always parses to the end of the input, therefore it is usually applied to
  * the result of a previous parser invocation.
  *
  * The API is restricted and not the standard `Parser` API to prevent invocations with
  * just a plain string which would lose all position tracking in error messages.
  * Instead both methods expect a full `SourceFragment` instance which carries the context
  * of the root input.
  */
trait RecursiveSpanParser {

  /** Parses the specified input while recovering from all errors by inserting instances
    * of `InvalidSpan` which leaves the error handling to the user's configuration.
    */
  def parseAndRecover (in: SourceFragment): Seq[Span]

  /** Parses the specified input while leaving all error handling to the call-site.
    * Use `parseAndRecover` instead if you want to allow the error handling to be controlled by user configuration.
    */
  def parse (in: SourceFragment): Parsed[Seq[Span]]
  
}
