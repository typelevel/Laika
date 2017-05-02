/*
 * Copyright 2013-2016 the original author or authors.
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

package laika.parse

import laika.parse.core.markup.{EndDelimiter, InlineDelimiter, NestedDelimiter}
import laika.parse.core.text.{DelimitedBy, DelimitedText}
import laika.parse.core._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import laika.tree.Elements.Span
import laika.tree.Elements.Text
import laika.tree.Elements.NoOpt
  
/** A generic base trait for inline parsers. Provides base parsers that abstract
 *  aspects of inline parsing common to most lightweight markup languages.
 *  
 *  It contains helper parsers that abstract the typical logic required for parsing
 *  nested spans. In many cases a parser has to recognize the end of the span as well
 *  as potentially the start of a nested spans. These two concerns are usually unrelated.
 *  
 *  This trait offers helpers that simplify creating these types of parsers and also
 *  optimize performance of inline parsing. Due to the nature of lightweight text markup
 *  inline parsing would usually require trying a long list of choices on each input
 *  character, which is slow. These base parsers work based on mappings from the first
 *  character of an inline span to the corresponding full parser.
 *  
 *  @author Jens Halm
 */
trait InlineParsers extends MarkupParsers {
  

  /** The mapping of markup start characters to their corresponding
   *  span parsers.
   * 
   *  A parser mapped to a start character is not required
   *  to successfully parse the subsequent input. If it fails the 
   *  character that triggered the parser invocation will be treated
   *  as normal text. The mapping is merely used as a performance
   *  optimization. The parser will be invoked with the input 
   *  offset pointing to the character after the one
   *  specified as the key for the mapping.
   */
  final lazy val spanParsers: Map[Char,Parser[Span]] = prepareSpanParsers
  
  /** Extension hook for modifying the default span parser map.
   *  The default implementation returns the specified parser unchanged.
   */
  protected def prepareSpanParsers: Map[Char,Parser[Span]]
  
  
  /** Abstracts the internal process of building up the result of an inline parser.
   *  Since some inline parser produce a tree of nested spans whereas others may
   *  only produce a text result, they often require the same logic in how they
   *  deal with nested constructs.
   */
  trait ResultBuilder[Elem, +To] {
    def fromString (str: String): Elem
    def += (item: Elem): Unit
    def result: To
  } 
  
  /** ResultBuilder that produces a list of spans.
   */
  class SpanBuilder extends ResultBuilder[Span, List[Span]] {
    
    private val buffer = new ListBuffer[Span]
    
    def fromString (str: String): Span = Text(str)
    
    def += (item: Span): Unit = buffer += item
    
    def mergeAdjacentTextSpans (spans: List[Span]): List[Span] = {
      (List[Span]() /: spans) {  
        case (Text(text1,NoOpt) :: rest, Text(text2,NoOpt)) => Text(text1 ++ text2) :: rest
        case (xs, x) => x :: xs
      }.reverse
    }
    def result: List[Span] = mergeAdjacentTextSpans(buffer.toList)
  }

  /** ResultBuilder that produces a String.
   */
  class TextBuilder extends ResultBuilder[String, String] {
    
    private val builder = new scala.collection.mutable.StringBuilder
    
    def fromString (str: String): String = str
    def += (item: String): Unit = builder ++= item
    def result: String = builder.toString
  }

  /** Generic base parser that parses inline elements based on the specified
    *  helper parsers. Usually not used directly by parser implementations,
    *  this is the base parser the other inline parsers of this trait delegate to.
    *
    *  @tparam Elem the element type produced by a single parser for a nested span
    *  @tparam To the type of the result this parser produces
    *  @param text the parser for the text of the current span element
    *  @param nested a mapping from the start character of a span to the corresponding parser for nested span elements
    *  @param resultBuilder responsible for building the final result of this parser based on the results of the helper parsers
    *  @return the resulting parser
    */
  def inline [Elem,To] (text: => DelimitedText[String],
                       nested: => Map[Char, Parser[Elem]],
                       resultBuilder: => ResultBuilder[Elem,To]): Parser[To] = Parser { in =>

    lazy val builder = resultBuilder // evaluate only once
    lazy val nestedMap = nested
    lazy val textParser = DelimitedBy(new InlineDelimiter(nestedMap.keySet, text.delimiter))

    def addText (text: String) = if (!text.isEmpty) builder += builder.fromString(text)

    def nestedSpanOrNextChar (parser: Parser[Elem], input: ParserContext) = {
      parser(input) match {
        case Success(result, next) => builder += result; next
        case _ => builder += builder.fromString(input.charAt(-1).toString); input
      }
    }

    @tailrec
    def parse (input: ParserContext) : ParseResult[To] = {
      textParser(input) match {
        case Failure(msg, _) =>
          Failure(msg, in)
        case Success(EndDelimiter(text), next) =>
          addText(text)
          Success(builder.result, next)
        case Success(NestedDelimiter(startChar, text), next) =>
          addText(text)
          val parser = nestedMap(startChar)
          val newIn = nestedSpanOrNextChar(parser, next)
          parse(newIn)
      }
    }

    parse(in)
  }

  /** Parses a list of spans based on the specified helper parsers.
    *
    *  @param parser the parser for the text of the current span element
    *  @param spanParsers a mapping from the start character of a span to the corresponding parser for nested span elements
    *  @return the resulting parser
    */
  def spans (parser: => DelimitedText[String], spanParsers: => Map[Char, Parser[Span]]): Parser[List[Span]]
      = inline(parser, spanParsers, new SpanBuilder)

  /** Parses a list of spans based on the specified span parsers.
    *
    *  @param spanParsers a mapping from the start character of a span to the corresponding parser for nested span elements
    *  @return the resulting parser
    */
  def spans (spanParsers: => Map[Char, Parser[Span]]): Parser[List[Span]]
      = inline(DelimitedBy.Undelimited, spanParsers, new SpanBuilder)

  /** Parses text based on the specified helper parsers.
    *
    *  @param parser the parser for the text of the current element
    *  @param nested a mapping from the start character of a span to the corresponding parser for nested span elements
    *  @return the resulting parser
    */
  def text (parser: => DelimitedText[String], nested: => Map[Char, Parser[String]]): Parser[String]
      = inline(parser, nested, new TextBuilder)

  /** Parses a single escape character.
   *  In the default implementation any character can be escaped.
   *  Sub-traits may override this parser to restrict the number of escapable characters.
   */
  lazy val escapedChar: Parser[String] = any take 1

  /** Adds support for escape sequences to the specified text parser.
    *
    *  @param p the parser to add support for escape sequences to
    *  @return a parser for a text span that supports escape sequences
    */
  def escapedText (p: DelimitedText[String]): Parser[String] = text(p, Map('\\' -> escapedChar))

  /** Parses a span of text until one of the specified characters is seen
   *  (unless it is escaped),
   *  while also processing escaped characters, but no other nested
   *  spans. The final character is not included in the result.
   *
   *  @param char the character that signals the end of the text span
   *  @return a parser for a text span that supports escape sequences
   */
  def escapedUntil (char: Char*): Parser[String] = escapedText(DelimitedBy(char:_*).nonEmpty)

  /** Fully parses the input string and produces a list of spans.
   *
   *  This function is expected to always succeed, errors would be considered a bug
   *  of this library, as the parsers treat all unknown or malformed markup as regular
   *  text. Some parsers might additionally insert system message elements in case
   *  of markup errors.
   *
   *  @param source the input to parse
   *  @param spanParsers a mapping from the start character of a span to the corresponding parser
   *  @return the result of the parser in form of a list of spans
   */
  def parseInline (source: String, spanParsers: Map[Char, Parser[Span]]): List[Span] =
    parseMarkup(inline(DelimitedBy.Undelimited, spanParsers, new SpanBuilder), source)
    
  /** Fully parses the input string and produces a list of spans, using the
   *  default span parsers returned by the `spanParsers` method.
   * 
   *  This function is expected to always succeed, errors would be considered a bug
   *  of this library, as the parsers treat all unknown or malformed markup as regular
   *  text. Some parsers might additionally insert system message elements in case
   *  of markup errors.
   *  
   *  @param source the input to parse
   *  @return the result of the parser in form of a list of spans
   */
  def parseInline (source: String): List[Span] = parseInline(source, spanParsers)
  
}
