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

import laika.ast._
import laika.parse._
import laika.parse.text.{DelimitedParser, DelimitedText, Delimiter, PrefixedParser}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
  
/** Provides base parsers that abstract aspects of inline parsing common to most lightweight markup languages.
 *  
 *  It contains helper parsers that abstract the typical logic required for parsing
 *  nested spans. In many cases a parser has to recognize the end of the span as well
 *  as potentially the start of a nested span. These two concerns are usually unrelated.
 *  
 *  This object offers helpers that simplify creating these types of parsers and also
 *  optimize performance of inline parsing. Due to the nature of lightweight text markup
 *  inline parsing would usually require trying a long list of choices on each input
 *  character, which is slow. These base parsers work based on mappings from the first
 *  character of an inline span to the corresponding full parser.
 *  
 *  @author Jens Halm
 */
object InlineParsers {
  

  /** Abstracts the internal process of building up the result of an inline parser.
   *  Since some inline parser produce a tree of nested spans whereas others may
   *  only produce a text result, they often require the same logic in how they
   *  deal with nested constructs.
   */
  private trait ResultBuilder[Elem, +To] {
    def fromString (str: String): Elem
    def += (item: Elem): Unit
    def result: To
  } 
  
  /** ResultBuilder that produces a list of spans.
   */
  private class SpanBuilder extends ResultBuilder[Span, List[Span]] {
    
    private val buffer = new ListBuffer[Span]

    private var last: Option[Span] = None // ListBuffer does not have constant-time update-last
    
    def fromString (str: String): Span = Text(str)
    
    def += (item: Span): Unit = (last, item) match {
      case (Some(Text(text1, NoOpt)), Text(text2, NoOpt)) =>
        last = Some(Text(text1 ++ text2))
      case (Some(Text(content, _)), Reverse(len, target, _, _)) if content.length >= len =>
        buffer += Text(content.dropRight(len))
        last = Some(target)
      case (Some(span), Reverse(_, _, fallback, _)) =>
        buffer += span
        last = Some(fallback)
      case (Some(span), newLast) =>
        buffer += span
        last = Some(newLast)
      case (None, Reverse(_, _, fallback, _)) =>
        last = Some(fallback)
      case (None, newLast) =>
        last = Some(newLast)
    }

    def result: List[Span] = last match {
      case Some(span) =>
        buffer += span
        buffer.toList
      case None =>
        Nil
    }
  }

  /** ResultBuilder that produces a String.
   */
  private class TextBuilder extends ResultBuilder[String, String] {
    
    private val builder = new scala.collection.mutable.StringBuilder
    
    def fromString (str: String): String = str
    def += (item: String): Unit = builder ++= item
    def result: String = builder.toString
  }

  private class DefaultInlineParser [Elem,To] (textDelimiter: => Delimiter[String],
                                               nested: => Seq[PrefixedParser[Elem]],
                                               resultBuilder: => ResultBuilder[Elem, To]) extends InlineParser[Elem,To] {
    
    private lazy val nestedMap = PrefixedParser.mapAndMerge(nested)
    private lazy val textParser = new DelimitedParser(new InlineDelimiter(nestedMap.keySet, textDelimiter))

    def embed (parser: => PrefixedParser[Elem]): InlineParser[Elem, To] = 
      new DefaultInlineParser(textDelimiter, nested :+ parser, resultBuilder)

    def embedAll (parsers: => Seq[PrefixedParser[Elem]]): InlineParser[Elem, To] =
      new DefaultInlineParser(textDelimiter, nested ++ parsers, resultBuilder)
    
    def parse (in: ParserContext): Parsed[To] = {

      lazy val builder = resultBuilder // need a fresh one on each invocation

      def addText (text: String): Unit = if (!text.isEmpty) builder += builder.fromString(text)

      def nestedSpanOrNextChar (parser: Parser[Elem], input: ParserContext) = {
        parser.parse(input) match {
          case Success(result, next) => builder += result; next
          case _ => builder += builder.fromString(input.char.toString); input.consume(1)
        }
      }

      @tailrec
      def parse (input: ParserContext) : Parsed[To] = {
        textParser.parse(input) match {
          case Failure(msg, _, maxOffset) =>
            Failure(msg, in, maxOffset)
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

  }

  /** Creates a new parser that reads input until the delimiters in the specified parser are detected.
    * 
    * The returned parser allows to register parsers for child spans with its `embed` method.
    * Without calling it, the result of this parser would always just be a single span of type `Text`.
    */
  def spans (parser: => DelimitedText): InlineParser[Span, List[Span]] = 
    new DefaultInlineParser(parser.delimiter, Nil, new SpanBuilder)

  /** Creates a new parser that reads text until the delimiters in the specified parser are detected.
    * 
    * The returned parser allows to register parsers for child spans with its `embed` method,
    * for example for reading escape sequences.
    */
  def text (parser: => DelimitedText): InlineParser[String, String] = 
    new DefaultInlineParser(parser.delimiter, Nil, new TextBuilder)

  @deprecated("use .spans(...).embed(...) instead", "0.14.0")
  def spans (parser: => DelimitedText, spanParsers: => Map[Char, Parser[Span]]): Parser[List[Span]]
      = new DefaultInlineParser(parser.delimiter, PrefixedParser.fromLegacyMap(spanParsers), new SpanBuilder)

  @deprecated("use .text(...).embed(...) instead", "0.14.0")
  def text (parser: => DelimitedText, nested: => Map[Char, Parser[String]]): Parser[String]
      = new DefaultInlineParser(parser.delimiter, PrefixedParser.fromLegacyMap(nested), new TextBuilder)


}

/** Generic base parser that parses inline elements with potentially nested spans. 
  * 
  * The two embed methods allow the registration of parsers for nested child spans.
  * They can be invoked multiple times. Child parsers passed first have higher
  * precedence than those passed later.
  * 
  * Only parsers of type `PrefixedParser[T]` can be passed to the embed methods,
  * which are parsers with known, stable prefixes of the child span consisting
  * of a limited set of characters so that the checks that need to be performed
  * for each character can be optimized for performance.
  *
  * @tparam Elem the element type produced by a single parser for a nested span
  * @tparam To the type of the result this parser produces
  */
trait InlineParser[Elem,To] extends Parser[To] {

  def embed (parser: => PrefixedParser[Elem]): InlineParser[Elem, To]

  def embedAll (parsers: => Seq[PrefixedParser[Elem]]): InlineParser[Elem, To]
  
}
