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

import laika.ast.{InvalidElement, Span}
import laika.parse.text.{DelimitedText, PrefixedParser}
import laika.parse.{Failure, Parser, ParserContext, Success}

/** Default implementation for parsing inline markup recursively.
  *
  * @author Jens Halm
  */
trait DefaultRecursiveSpanParsers extends RecursiveSpanParsers with DefaultEscapedTextParsers {


  /** All default span parsers registered for a host markup language.
    */
  protected def spanParsers: Seq[PrefixedParser[Span]]

  private lazy val defaultSpanParser: InlineParser[Span, List[Span]] = 
    InlineParsers.spans(DelimitedText.Undelimited).embedAll(spanParsers)
  
  private class TwoPhaseInlineParser (textParser: Parser[String],
                                      delegate: => InlineParser[Span, List[Span]]) extends InlineParser[Span, List[Span]] {

    private lazy val spanParser0 = delegate

    override def embed (parser: => PrefixedParser[Span]) = 
      new TwoPhaseInlineParser(textParser, delegate.embed(parser))

    override def embedAll (parsers: => Seq[PrefixedParser[Span]]) = 
      new TwoPhaseInlineParser(textParser, delegate.embedAll(parsers))
    
    override def parse (ctx: ParserContext) = {
      textParser.parse(ctx) match {
        case Success(str, next) =>
          spanParser0.parse(str) match {
            case Success(spans, _) => Success(spans, next)
            case f: Failure => f
          }
        case f: Failure => f
      }
    }
    
  }

  def recursiveSpans (p: Parser[String]): InlineParser[Span, List[Span]] = p match {
    case dt: DelimitedText => InlineParsers.spans(dt).embedAll(spanParsers)
    case _                 => new TwoPhaseInlineParser(p, defaultSpanParser)
  }

  def recursiveSpans: InlineParser[Span, List[Span]] = defaultSpanParser

  def withRecursiveSpanParser [T] (p: Parser[T]): Parser[(String => List[Span], T)] = Parser { ctx =>
    p.parse(ctx) match {
      case Success(res, next) =>
        val recParser: String => List[Span] = { source: String =>
          defaultSpanParser.parse(source) match {
            case Success(spans, _)  => spans
            case f: Failure => List(InvalidElement(f.message, source).asSpan)
          }
        }
        Success((recParser, res), next)
      case f: Failure => f
    }
  }


  def recursiveSpans (p: Parser[String],
                      additionalParsers: => Map[Char, Parser[Span]] = Map.empty): Parser[List[Span]] = {
    recursiveSpans(p).embedAll(PrefixedParser.fromLegacyMap(additionalParsers))
  }

  def delimitedRecursiveSpans (textParser: DelimitedText,
                               additionalSpanParsers: => Map[Char, Parser[Span]]): Parser[List[Span]] =
    recursiveSpans(textParser).embedAll(PrefixedParser.fromLegacyMap(additionalSpanParsers))

  def delimitedRecursiveSpans (textParser: DelimitedText): Parser[List[Span]] =
    recursiveSpans(textParser)
  
}
