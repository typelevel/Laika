/*
 * Copyright 2013-2017 the original author or authors.
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

package laika.parse.core.markup

import laika.parse.InlineParsers
import laika.parse.core.{Failure, Parser, Success}
import laika.parse.core.text.{DelimitedBy, DelimitedText}
import laika.tree.Elements.{Error, InvalidSpan, Reverse, Span, SystemMessage, Text}

import scala.collection.mutable.ListBuffer

/**
  * @author Jens Halm
  */
trait DefaultRecursiveSpanParsers extends RecursiveSpanParsers {


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
  protected def spanParsers: Map[Char,Parser[Span]]


  private lazy val defaultSpanParser: Parser[List[Span]] = InlineParsers.spans(DelimitedBy.Undelimited, spanParsers)

  private def createRecursiveSpanParser (textParser: Parser[String], spanParser: => Parser[List[Span]]): Parser[List[Span]] = {
    lazy val spanParser0 = spanParser
    Parser { ctx =>
      textParser.parse(ctx) match {
        case Success(str, next) =>
          spanParser0.parse(str) match {
            case Success(spans, _) => Success(processReverseSpans(spans), next)
            case f: Failure => f
          }
        case f: Failure => f
      }
    }
  }


  def recursiveSpans (p: Parser[String]): Parser[List[Span]] =
    createRecursiveSpanParser(p, defaultSpanParser)

  def recursiveSpans (p: Parser[String],
                      additionalParsers: => Map[Char, Parser[Span]] = Map.empty): Parser[List[Span]] =
    createRecursiveSpanParser(p, InlineParsers.spans(DelimitedBy.Undelimited, spanParsers ++ additionalParsers))

  def recursiveSpans: Parser[List[Span]] = defaultSpanParser

  def delimitedRecursiveSpans (textParser: DelimitedText[String],
                               additionalSpanParsers: => Map[Char, Parser[Span]]): Parser[List[Span]] =
    InlineParsers.spans(textParser, spanParsers ++ additionalSpanParsers)

  def delimitedRecursiveSpans (textParser: DelimitedText[String]): Parser[List[Span]] =
    InlineParsers.spans(textParser, spanParsers)

  def withRecursiveSpanParser [T] (p: Parser[T]): Parser[(String => List[Span], T)] = Parser { ctx =>
    // TODO - avoid duplication with `withRecursiveBlockParser`
    p.parse(ctx) match {
      case Success(res, next) =>
        val recParser: String => List[Span] = { source: String =>
          defaultSpanParser.parse(source) match {
            case Success(spans, _) => spans
            case Failure(msg, next) =>
              val message = SystemMessage(Error, msg.message(next))
              val fallback = Text(source)
              List(InvalidSpan(message, fallback))
          }
        }
        Success((recParser, res), next)
      case f: Failure => f
    }
  }

  private def processReverseSpans (spans: List[Span]): List[Span] = {
    // TODO - solve this elsewhere and more efficiently
    if (spans.isEmpty) spans
    else {
      val buffer = new ListBuffer[Span]
      val last = (spans.head /: spans.tail) {
        case (t @ Text(content,_), Reverse(len, target, fallback, _)) =>
          if (content.length < len) { buffer += t; fallback }
          else { buffer += Text(content.dropRight(len)); target }
        case (prev, Reverse(_, _, fallback, _)) => buffer += prev; fallback
        case (prev, current)                    => buffer += prev; current
      }
      buffer += last
      buffer.toList
    }
  }


}
