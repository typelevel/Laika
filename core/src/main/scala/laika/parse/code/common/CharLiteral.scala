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

package laika.parse.code.common

import laika.ast.{Span, Text, ~}
import laika.parse.Parser
import laika.parse.code.{CodeCategory, CodeSpan, CodeSpanParsers, CodeSpanSequence}
import laika.parse.text.TextParsers._

/**
  * @author Jens Halm
  */
object CharLiteral {
  
  case class CharParser(delim: Char,
                          embedded: Seq[CodeSpanParsers] = Nil) {

    private val defaultParser = lookBehind(1, anyBut('\'', '\n').take(1))
      .map(CodeSpan(_, CodeCategory.CharLiteral))
    
    private val closingDelimParser = anyOf(delim).take(1)
      .map(CodeSpan(_, CodeCategory.CharLiteral))
    
    def embed(childSpans: CodeSpanParsers*): CharParser = {
      copy(embedded = embedded ++ childSpans)
    }

    def build: CodeSpanParsers = {
      val spanParserMap = embedded.flatMap(_.parsers).groupBy(_.startChar).map {
        case (char, definitions) => (char, definitions.map(_.parser).reduceLeft(_ | _))
      }
      CodeSpanParsers(delim) {

        val contentParser = any.take(1).flatMap { char =>
          (spanParserMap.getOrElse(char.head, defaultParser) ~ closingDelimParser).map { case a ~ b => Seq(a, b) }: Parser[Seq[Span]]
        }.map {
          case Seq(Text(content, _), delim: CodeSpan) => Seq(CodeSpan(content, CodeCategory.CharLiteral), delim)
          case Seq(codeSpan: CodeSpan, delim: CodeSpan) => Seq(codeSpan, delim)
          case Seq(codeSeq: CodeSpanSequence, delim: CodeSpan) => codeSeq.collect { case cs: CodeSpan => cs } :+ delim
        }

        contentParser.map { content =>
          val spans = CodeSpan(delim.toString, CodeCategory.CharLiteral) +: content
          spans.tail.foldLeft(List(spans.head)) { case (acc, next) =>
            if (acc.last.categories == next.categories) acc.init :+ CodeSpan(acc.last.content + next.content, next.categories)
            else acc :+ next
          }
        }
      }
    }

  }
  
  def standard: CharParser = CharParser('\'')
  
}
