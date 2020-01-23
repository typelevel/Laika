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

import laika.ast.{Span, ~}
import laika.parse.Parser
import laika.parse.code.{CodeCategory, CodeSpan, CodeSpanParsers, CodeSpans}
import laika.parse.markup.InlineParsers
import laika.parse.text.DelimitedText
import laika.parse.text.TextParsers._

/**
  * @author Jens Halm
  */
object EmbeddedCodeSpans {

  def parserMap (embedded: Seq[CodeSpanParsers]): Map[Char, Parser[Span]] = embedded.flatMap(_.parsers).groupBy(_.startChar).map {
    case (char, definitions) => (char, definitions.map(_.parser).reduceLeft(_ | _))
  }

  def parser (textParser: DelimitedText[String], embedded: Seq[CodeSpanParsers], defaultCategories: Set[CodeCategory] = Set.empty): Parser[Seq[CodeSpan]] = {
    val embeddedParserMap = parserMap(embedded)
    val mainParser = InlineParsers.spans(textParser, embeddedParserMap)
    def removeFirstChar(span: Span): Span = span match {
      case CodeSpan(content, categories, opt) if content.nonEmpty => CodeSpan(content.drop(1), categories, opt)
      case _ => span
    }
    embeddedParserMap.get('\n').fold(mainParser.map(_.flatMap(CodeSpans.extract(defaultCategories)))) { newLineParsers =>
      (opt((atStart | lookBehind(1, '\n')) ~> newLineParsers) ~ mainParser).map {
        case newLineSpan ~ mainSpans =>
          (newLineSpan.map(removeFirstChar).toList ++ mainSpans).flatMap(CodeSpans.extract(defaultCategories))
      }
    }

  }

}
