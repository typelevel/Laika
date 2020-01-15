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

import laika.ast.{Span, Text}
import laika.parse.code.{CodeCategory, CodeSpan, CodeSpanParsers, CodeSpanSequence}

/**
  * @author Jens Halm
  */
trait EmbeddedCodeSpans {
  
  def embedded: Seq[CodeSpanParsers]
  
  def defaultCategories: Set[CodeCategory]
  
  protected lazy val spanParserMap = embedded.flatMap(_.parsers).groupBy(_.startChar).map {
    case (char, definitions) => (char, definitions.map(_.parser).reduceLeft(_ | _))
  }
  
  protected def toCodeSpans (span: Span): Seq[CodeSpan] = span match {
    case Text(content, _)          => Seq(CodeSpan(content, defaultCategories))
    case codeSpan: CodeSpan        => Seq(codeSpan)
    case codeSeq: CodeSpanSequence => codeSeq.collect { case cs: CodeSpan => cs }
    case _                         => Nil
  }
  
  protected def mergeCodeSpans (startChar: Char, spans: Seq[CodeSpan]): Seq[CodeSpan] = {
    val startSpan = CodeSpan(startChar.toString, defaultCategories)
    spans.foldLeft(List(startSpan)) { case (acc, next) =>
      if (acc.last.categories == next.categories) acc.init :+ CodeSpan(acc.last.content + next.content, next.categories)
      else acc :+ next
    }
  }
  
}
