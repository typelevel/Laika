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

package laika.parse.core.markup

import laika.parse.core.text.{Complete, Continue, Delimiter, DelimiterResult}
import laika.parse.core.{ParseResult, ParserContext, Success}

/**
  * @author Jens Halm
  */
class InlineDelimiter (nestedDelimiters: Set[Char], endDelimiters: Delimiter[String]) extends Delimiter[InlineResult] {

  val startChars = nestedDelimiters ++ endDelimiters.startChars

  def atStartChar (startChar: Char, charsConsumed: Int, context: ParserContext): DelimiterResult[InlineResult] = {

    def nestedDelimiter: DelimiterResult[InlineResult] = {
      val totalConsumed = charsConsumed + 1
      val capturedText = context.capture(charsConsumed)
      Complete(Success(NestedDelimiter(startChar, capturedText), context.consume(totalConsumed)))
    }

    if (endDelimiters.startChars.contains(startChar))
      endDelimiters.atStartChar(startChar, charsConsumed, context) match {
        case Complete(s: ParseResult[String]) => Complete(s.map(EndDelimiter))
        case Continue => if (nestedDelimiters.contains(startChar)) nestedDelimiter else Continue
      }
    else nestedDelimiter
  }

  def atEOF (charsConsumed: Int, context: ParserContext): ParseResult[InlineResult] =
    endDelimiters.atEOF(charsConsumed, context).map(EndDelimiter)

}

sealed trait InlineResult

case class NestedDelimiter (startChar: Char, capturedText: String) extends InlineResult

case class EndDelimiter (capturedText: String) extends InlineResult
