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

package laika.internal.parse.markup

import laika.parse.text.DelimiterResult.{ Complete, Continue }
import laika.parse.text.{ Delimiter, DelimiterResult }
import laika.parse.{ Parsed, SourceCursor, Success }

/** Delimiter implementation for parsing inline spans that distinguishes
  * between a delimiter that marks the end of the span and a delimiter
  * that marks the start of a nested span.
  *
  * @author Jens Halm
  */
private[laika] class InlineDelimiter(nestedDelimiters: Set[Char], endDelimiters: Delimiter[String])
    extends Delimiter[InlineResult] {

  val startChars = nestedDelimiters ++ endDelimiters.startChars

  def atStartChar(
      startChar: Char,
      charsConsumed: Int,
      source: SourceCursor
  ): DelimiterResult[InlineResult] = {

    def nestedDelimiter: DelimiterResult[InlineResult] = {
      val capturedText = source.capture(charsConsumed)
      Complete(Success(NestedDelimiter(startChar, capturedText), source.consume(charsConsumed)))
    }

    if (endDelimiters.startChars.contains(startChar))
      endDelimiters.atStartChar(startChar, charsConsumed, source) match {
        case Complete(s: Parsed[String]) => Complete(s.map(EndDelimiter.apply))
        case Continue => if (nestedDelimiters.contains(startChar)) nestedDelimiter else Continue
      }
    else nestedDelimiter
  }

  def atEOF(charsConsumed: Int, source: SourceCursor): Parsed[InlineResult] =
    endDelimiters.atEOF(charsConsumed, source).map(EndDelimiter.apply)

}

/** The result of text parsed with an `InlineDelimiter`.
  */
private[laika] sealed trait InlineResult

/** The result in case the start character of a nested span has been parsed.
  */
private[laika] case class NestedDelimiter(startChar: Char, capturedText: String)
    extends InlineResult

/** The result in case the end delimiter for the text has been parsed.
  */
private[laika] case class EndDelimiter(capturedText: String) extends InlineResult
