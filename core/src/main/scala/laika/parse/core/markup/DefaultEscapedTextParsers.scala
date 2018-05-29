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
import laika.parse.core.Parser
import laika.parse.core.text.{DelimitedText, TextParsers}

/**
  * @author Jens Halm
  */
trait DefaultEscapedTextParsers extends EscapedTextParsers {

  /** Parses a single escape character.
    * In the default implementation any character can be escaped.
    * Sub-traits may override this parser to restrict the number of escapable characters.
    */
  lazy val escapedChar: Parser[String] = TextParsers.any take 1

  /** Adds support for escape sequences to the specified text parser.
    *
    * @param p the parser to add support for escape sequences to
    * @return a parser for a text span that supports escape sequences
    */
  def escapedText(p: DelimitedText[String]): Parser[String] = InlineParsers.text(p, Map('\\' -> escapedChar))

  /** Parses a span of text until one of the specified characters is seen
    * (unless it is escaped),
    * while also processing escaped characters, but no other nested
    *  spans. The final character is not included in the result.
    *
    * @param char the character that signals the end of the text span
    * @return a parser for a text span that supports escape sequences
    */
  def escapedUntil(char: Char*): Parser[String] = escapedText(TextParsers.delimitedBy(char: _*).nonEmpty)

}
