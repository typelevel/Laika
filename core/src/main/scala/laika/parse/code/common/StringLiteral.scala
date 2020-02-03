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

import laika.ast.{CodeSpan, CodeSpans, ~}
import laika.parse.Parser
import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.text.TextParsers._
import laika.parse.text.{PrefixedParser, TextParsers}

/** Configurable base parsers for string literals.
  * 
  * @author Jens Halm
  */
object StringLiteral {
  
  import NumberLiteral._

  /** Base parsers for escape sequences.
   */
  object Escape {
    
    /** Parses a simple backslash character escape. Does except any character after the backslash,
      * if you want to be strict and apply the specific rules about which characters are valid
      * escapes you need to create a custom parser. */
    val char: CodeSpanParser = CodeSpanParser(CodeCategory.EscapeSequence) {
      (TextParsers.literal("\\") ~ any.take(1)).concat
    }

    /** Parses a unicode character escape.
      * It must start with a backslash, followed by the letter `u` and exactly four hex digits,
      * e.g. `\\u20ff`.
      */
    val unicode: CodeSpanParser = CodeSpanParser(CodeCategory.EscapeSequence) {
      (TextParsers.literal("\\u") ~ DigitParsers.hex.take(4)).concat
    }

    /** Parses a hexadecimal character escape.
      * It must start with a backslash, followed by the letter `x` and exactly two hex digits,
      * e.g. `\\xf2`.
      */
    val hex: CodeSpanParser = CodeSpanParser(CodeCategory.EscapeSequence) {
      (TextParsers.literal("\\x") ~ DigitParsers.hex.take(2)).concat
    }

    /** Parses a octal character escape.
      * It must start with a backslash, followed by one to three octal digits,
      * e.g. `\\207`.
      */
    val octal: CodeSpanParser = CodeSpanParser(CodeCategory.EscapeSequence) {
      (TextParsers.literal("\\") ~ ((anyIn('0' to '3').take(1) ~ DigitParsers.octal.max(2)).concat | DigitParsers.octal.min(1).max(2))).concat
    }

    /** Parses a single literal escape. Example: `$$`. 
      */
    def literal(value: String): CodeSpanParser = {
      require(value.nonEmpty)
      CodeSpanParser(CodeCategory.EscapeSequence)(TextParsers.literal(value))
    }
  }

  /** Base parsers for substitution references in interpolated strings (or sometimes
    * called string templates depending on the language).
    */
  object Substitution {

    /** Parses a substitution code span based on the specified trigger character
      * and string parser. 
      */
    def apply (parser: PrefixedParser[String]): CodeSpanParser = 
      CodeSpanParser(CodeCategory.Substitution)(parser)

    /** Parses a substitution code span based on the specified start and end delimiters. 
      */
    def between(start: String, end: String): CodeSpanParser = {
      require(start.nonEmpty)
      require(end.nonEmpty)
      apply {
        (literal(start) ~ delimitedBy(end).keepDelimiter.failOn('\n') ~ end).concat
      }
    }

    /** Parses a substitution code span based on the specified start and end delimiter. 
      */
    def between(delimiter: String): CodeSpanParser = between(delimiter, delimiter)
    
  }
  
  /** Configurable base parser for string literals. */
  case class StringParser(startDelimParser: PrefixedParser[String],
                          endDelimParser: PrefixedParser[String],
                          multiline: Boolean = false,
                          postCondition: Option[Parser[Unit]] = None,
                          embedded: Seq[CodeSpanParser] = Nil,
                          defaultCategories: Set[CodeCategory] = Set(CodeCategory.StringLiteral)) extends CodeParserBase {

    /** Embeds the specified parsers for child spans inside a character literal.
      *
      * This is usually used for allowing escape sequences or substitution expressions inside the literal.
      */
    def embed(childSpans: CodeSpanParser*): StringParser = {
      copy(embedded = embedded ++ childSpans)
    }
    
    /** Tests and consumes a post condition after the end delimiter has been read.
      */
    def withPostCondition (parser: Parser[Unit]): StringParser = copy(postCondition = Some(parser))

    /** Applies the specified category to the result.
      * 
      * Useful where the logic of the string literal parser needs to be used with
      * a span that should not be classified as a string literal.
      */
    def withCategory (category: CodeCategory): StringParser = copy(defaultCategories = Set(category))
    
    lazy val underlying: PrefixedParser[Seq[CodeSpan]] = {
      
      val startParser = startDelimParser.map(CodeSpan(_, defaultCategories))
      val endParser = postCondition.fold(endDelimParser)(endDelimParser <~ _).map(CodeSpan(_, defaultCategories))
      
      val textParser = {
        val base = delimitedBy(endDelimParser).keepDelimiter
        if (multiline) base else base.failOn('\n')
      }

      val embeddedParser = EmbeddedCodeSpans.parser(textParser, embedded, defaultCategories)

      (startParser ~ embeddedParser ~ endParser).map {
        case start ~ content ~ post => CodeSpans.merge(start +: (content :+ post))
      }
    }
    
  }

  /** Parses a string literal on a single line enclosed by the specified start and end delimiter.
    */
  def singleLine (between: Char): StringParser = singleLine(between.toString, between.toString)

  /** Parses a string literal on a single line enclosed by the specified start and end delimiters.
    */
  def singleLine (startDelim: String, endDelim: String): StringParser = {
    require(startDelim.nonEmpty)
    require(endDelim.nonEmpty)
    StringParser(literal(startDelim), literal(endDelim))
  }

  /** Parses a string literal on a single line enclosed by the specified start and end delimiters.
    */
  def singleLine (startDelim: PrefixedParser[String], endDelim: PrefixedParser[String]): StringParser = {
    StringParser(startDelim, endDelim)
  }

  /** Parses a string literal that can span multiple lines, enclosed by the specified start and end delimiter.
    */
  def multiLine (between: String): StringParser = multiLine(between, between)

  /** Parses a string literal that can span multiple lines, enclosed by the specified start and end delimiters.
    */
  def multiLine (startDelim: String, endDelim: String): StringParser = {
    require(startDelim.nonEmpty)
    require(endDelim.nonEmpty)
    StringParser(literal(startDelim), literal(endDelim), multiline = true)
  }

  /** Parses a string literal that can span multiple lines, enclosed by the specified delimiters.
    */
  def multiLine (startDelim: PrefixedParser[String], endDelim: PrefixedParser[String]): StringParser = {
    StringParser(startDelim, endDelim, multiline = true)
  }
  
}
