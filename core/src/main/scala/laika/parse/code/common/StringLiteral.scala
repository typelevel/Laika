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

import laika.ast.~
import laika.parse.Parser
import laika.parse.code.{CodeCategory, CodeSpan, CodeSpanParsers, CodeSpans}
import laika.parse.text.TextParsers._
import laika.parse.text.{DelimitedText, TextParsers}

/**
  * @author Jens Halm
  */
object StringLiteral {
  
  import NumberLiteral._

  object Escape {
    
    val char: CodeSpanParsers = CodeSpanParsers(CodeCategory.EscapeSequence, '\\')(any.take(1))
    
    val unicode: CodeSpanParsers = CodeSpanParsers(CodeCategory.EscapeSequence, '\\') {
      (anyOf('u').take(1) ~ DigitParsers.hex.take(4)).concat
    }
  
    val hex: CodeSpanParsers = CodeSpanParsers(CodeCategory.EscapeSequence, '\\') {
      (anyOf('x').take(1) ~ DigitParsers.hex.take(2)).concat
    }
  
    val octal: CodeSpanParsers = CodeSpanParsers(CodeCategory.EscapeSequence, '\\') {
      (anyIn('0' to '3').take(1) ~ DigitParsers.octal.max(2)).concat | DigitParsers.octal.min(1).max(2)
    }
    
    def literal(value: String): CodeSpanParsers = {
      require(value.nonEmpty)
      CodeSpanParsers(CodeCategory.EscapeSequence, value.head) {
        TextParsers.literal(value.tail)
      }
    }
  }
  
  object Substitution {
    
    def apply(startChar: Char)(parser: Parser[String]): CodeSpanParsers = 
      CodeSpanParsers(CodeCategory.Substitution, startChar)(parser)
    
    def between(start: String, end: String): CodeSpanParsers = {
      require(start.nonEmpty)
      require(end.nonEmpty)
      apply(start.head) {
        if (start.tail.isEmpty) (delimitedBy(end).keepDelimiter.failOn('\n') ~ end).concat
        else (literal(start.tail) ~ delimitedBy(end).keepDelimiter.failOn('\n') ~ end).concat
      }
    }
    
    def between(delimiter: String): CodeSpanParsers = between(delimiter, delimiter)
    
  }
  
  case class StringParser(startChars: Set[Char],
                          parser: DelimitedText[String],
                          prefix: Option[Parser[String]] = None,
                          postfix: Option[Parser[String]] = None,
                          embedded: Seq[CodeSpanParsers] = Nil,
                          defaultCategories: Set[CodeCategory] = Set(CodeCategory.StringLiteral)) {
    
    def embed(childSpans: CodeSpanParsers*): StringParser = {
      copy(embedded = embedded ++ childSpans)
    }
    
    def withPrefix (parser: Parser[String]): StringParser = copy(prefix = prefix.fold(Some(parser)) { oldPrefix =>
      Some((oldPrefix ~ parser).concat)
    })

    def withPostCondition (parser: Parser[Unit]): StringParser = copy(postfix = postfix.fold(Some(parser ^^^ "")) { oldPrefix =>
      Some(oldPrefix <~ parser)
    })
    
    def withCategory (category: CodeCategory): StringParser = copy(defaultCategories = Set(category))
    
    def build: CodeSpanParsers = CodeSpanParsers(startChars) {
      
      def optParser(p: Option[Parser[String]]): Parser[List[CodeSpan]] = 
        p.map(_.map(res => List(CodeSpan(res, defaultCategories)))).getOrElse(success(Nil))
      
      (lookBehind(1, any.take(1)) ~ optParser(prefix) ~ EmbeddedCodeSpans.parser(parser, embedded, defaultCategories) ~ optParser(postfix)).map {
        case startChar ~ pre ~ content ~ post => CodeSpans.merge(startChar.head, pre ++ content ++ post, defaultCategories)
      }
    }
    
  }
  
  def singleLine (between: Char): StringParser = singleLine(Set(between), between)

  def singleLine (startChars: Set[Char], end: Char): StringParser = {
    require(startChars.nonEmpty)
    val parser = delimitedBy(end).failOn('\n').keepDelimiter
    StringParser(startChars, parser, postfix = Some(anyOf(end).take(1)))
  }

  def multiLine (between: String): StringParser = multiLine(between, between)

  def multiLine (startDelim: String, endDelim: String): StringParser = {
    require(startDelim.nonEmpty)
    require(endDelim.nonEmpty)
    val prefix = if (startDelim.tail.nonEmpty) Some(literal(startDelim.tail)) else None
    val parser = delimitedBy(endDelim).keepDelimiter
    StringParser(Set(startDelim.head), parser, prefix, postfix = Some(literal(endDelim)))
  }

  def multiLine (startChars: Set[Char], end: String): StringParser = {
    require(startChars.nonEmpty)
    val parser = delimitedBy(end).keepDelimiter
    StringParser(startChars, parser, postfix = Some(literal(end)))
  }
  
}
