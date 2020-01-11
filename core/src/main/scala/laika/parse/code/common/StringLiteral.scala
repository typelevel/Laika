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
import laika.ast.Text
import laika.parse.Parser
import laika.parse.code.{CodeCategory, CodeSpan, CodeSpanParsers, CodeSpanSequence}
import laika.parse.markup.InlineParsers
import laika.parse.text.{DelimitedText, TextParsers}
import laika.parse.text.TextParsers._

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
  
  case class StringParser(startChar: Char,
                          parser: DelimitedText[String],
                          prefix: Option[Parser[String]] = None,
                          postfix: Option[Parser[String]] = None,
                          embedded: Seq[CodeSpanParsers] = Nil) {
    
    def embed(childSpans: CodeSpanParsers*): StringParser = {
      copy(embedded = embedded ++ childSpans)
    }
    
    def build: CodeSpanParsers = {
      val spanParserMap = embedded.flatMap(_.parsers).groupBy(_.startChar).map {
        case (char, definitions) => (char, definitions.map(_.parser).reduceLeft(_ | _))
      }
      CodeSpanParsers(startChar) {
        val contentParser = InlineParsers.spans(parser, spanParserMap).map(
          _.flatMap {
            case Text(content, _)          => Seq(CodeSpan(content, CodeCategory.StringLiteral))
            case codeSpan: CodeSpan        => Seq(codeSpan)
            case codeSeq: CodeSpanSequence => codeSeq.collect { case cs: CodeSpan => cs }
          }
        )
        def optParser(p: Option[Parser[String]]): Parser[List[CodeSpan]] = 
          p.map(_.map(res => List(CodeSpan(res, CodeCategory.StringLiteral)))).getOrElse(success(Nil))
        
        (optParser(prefix) ~ contentParser ~ optParser(postfix)).map {
          case pre ~ content ~ post => 
            val spans = CodeSpan(startChar.toString, CodeCategory.StringLiteral) +: (pre ++ content ++ post)
            spans.tail.foldLeft(List(spans.head)) { case (acc, next) =>
              if (acc.last.categories == next.categories) acc.init :+ CodeSpan(acc.last.content + next.content, next.categories)
              else acc :+ next
            }
        }
      }
    }
    
  }
  
  def singleLine (between: Char): StringParser = {
    require(between.nonEmpty)
    val parser = delimitedBy(between, '\n').keepDelimiter
    StringParser(between.head, parser, postfix = Some(anyOf(between).take(1)))
  }

  def singleLineTemplate (between: Char): StringParser =  {
    require(between.nonEmpty)
    val parser = delimitedBy(between, '\n').keepDelimiter
    StringParser(between.head, parser, postfix = Some(anyOf(between).take(1)))
  }
  
  def multiLine (between: String): StringParser = {
    require(between.nonEmpty)
    val prefix = if (between.tail.nonEmpty) Some(literal(between.tail)) else None
    val parser = delimitedBy(between).keepDelimiter
    StringParser(between.head, parser, prefix, postfix = Some(literal(between)))
  }
  
  def multiLineTemplate (between: String): StringParser =  {
    require(between.nonEmpty)
    val prefix = if (between.tail.nonEmpty) Some(literal(between.tail)) else None
    val parser = delimitedBy(between)
    StringParser(between.head, parser, prefix, postfix = Some(literal(between)))
  }
  
}
