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

package laika.parse.code

import laika.ast.{NoOpt, Options, Span, SpanContainer, TextContainer}
import laika.parse.Parser

/**
  * @author Jens Halm
  */
sealed trait CodeSpanParser {

  def startChar: Char
  
  def parser: Parser[Span]
  
}

sealed trait CodeSpanParsers { self =>
  
  def parsers: Seq[CodeSpanParser]
  
  def ++ (other: CodeSpanParsers): CodeSpanParsers = new CodeSpanParsers {
    override def parsers = self.parsers ++ other.parsers
  }
  
}

object CodeSpanParsers {
  
  private def create(s: Char, p: Parser[Span]) = new CodeSpanParser {
    val startChar = s
    val parser = p
  }
  
  def apply(category: CodeCategory, startChar: Char)(parser: Parser[String]): CodeSpanParsers =
    apply(category)(Seq((startChar, parser)))
    
  def apply(category: CodeCategory, startChars: Set[Char])(parser: Parser[String]): CodeSpanParsers =
    apply(category)(startChars.toSeq.map((_, parser)))

  def apply(category: CodeCategory)(startCharAndParsers: Seq[(Char, Parser[String])]): CodeSpanParsers =
    new CodeSpanParsers {
      val parsers = startCharAndParsers.map { case (char, parser) =>
        create(char, parser.map(res => CodeSpan(s"$char$res", category)))
      }
    }

  def apply(startChar: Char)(parser: Parser[Seq[CodeSpan]]): CodeSpanParsers =
    apply(Seq((startChar, parser)))

  def apply(startChars: Set[Char])(parser: Parser[Seq[CodeSpan]]): CodeSpanParsers =
    apply(startChars.toSeq.map((_, parser)))

  def apply(startCharAndParsers: Seq[(Char, Parser[Seq[CodeSpan]])]): CodeSpanParsers =
    new CodeSpanParsers {
      val parsers = startCharAndParsers.map { case (char, parser) =>
        create(char, parser.map(CodeSpanSequence(_)))
      }
    }
  
}

sealed trait CodeCategory

object CodeCategory {
  
  case object Comment extends CodeCategory
  case object Keyword extends CodeCategory
  case object BooleanLiteral extends CodeCategory
  case object NumberLiteral extends CodeCategory
  case object StringLiteral extends CodeCategory
  case object CharLiteral extends CodeCategory
  case object SymbolLiteral extends CodeCategory
  case object RegexLiteral extends CodeCategory
  case object LiteralValue extends CodeCategory
  case object EscapeSequence extends CodeCategory
  case object Substitution extends CodeCategory
  case object TypeName extends CodeCategory
  case object Identifier extends CodeCategory
  
}

case class CodeSpan (content: String, categories: Set[CodeCategory], options: Options = NoOpt) extends Span with TextContainer {
  type Self = CodeSpan
  def withOptions (options: Options): CodeSpan = copy(options = options)
}

object CodeSpan {
  
  def apply (content: String, category: CodeCategory): CodeSpan = apply(content, Set(category))

  def apply (content: String): CodeSpan = apply(content, Set(), NoOpt)
  
}

case class CodeSpanSequence (content: Seq[Span], options: Options = NoOpt) extends Span with SpanContainer {
  type Self = CodeSpanSequence
  def withContent (newContent: Seq[Span]): CodeSpanSequence = copy(content = newContent)
  def withOptions (options: Options): CodeSpanSequence = copy(options = options)
}
