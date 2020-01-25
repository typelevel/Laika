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

import laika.ast.{CategorizedCode, CodeSpan, CodeSpanSequence}
import laika.parse.Parser
import laika.parse.text.TextParsers._

/**
  * @author Jens Halm
  */
trait CodeSpanParser {

  def startChar: Char
  
  def parser: Parser[CategorizedCode]
  
}

trait CodeSpanParsers { self =>
  
  def parsers: Seq[CodeSpanParser]
  
  def ++ (other: CodeSpanParsers): CodeSpanParsers = new CodeSpanParsers {
    override def parsers = self.parsers ++ other.parsers
  }
  
}

object CodeSpanParsers {
  
  private def create(s: Char, p: Parser[CategorizedCode]) = new CodeSpanParser {
    val startChar = s
    val parser = p
  }

  def apply (category: CodeCategory, start: String, end: String): CodeSpanParsers = {
    require(start.nonEmpty)
    CodeSpanParsers(category, start.head) {
      start.tail ~> delimitedBy(end) ^^ { text => start.tail + text + end }
    }
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
