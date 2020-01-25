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

/** A parser for a single span of categorized code. 
  */
trait CodeSpanParser {

  /** The start character that triggers the invocation of the parser.
    * This explicit mapping is required for performance optimizations. */
  def startChar: Char

  /** The actual parser for the code span. */
  def parser: Parser[CategorizedCode]
  
}

/** A collection of code span parsers that are intended to be applied together. 
  */
trait CodeSpanParsers { self =>
  
  /** The collection of parsers provided by this instance. The order of parsers
    * is relevant, since they will be applied in the specified order. If any
    * parser successfully produces a result, the subsequent parsers will not
    * be invoked. */
  def parsers: Seq[CodeSpanParser]

  /** Merges the parsers of this collection with those of the specified other
    * collection. The order of parsers is relevant, since they will be applied 
    * in the specified order. If any parser successfully produces a result, 
    * the subsequent parsers will not be invoked
    */
  def ++ (other: CodeSpanParsers): CodeSpanParsers = new CodeSpanParsers {
    override def parsers = self.parsers ++ other.parsers
  }
  
}

/** Companion with a range of convenient constructors for
  * creating CodeSpanParsers instances, based on existing parser
  * instances or simple specifications of start and end delimiters.
  */
object CodeSpanParsers {
  
  private def create(s: Char, p: Parser[CategorizedCode]) = new CodeSpanParser {
    val startChar = s
    val parser = p
  }

  /** Parses a single span delimited by the specified start and end strings 
    * and associates it with the given code category.
    */
  def apply (category: CodeCategory, start: String, end: String): CodeSpanParsers = {
    require(start.nonEmpty)
    CodeSpanParsers(category, start.head) {
      start.tail ~> delimitedBy(end) ^^ { text => start.tail + text + end }
    }
  }

  /** Parses a single span triggered by the specified start character 
    * and associates it with the given code category.
    */
  def apply(category: CodeCategory, startChar: Char)(parser: Parser[String]): CodeSpanParsers =
    apply(category)(Seq((startChar, parser)))

  /** Parses a single span triggered by any of the specified start characters 
    * and associates it with the given code category.
    */
  def apply(category: CodeCategory, startChars: Set[Char])(parser: Parser[String]): CodeSpanParsers =
    apply(category)(startChars.toSeq.map((_, parser)))

  /** Parses code spans based on a sequence mapping start characters to string
    * parsers and associates all results with the given code category.
    */
  def apply(category: CodeCategory)(startCharAndParsers: Seq[(Char, Parser[String])]): CodeSpanParsers = // TODO - 0.14 - might be obsolete after delimiter refactoring
    new CodeSpanParsers {
      val parsers = startCharAndParsers.map { case (char, parser) =>
        create(char, parser.map(res => CodeSpan(s"$char$res", category)))
      }
    }

  /** Parses a sequence of spans triggered by the specified start character. 
    */
  def apply(startChar: Char)(parser: Parser[Seq[CodeSpan]]): CodeSpanParsers =
    apply(Seq((startChar, parser)))

  /** Parses a sequence of spans triggered by any of the specified start characters. 
    */
  def apply(startChars: Set[Char])(parser: Parser[Seq[CodeSpan]]): CodeSpanParsers =
    apply(startChars.toSeq.map((_, parser)))

  /** Parses code spans based on a sequence mapping start characters to code span parsers.
    */
  def apply(startCharAndParsers: Seq[(Char, Parser[Seq[CodeSpan]])]): CodeSpanParsers =
    new CodeSpanParsers {
      val parsers = startCharAndParsers.map { case (char, parser) =>
        create(char, parser.map(CodeSpanSequence(_)))
      }
    }
  
}
