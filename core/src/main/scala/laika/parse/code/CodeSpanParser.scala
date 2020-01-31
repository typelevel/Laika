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
import laika.parse.text.PrefixedParser
import laika.parse.text.TextParsers._

/** A collection of code span parsers that are intended to be applied together. 
  */
trait CodeSpanParser { self =>
  
  /** The collection of parsers provided by this instance. The order of parsers
    * is relevant, since they will be applied in the specified order. If any
    * parser successfully produces a result, the subsequent parsers will not
    * be invoked. */
  def parsers: Seq[PrefixedParser[CategorizedCode]]

  /** Merges the parsers of this collection with those of the specified other
    * collection. The order of parsers is relevant, since they will be applied 
    * in the specified order. If any parser successfully produces a result, 
    * the subsequent parsers will not be invoked
    */
  def ++ (other: CodeSpanParser): CodeSpanParser = new CodeSpanParser {
    override def parsers = self.parsers ++ other.parsers
  }
  
}

/** Companion with a range of convenient constructors for
  * creating CodeSpanParsers instances, based on existing parser
  * instances or simple specifications of start and end delimiters.
  */
object CodeSpanParser {
  
  /** Parses a single span delimited by the specified start and end strings 
    * and associates it with the given code category.
    */
  def apply (category: CodeCategory, start: String, end: String): CodeSpanParser = {
    require(start.nonEmpty)
    CodeSpanParser(category, start.head) {
      start ~> delimitedBy(end) ^^ { text => start + text + end }
    }
  }

  /** Parses a single span triggered by the specified start character 
    * and associates it with the given code category.
    */
  def apply(category: CodeCategory, startChar: Char)(parser: PrefixedParser[String]): CodeSpanParser =
    apply(category)(Seq((startChar, parser)))

  /** Parses a single span triggered by any of the specified start characters 
    * and associates it with the given code category.
    */
  def apply(category: CodeCategory, startChars: Set[Char])(parser: PrefixedParser[String]): CodeSpanParser =
    apply(category)(startChars.toSeq.map((_, parser)))

  /** Parses code spans based on a sequence mapping start characters to string
    * parsers and associates all results with the given code category.
    */
  def apply(category: CodeCategory)(startCharAndParsers: Seq[(Char, PrefixedParser[String])]): CodeSpanParser =
    new CodeSpanParser {
      val parsers = startCharAndParsers.map { case (char, parser) =>
        PrefixedParser(char)(parser.map(res => CodeSpan(res, category)))
      }
    }

  /** Parses a sequence of spans triggered by the specified start character. 
    */
  def apply(startChar: Char)(parser: PrefixedParser[Seq[CodeSpan]]): CodeSpanParser =
    apply(Seq((startChar, parser)))

  /** Parses a sequence of spans triggered by any of the specified start characters. 
    */
  def apply(startChars: Set[Char])(parser: PrefixedParser[Seq[CodeSpan]]): CodeSpanParser =
    apply(startChars.toSeq.map((_, parser)))

  /** Parses code spans based on a sequence mapping start characters to code span parsers.
    */
  def apply(startCharAndParsers: Seq[(Char, PrefixedParser[Seq[CodeSpan]])]): CodeSpanParser =
    new CodeSpanParser {
      val parsers = startCharAndParsers.map { case (char, parser) =>
        PrefixedParser(char)(parser.map(CodeSpanSequence(_)))
      }
    }
  
}
