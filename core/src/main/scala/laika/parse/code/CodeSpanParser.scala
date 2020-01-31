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
    CodeSpanParser(category) {
      start ~> delimitedBy(end) ^^ { text => start + text + end }
    }
  }

  /** Parses a single text span and associates it with the given code category.
    */
  def apply (category: CodeCategory)(parser: PrefixedParser[String]): CodeSpanParser =
    new CodeSpanParser {
      val parsers = Seq(parser.map(res => CodeSpan(res, category)))
    }

  /** Parses a sequence of code spans. 
    */
  def apply(parser: PrefixedParser[Seq[CodeSpan]]): CodeSpanParser =
    new CodeSpanParser {
      val parsers = Seq(parser.map(CodeSpanSequence(_)))
    }

}
