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

import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.text.Literal
import laika.parse.text.TextParsers._

/** Base parsers for keywords in code blocks.
  * 
  * @author Jens Halm
  */
object Keywords {

  /** Parses the specified list of keywords. */
  def apply (keyword: String, keywords: String*): CodeSpanParser = 
    apply(CodeCategory.Keyword)(keyword, keywords:_*)

  /** Parses the specified list of keywords, but associates it with the specified
    * code category and not with the `Keyword` category. */
  def apply (category: CodeCategory)(keyword: String, keywords: String*): CodeSpanParser = CodeSpanParser(category) {
    (keyword +: keywords).map { kw =>
      require(kw.nonEmpty)
      (kw.head, (Literal(kw) <~ not(anyIn('a' to 'z', 'A' to 'Z', '0' to '9', '_').take(1))) ^^^ kw)
    }
  }

}
