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

import laika.parse.code.{ CodeCategory, CodeSpanParser }
import laika.parse.text.CharGroup
import laika.parse.builders._

/** Base parsers for regular expression literals in code blocks.
  *
  * @author Jens Halm
  */
object RegexLiteral {

  /** Parses a regular expression between `/` delimiters, followed by optional modifier characters. */
  val standard: CodeSpanParser = {
    val startDelim = literal("/")
    val endDelim   = (startDelim ~ anyOf(CharGroup.alpha)).source
    StringLiteral
      .singleLine(startDelim, endDelim)
      .embed(StringLiteral.Escape.char)
      .withCategory(CodeCategory.RegexLiteral)
  }

}
