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

import cats.implicits._
import cats.data.NonEmptySet
import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.code.common.StringLiteral.StringParser
import laika.parse.text.CharGroup
import laika.parse.text.TextParsers._

/** Base parsers for regular expression literals in code blocks.
  * 
  * @author Jens Halm
  */
object RegexLiteral {
  
  import NumberLiteral._

  /** Parses a regular expression between `/` delimiters, followed by optional modifier characters. */
  val standard: CodeSpanParser = StringParser(
    chars = NonEmptySet.one('/'),
    parser = delimitedBy("/").failOn('\n').keepDelimiter,
    postfix = Some((anyOf('/').take(1) ~ anyOf(CharGroup.alpha)).concat),
    embedded = Seq(StringLiteral.Escape.char),
    defaultCategories = Set(CodeCategory.RegexLiteral)
  )
  
}
