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

package laika.parse.code.languages

import cats.data.NonEmptyList
import laika.bundle.SyntaxHighlighter
import laika.parse.code.{CodeCategory, CodeSpanParsers}
import laika.parse.code.CodeCategory.{BooleanLiteral, LiteralValue}
import laika.parse.code.common.StringLiteral.StringParser
import laika.parse.code.common.{Keywords, NumberLiteral, StringLiteral}
import laika.parse.text.TextParsers._

/**
  * @author Jens Halm
  */
object JSON extends SyntaxHighlighter {

  val string: StringParser = StringLiteral.singleLine('"').embed(
    StringLiteral.Escape.unicode,
    StringLiteral.Escape.char
  )
  
  val attributeName: StringParser = string
    .withPostCondition(lookAhead(ws ~ ':') ^^^ (()))
    .copy(defaultCategories = Set(CodeCategory.AttributeName))

  val language: NonEmptyList[String] = NonEmptyList.of("json")

  val spanParsers: Seq[CodeSpanParsers] = Seq(
    Keywords(BooleanLiteral)("true", "false"),
    Keywords(LiteralValue)("null"),
    NumberLiteral.decimalFloat,
    NumberLiteral.decimalInt,
    attributeName,
    string,
  )
  
}
