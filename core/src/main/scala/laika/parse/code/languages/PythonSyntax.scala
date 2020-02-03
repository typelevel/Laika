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
import laika.parse.code.CodeCategory.{BooleanLiteral, LiteralValue}
import laika.parse.code.CodeSpanParser
import laika.parse.code.common.StringLiteral.StringParser
import laika.parse.code.common._

/**
  * @author Jens Halm
  */
object PythonSyntax extends SyntaxHighlighter {
  
  def embedEscapes(parser: StringParser): StringParser = parser.embed(
    StringLiteral.Escape.unicode,
    StringLiteral.Escape.hex,
    StringLiteral.Escape.octal,
    StringLiteral.Escape.char
  )

  def embedSubstitutions(parser: StringParser): StringParser = parser.embed(
    StringLiteral.Escape.literal("{{"),
    StringLiteral.Escape.literal("}}"),
    StringLiteral.Substitution.between("{", "}")
  )
  
  def stringLiteral (embed: StringParser => StringParser) (prefix: String, prefixes: String*): CodeSpanParser =
    (prefix +: prefixes).map { pref =>
      embed(StringLiteral.multiLine(pref + "'''", "'''")) ++
      embed(StringLiteral.multiLine(pref + "\"\"\"", "\"\"\"")) ++
      embed(StringLiteral.singleLine(pref + "'", "'")) ++
      embed(StringLiteral.singleLine(pref + "\"", "\""))
    }.reduceLeft(_ ++ _)

  val language: NonEmptyList[String] = NonEmptyList.of("python", "py")

  val spanParsers: Seq[CodeSpanParser] = Seq(
    Comment.singleLine("#"),
    stringLiteral(embedEscapes)("","u","U","b","B"),
    stringLiteral((embedEscapes _).andThen(embedSubstitutions))("f","F"),
    stringLiteral(embedSubstitutions)("rf","Rf","rF","RF","fr","fR","Fr","FR"),
    stringLiteral(embedSubstitutions)("rb","Rb","rB","RB","br","bR","Br","BR"),
    stringLiteral(identity)("r", "R"),
    Keywords(BooleanLiteral)("True", "False"),
    Keywords(LiteralValue)("None"),
    Keywords("and", "assert", "async", "as", "await", "break", "class", "continue", "def", "del", "elif", "else", 
      "except", "exec", "finally", "for", "from", "global", "if", "import", "in", "is", "lambda", 
      "nonlocal", "not", "or", "pass", "print", "raise", "return", "try", "with", "while", "yield"),
    Identifier.alphaNum.withIdStartChars('_','$'),
    NumberLiteral.binary.withUnderscores,
    NumberLiteral.octal.withUnderscores,
    NumberLiteral.hex.withUnderscores,
    NumberLiteral.decimalFloat.withUnderscores.withSuffix(NumericSuffix.imaginary),
    NumberLiteral.decimalInt.withUnderscores.withSuffix(NumericSuffix.imaginary),
  )
  
}
