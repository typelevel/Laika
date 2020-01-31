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

import cats.implicits._
import cats.data.{NonEmptyList, NonEmptySet}
import laika.bundle.SyntaxHighlighter
import laika.parse.Parser
import laika.parse.code.CodeCategory.{BooleanLiteral, LiteralValue}
import laika.parse.code.CodeSpanParser
import laika.parse.code.common.StringLiteral.StringParser
import laika.parse.code.common.{Comment, Identifier, Keywords, NumberLiteral, NumericSuffix, StringLiteral}
import laika.parse.text.TextParsers._

/**
  * @author Jens Halm
  */
object PythonSyntax extends SyntaxHighlighter {
  
  import NumberLiteral._
  
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
  
  object Prefix {
    val u: NonEmptySet[Char] = NonEmptySet.of('u','U')
    val r: NonEmptySet[Char] = NonEmptySet.of('r', 'R')
    val f: NonEmptySet[Char] = NonEmptySet.of('f', 'F')
    val b: NonEmptySet[Char] = NonEmptySet.of('b', 'B')
  }

  def stringNoPrefix (embed: StringParser => StringParser): CodeSpanParser = {
    embed(StringLiteral.multiLine("'''")) ++
    embed(StringLiteral.multiLine("\"\"\"")) ++
    embed(StringLiteral.singleLine('\'')) ++
    embed(StringLiteral.singleLine('"'))
  }
  
  def stringSinglePrefix (prefixChars: NonEmptySet[Char], embed: StringParser => StringParser): CodeSpanParser = {
    embed(StringLiteral.multiLine(prefixChars, "'''").withPrefix(anyOf('\'').take(3))) ++ 
    embed(StringLiteral.multiLine(prefixChars, "\"\"\"").withPrefix(anyOf('"').take(3))) ++
    embed(StringLiteral.singleLine(prefixChars, '\'').withPrefix(anyOf('\'').take(1))) ++
    embed(StringLiteral.singleLine(prefixChars, '"').withPrefix(anyOf('"').take(1)))
  }

  def stringDoublePrefix (prefixChars: NonEmptySet[Char], followingChars: NonEmptySet[Char], embed: StringParser => StringParser): CodeSpanParser = {
    def prefix(delim: Char, num: Int): Parser[String] = (anyOf(followingChars).take(1) ~ anyOf(delim).take(num)).concat
    embed(StringLiteral.multiLine(prefixChars, "'''").withPrefix(prefix('\'', 3))) ++
    embed(StringLiteral.multiLine(prefixChars, "\"\"\"").withPrefix(prefix('"', 3))) ++
    embed(StringLiteral.singleLine(prefixChars, '\'').withPrefix(prefix('\'', 1))) ++
    embed(StringLiteral.singleLine(prefixChars, '"').withPrefix(prefix('"', 1)))
  }

  val language: NonEmptyList[String] = NonEmptyList.of("python", "py")

  val spanParsers: Seq[CodeSpanParser] = Seq(
    Comment.singleLine("#"),
    stringNoPrefix(embedEscapes),
    stringSinglePrefix(Prefix.u ++ Prefix.b, embedEscapes),
    stringSinglePrefix(Prefix.f, (embedEscapes _).andThen(embedSubstitutions)),
    stringDoublePrefix(Prefix.r, Prefix.f, embedSubstitutions),
    stringDoublePrefix(Prefix.f, Prefix.r, embedSubstitutions),
    stringDoublePrefix(Prefix.r, Prefix.b, identity),
    stringDoublePrefix(Prefix.b, Prefix.r, identity),
    stringSinglePrefix(Prefix.r, identity),
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
