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
import laika.parse.Parser
import laika.parse.code.CodeCategory.{BooleanLiteral, LiteralValue}
import laika.parse.code.CodeSpanParsers
import laika.parse.code.common.StringLiteral.StringParser
import laika.parse.code.common.{Comment, Identifier, Keywords, NumberLiteral, NumericSuffix, StringLiteral}
import laika.parse.text.TextParsers._

/**
  * @author Jens Halm
  */
object Python extends SyntaxHighlighter {
  
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
    val u: Set[Char] = Set('u','U')
    val r: Set[Char] = Set('r', 'R')
    val f: Set[Char] = Set('f', 'F')
    val b: Set[Char] = Set('b', 'B')
  }

  def stringNoPrefix (embed: StringParser => StringParser): CodeSpanParsers = {
    embed(StringLiteral.multiLine("'''")) ++
    embed(StringLiteral.multiLine("\"\"\"")) ++
    embed(StringLiteral.singleLine('\'')) ++
    embed(StringLiteral.singleLine('"'))
  }
  
  def stringSinglePrefix (prefixChars: Set[Char], embed: StringParser => StringParser): CodeSpanParsers = {
    embed(StringLiteral.multiLine(prefixChars, "'''").withPrefix(anyOf('\'').take(3))) ++ 
    embed(StringLiteral.multiLine(prefixChars, "\"\"\"").withPrefix(anyOf('"').take(3))) ++
    embed(StringLiteral.singleLine(prefixChars, '\'').withPrefix(anyOf('\'').take(1))) ++
    embed(StringLiteral.singleLine(prefixChars, '"').withPrefix(anyOf('"').take(1)))
  }

  def stringDoublePrefix (prefixChars: Set[Char], followingChars: Set[Char], embed: StringParser => StringParser): CodeSpanParsers = {
    def prefix(delim: Char, num: Int): Parser[String] = (anyOf(followingChars.toSeq:_*).take(1) ~ anyOf(delim).take(num)).concat
    embed(StringLiteral.multiLine(prefixChars, "'''").withPrefix(prefix('\'', 3))) ++
    embed(StringLiteral.multiLine(prefixChars, "\"\"\"").withPrefix(prefix('"', 3))) ++
    embed(StringLiteral.singleLine(prefixChars, '\'').withPrefix(prefix('\'', 1))) ++
    embed(StringLiteral.singleLine(prefixChars, '"').withPrefix(prefix('"', 1)))
  }

  val language: NonEmptyList[String] = NonEmptyList.of("python", "py")

  val spanParsers: Seq[CodeSpanParsers] = Seq(
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
    Identifier.standard.withIdStartChars('_','$'),
    NumberLiteral.binary.withUnderscores,
    NumberLiteral.octal.withUnderscores,
    NumberLiteral.hex.withUnderscores,
    NumberLiteral.decimalFloat.withUnderscores.withSuffix(NumericSuffix.imaginary),
    NumberLiteral.decimalInt.withUnderscores.withSuffix(NumericSuffix.imaginary),
  )
  
}
