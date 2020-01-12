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

import laika.bundle.SyntaxHighlighter
import laika.parse.code.CodeCategory.{BooleanLiteral, LiteralValue}
import laika.parse.code.CodeSpanParsers
import laika.parse.code.common.StringLiteral.StringParser
import laika.parse.code.common.{Comment, Keywords, NumberLiteral, NumericSuffix, StringLiteral}
import laika.parse.text.TextParsers._

/**
  * @author Jens Halm
  */
object Python {
  
  import NumberLiteral._
  
  def embedEscapes(parser: StringParser): CodeSpanParsers = parser.embed(
    StringLiteral.Escape.unicode,
    StringLiteral.Escape.hex,
    StringLiteral.Escape.octal,
    StringLiteral.Escape.char
  ).build

  def embedEscapesAndSubstitutions(parser: StringParser): CodeSpanParsers = parser.embed(
    StringLiteral.Escape.unicode,
    StringLiteral.Escape.hex,
    StringLiteral.Escape.octal,
    StringLiteral.Escape.char,
    StringLiteral.Escape.literal("{{"),
    StringLiteral.Escape.literal("}}"),
    StringLiteral.Substitution.between("{", "}")
  ).build

  def embedSubstitutions(parser: StringParser): CodeSpanParsers = parser.embed(
    StringLiteral.Escape.literal("{{"),
    StringLiteral.Escape.literal("}}"),
    StringLiteral.Substitution.between("{", "}")
  ).build
  
  object Prefix {
    val u: Set[Char] = Set('u','U')
    val r: Set[Char] = Set('r', 'R')
    val f: Set[Char] = Set('f', 'F')
    val b: Set[Char] = Set('b', 'B')
  }
  
  def singleLine(prefixChars: Set[Char], delim: Char): StringParser =
    StringLiteral.singleLine(prefixChars, delim).withPrefix(anyOf(delim).take(1))

  def singleLine(prefixChars: Set[Char], optFollowingChars: Set[Char], delim: Char): StringParser =
    StringLiteral.singleLine(prefixChars, delim).withPrefix((anyOf(optFollowingChars.toSeq:_*).max(1) ~ anyOf(delim).take(1)).concat)

  def singleLineDoublePrefix(prefixChars: Set[Char], followingChars: Set[Char], delim: Char): StringParser =
    StringLiteral.singleLine(prefixChars, delim).withPrefix((anyOf(followingChars.toSeq:_*).take(1) ~ anyOf(delim).take(1)).concat)

  def multiLine(prefixChars: Set[Char], delim: Char): StringParser =
    StringLiteral.multiLine(prefixChars, s"$delim$delim$delim").withPrefix(anyOf(delim).take(3))

  def multiLine(prefixChars: Set[Char], optFollowingChars: Set[Char], delim: Char): StringParser =
    StringLiteral.multiLine(prefixChars, s"$delim$delim$delim").withPrefix((anyOf(optFollowingChars.toSeq:_*).max(1) ~ anyOf(delim).take(3)).concat)

  def multiLineDoublePrefix(prefixChars: Set[Char], followingChars: Set[Char], delim: Char): StringParser =
    StringLiteral.multiLine(prefixChars, s"$delim$delim$delim").withPrefix((anyOf(followingChars.toSeq:_*).take(1) ~ anyOf(delim).take(3)).concat)

  lazy val highlighter: SyntaxHighlighter = SyntaxHighlighter.build("python", "py")(
    Comment.singleLine("#"),
    embedEscapes(StringLiteral.singleLine('\'')),
    embedEscapes(StringLiteral.singleLine('\"')),
    embedEscapes(StringLiteral.multiLine("'''")),
    embedEscapes(StringLiteral.multiLine("\"\"\"")),
    embedEscapes(singleLine(Prefix.u ++ Prefix.b, '\'')),
    embedEscapes(singleLine(Prefix.u ++ Prefix.b, '"')),
    embedEscapes(multiLine(Prefix.u ++ Prefix.b, '\'')),
    embedEscapes(multiLine(Prefix.u ++ Prefix.b, '"')),
    singleLine(Prefix.r, Prefix.b, '\'').build,
    singleLine(Prefix.r, Prefix.b, '"').build,
    singleLineDoublePrefix(Prefix.b, Prefix.r, '\'').build,
    singleLineDoublePrefix(Prefix.b, Prefix.r, '"').build,
    multiLine(Prefix.r, Prefix.b, '\'').build,
    multiLine(Prefix.r, Prefix.b, '"').build,
    multiLineDoublePrefix(Prefix.b, Prefix.r, '\'').build,
    multiLineDoublePrefix(Prefix.b, Prefix.r, '"').build,
    embedEscapesAndSubstitutions(singleLine(Prefix.f, '\'')),
    embedEscapesAndSubstitutions(singleLine(Prefix.f, '"')),
    embedEscapesAndSubstitutions(multiLine(Prefix.f, '\'')),
    embedEscapesAndSubstitutions(multiLine(Prefix.f, '"')),
    embedSubstitutions(singleLineDoublePrefix(Prefix.r, Prefix.f, '\'')),
    embedSubstitutions(singleLineDoublePrefix(Prefix.f, Prefix.r, '\'')),
    embedSubstitutions(singleLineDoublePrefix(Prefix.r, Prefix.f, '"')),
    embedSubstitutions(singleLineDoublePrefix(Prefix.f, Prefix.r, '"')),
    embedSubstitutions(multiLineDoublePrefix(Prefix.r, Prefix.f, '\'')),
    embedSubstitutions(multiLineDoublePrefix(Prefix.f, Prefix.r, '\'')),
    embedSubstitutions(multiLineDoublePrefix(Prefix.r, Prefix.f, '"')),
    embedSubstitutions(multiLineDoublePrefix(Prefix.f, Prefix.r, '"')),
    Keywords(BooleanLiteral)("True", "False"),
    Keywords(LiteralValue)("None"),
    Keywords("and", "assert", "async", "as", "await", "break", "class", "continue", "def", "del", "elif", "else", 
      "except", "exec", "finally", "for", "from", "global", "if", "import", "in", "is", "lambda", 
      "nonlocal", "not", "or", "pass", "print", "raise", "return", "try", "with", "while", "yield"),
    NumberLiteral.binary.withUnderscores.build,
    NumberLiteral.octal.withUnderscores.build,
    NumberLiteral.hex.withUnderscores.build,
    NumberLiteral.decimalFloat.withUnderscores.withSuffix(NumericSuffix.imaginary).build,
    NumberLiteral.decimalInt.withUnderscores.withSuffix(NumericSuffix.imaginary).build,
  )
  
}
