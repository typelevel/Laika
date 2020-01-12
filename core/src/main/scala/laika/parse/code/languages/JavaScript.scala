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

import laika.ast.~
import laika.bundle.SyntaxHighlighter
import laika.parse.code.CodeCategory.{BooleanLiteral, LiteralValue}
import laika.parse.code.{CodeCategory, CodeSpanParsers}
import laika.parse.code.common.NumberLiteral.{DigitParsers, NumericParser}
import laika.parse.code.common.{Comment, Identifier, Keywords, NumberLiteral, NumericSuffix, RegexLiteral, StringLiteral}
import laika.parse.text.TextParsers._

/**
  * @author Jens Halm
  */
object JavaScript {

  val unicodeCodePointEscape: CodeSpanParsers = CodeSpanParsers(CodeCategory.EscapeSequence, '\\') {
    ("u{" ~ DigitParsers.hex.min(1) ~ '}').map { case a ~ b ~ c => a + b + c.toString }
  }
  
  def number(parser: NumericParser): CodeSpanParsers = parser.withUnderscores.withSuffix(NumericSuffix.bigInt).build
  
  val keywords = Keywords("async", "as", "await", "break", "case", "catch", "const", "continue", "debugger", "default", "delete",
    "do", "else", "export", "finally", "for", "from", "function", "if", "import", "instanceof", "in",
    "let", "new", "of", "return", "static", "super", "switch", "this", "throw", "try", "typeof",
    "var", "void", "while", "with", "yield")

  lazy val highlighter: SyntaxHighlighter = SyntaxHighlighter.build("javascript", "js")(
    Comment.singleLine("//"),
    Comment.multiLine("/*", "*/"),
    StringLiteral.singleLine('"').embed(
      unicodeCodePointEscape,
      StringLiteral.Escape.unicode,
      StringLiteral.Escape.hex,
      StringLiteral.Escape.char,
    ).build,
    StringLiteral.singleLine('\'').embed(
      unicodeCodePointEscape,
      StringLiteral.Escape.unicode,
      StringLiteral.Escape.hex,
      StringLiteral.Escape.char,
    ).build,
    StringLiteral.multiLine("`").embed(
      unicodeCodePointEscape,
      StringLiteral.Escape.unicode,
      StringLiteral.Escape.hex,
      StringLiteral.Escape.char,
      StringLiteral.Substitution.between("${", "}"),
    ).build,
    RegexLiteral.standard,
    Keywords(BooleanLiteral)("true", "false"),
    Keywords(LiteralValue)("null", "undefined", "NaN", "Infinity"),
    keywords,
    Identifier.standard.withIdStartChars('_','$').build,
    number(NumberLiteral.binary),
    number(NumberLiteral.octal),
    number(NumberLiteral.hex),
    number(NumberLiteral.decimalFloat),
    number(NumberLiteral.decimalInt),
  )
  
}
