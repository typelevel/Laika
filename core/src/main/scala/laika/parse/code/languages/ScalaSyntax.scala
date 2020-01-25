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
import laika.parse.code.{CodeCategory, CodeSpanParsers}
import laika.parse.code.common.{CharLiteral, Comment, Identifier, Keywords, NumberLiteral, NumericSuffix, StringLiteral}
import laika.parse.text.TextParsers._

/**
  * @author Jens Halm
  */
object ScalaSyntax extends SyntaxHighlighter {

  import NumberLiteral._
  
  private val interpolatedStartChars: Set[Char] = ('a' to 'z').toSet ++ ('A' to 'Z').toSet
  
  val symbolParser: CodeSpanParsers = CodeSpanParsers('\'') {
    Identifier.standard
      .withCategoryChooser(_ => CodeCategory.SymbolLiteral)
      .standaloneParser
      .map(Seq(_))
  }
  
  val backtickIdParser: CodeSpanParsers = CodeSpanParsers(CodeCategory.Identifier, '`') {
    (anyBut('\n', '`') ~ anyOf('`').take(1)).concat
  }

  val language: NonEmptyList[String] = NonEmptyList.of("scala")

  val spanParsers: Seq[CodeSpanParsers] = Seq(
    Comment.singleLine("//"),
    Comment.multiLine("/*", "*/"),
    CharLiteral.standard.embed(
      StringLiteral.Escape.unicode,
      StringLiteral.Escape.char
    ),
    symbolParser,
    backtickIdParser,
    StringLiteral.multiLine("\"\"\""),
    StringLiteral.multiLine(interpolatedStartChars, "\"\"\"").withPrefix((anyIn('a' to 'z', 'A' to 'Z') ~ "\"\"\"").concat).embed(
      StringLiteral.Escape.literal("$$"),
      StringLiteral.Substitution.between("${", "}"),
      StringLiteral.Substitution('$')(anyIn('a' to 'z', 'A' to 'Z', '0' to '9', '_').min(1))
    ),
    StringLiteral.singleLine('"').embed(
      StringLiteral.Escape.unicode,
      StringLiteral.Escape.char,
    ),
    StringLiteral.singleLine(interpolatedStartChars, '\"').withPrefix((anyIn('a' to 'z', 'A' to 'Z') ~ "\"").concat).embed(
      StringLiteral.Escape.unicode,
      StringLiteral.Escape.char,
      StringLiteral.Escape.literal("$$"),
      StringLiteral.Substitution.between("${", "}"),
      StringLiteral.Substitution('$')(anyIn('a' to 'z', 'A' to 'Z', '0' to '9', '_').min(1))
    ),
    Keywords(BooleanLiteral)("true", "false"),
    Keywords(LiteralValue)("null"),
    Keywords("abstract", "break", "case", "catch", "class", "continue", "default", "def", "else", "extends",
      "finally", "final", "forSome", "for", "if", "implicit", "import", "lazy", "match",
      "new", "object", "override", "package", "private", "protected", "return", "sealed", "super",
      "this", "throw", "throws", "trait", "try", "type", "yield", "val", "var", "while", "with"),
    Identifier.standard.withIdStartChars('_','$').withCategoryChooser(Identifier.upperCaseTypeName),
    NumberLiteral.hex.withUnderscores.withSuffix(NumericSuffix.long),
    NumberLiteral.decimalFloat.withUnderscores.withSuffix(NumericSuffix.float),
    NumberLiteral.decimalInt.withUnderscores.withSuffix(NumericSuffix.long | NumericSuffix.float),
  )
  
}
