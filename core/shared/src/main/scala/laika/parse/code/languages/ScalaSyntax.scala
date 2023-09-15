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
import laika.api.bundle.SyntaxHighlighter
import laika.parse.code.CodeCategory.{ BooleanLiteral, LiteralValue }
import laika.parse.code.{ CodeCategory, CodeSpanParser }
import laika.parse.code.common.{
  CharLiteral,
  Comment,
  Identifier,
  Keywords,
  NumberLiteral,
  StringLiteral
}
import laika.parse.text.{ CharGroup, PrefixedParser }
import laika.parse.builders._
import laika.parse.implicits._
import laika.parse.code.common.Identifier.IdParser
import laika.parse.code.implicits._

/** @author Jens Halm
  */
object ScalaSyntax extends SyntaxHighlighter {

  val language: NonEmptyList[String] = NonEmptyList.of("scala")

  private val comment: CodeSpanParser = Comment.singleLine("//") ++ Comment.multiLine("/*", "*/")

  private val symbol: CodeSpanParser = Identifier.alphaNum
    .withPrefix(literal("'"))
    .withCategory(CodeCategory.SymbolLiteral)

  private val backtickId: CodeSpanParser = CodeSpanParser(CodeCategory.Identifier) {
    (oneOf('`') ~ anyNot('\n', '`') ~ oneOf('`')).source
  }

  private val charEscapes: CodeSpanParser =
    StringLiteral.Escape.unicode ++ StringLiteral.Escape.char

  private val stringPrefixChar: PrefixedParser[String] = someOf(CharGroup.alpha)

  private val substitutions: CodeSpanParser =
    StringLiteral.Substitution.between("${", "}") ++
      StringLiteral.Substitution(("$" ~ someOf(CharGroup.alphaNum.add('_'))).source)

  private val stringLiteral: CodeSpanParser =
    StringLiteral.multiLine("\"\"\"") ++
      StringLiteral.multiLine((stringPrefixChar ~ "\"\"\"").source, literal("\"\"\"")).embed(
        substitutions
      ) ++
      StringLiteral.singleLine('"').embed(charEscapes) ++
      StringLiteral.singleLine((stringPrefixChar ~ "\"").source, literal("\"")).embed(
        charEscapes,
        substitutions
      )

  private val numberLiteral: CodeSpanParser =
    NumberLiteral.hex.withUnderscores.withSuffix(NumberLiteral.suffix.long) ++
      NumberLiteral.decimalFloat.withUnderscores.withSuffix(NumberLiteral.suffix.float) ++
      NumberLiteral.decimalInt.withUnderscores.withSuffix(
        NumberLiteral.suffix.long | NumberLiteral.suffix.float
      )

  /** Keywords for both Scala2 and Dotty/Scala3
    */
  private val keywords: CodeSpanParser =
    Keywords(BooleanLiteral)("true", "false") ++
      Keywords(LiteralValue)("null") ++
      Keywords(
        "abstract",
        "case",
        "catch",
        "class",
        "def",
        "else",
        "extends",
        "finally",
        "final",
        "for",
        "if",
        "implicit",
        "import",
        "lazy",
        "match",
        "new",
        "object",
        "override",
        "package",
        "private",
        "protected",
        "return",
        "sealed",
        "super",
        "this",
        "throw",
        "trait",
        "try",
        "type",
        "yield",
        "val",
        "var",
        "while",
        "with"
      )

  private val identifier: IdParser = Identifier.alphaNum
    .withIdStartChars('_', '$')
    .withCategoryChooser(Identifier.upperCaseTypeName)

  private val declaration: CodeSpanParser = CodeSpanParser {
    val keyword = literal("def").asCode(CodeCategory.Keyword)
    val name    = identifier.withCategory(CodeCategory.DeclarationName)
    (keyword ~ ws.asCode() ~ name).mapN { Seq(_, _, _) }
  }

  val spanParsers: Seq[CodeSpanParser] = Seq(
    comment,
    CharLiteral.standard.embed(charEscapes),
    symbol,
    backtickId,
    stringLiteral,
    JavaSyntax.annotation,
    declaration,
    keywords,
    Keywords(
      "break",
      "continue",
      "default",
      "forSome",
      "throws"
    ), // keywords removed in Dotty/Scala3
    identifier,
    numberLiteral
  )

  object Scala3 extends SyntaxHighlighter {

    val language: NonEmptyList[String] = NonEmptyList.of("dotty")

    /** Soft keywords would require too much context to be fully accurate.
      * For example, we do not even attempt to detect inline matches.
      * It should be sufficient to be right for the most basic cases.
      */
    private val softKeywords: CodeSpanParser = CodeSpanParser(CodeCategory.Keyword) {
      "inline" <~ lookAhead(ws ~ ("def" | "val" | "if")) |
        "opaque" <~ lookAhead(ws ~ "type") |
        "open" <~ lookAhead(ws ~ "class") |
        "infix " <~ lookAhead(ws ~ "def") |
        "transparent" <~ lookAhead(ws ~ "inline") |
        "as" <~ lookAhead(ws ~ identifier) |
        "derives" <~ lookAhead(ws ~ identifier) |
        "using" <~ lookAhead(ws ~ identifier) ~ lookBehind(6, literal("(")) |
        "extension" <~ lookAhead(ws ~ ("(" | "["))
    }

    val spanParsers: Seq[CodeSpanParser] = Seq(
      comment,
      CharLiteral.standard.embed(charEscapes),
      symbol,
      backtickId,
      stringLiteral,
      JavaSyntax.annotation,
      declaration,
      keywords,
      Keywords("do", "enum", "export", "given", "then"), // keywords added in Dotty/Scala3
      softKeywords,
      identifier,
      numberLiteral
    )

  }

}
