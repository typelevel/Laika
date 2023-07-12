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
import laika.parse.code.CodeCategory.{ BooleanLiteral, LiteralValue }
import laika.parse.code.common.NumberLiteral.{ digits, NumericParser }
import laika.parse.code.common._
import laika.parse.code.{ CodeCategory, CodeSpanParser }
import laika.parse.implicits._

/** @author Jens Halm
  */
object JavaScriptSyntax extends SyntaxHighlighter {

  private[languages] val unicodeCodePointEscape: CodeSpanParser =
    CodeSpanParser(CodeCategory.EscapeSequence) {
      ("\\u{" ~ digits.hex.min(1) ~ "}").source
    }

  private val charEscapes: CodeSpanParser =
    unicodeCodePointEscape ++
      StringLiteral.Escape.unicode ++
      StringLiteral.Escape.hex ++
      StringLiteral.Escape.char

  private[languages] def number(parser: NumericParser): CodeSpanParser =
    parser.withUnderscores.withSuffix(NumberLiteral.suffix.bigInt)

  private[languages] val keywords = Keywords(
    "async",
    "as",
    "await",
    "break",
    "case",
    "catch",
    "class",
    "const",
    "continue",
    "debugger",
    "default",
    "delete",
    "do",
    "else",
    "export",
    "extends",
    "finally",
    "for",
    "from",
    "function",
    "if",
    "import",
    "instanceof",
    "in",
    "let",
    "new",
    "of",
    "return",
    "static",
    "super",
    "switch",
    "this",
    "throw",
    "try",
    "typeof",
    "var",
    "void",
    "while",
    "with",
    "yield"
  )

  val language: NonEmptyList[String] = NonEmptyList.of("javascript", "js")

  val spanParsers: Seq[CodeSpanParser] = Seq(
    Comment.singleLine("//"),
    Comment.multiLine("/*", "*/"),
    StringLiteral.singleLine('"').embed(charEscapes),
    StringLiteral.singleLine('\'').embed(charEscapes),
    StringLiteral.multiLine("`").embed(
      charEscapes,
      StringLiteral.Substitution.between("${", "}")
    ),
    RegexLiteral.standard,
    Keywords(BooleanLiteral)("true", "false"),
    Keywords(LiteralValue)("null", "undefined", "NaN", "Infinity"),
    keywords,
    Identifier.alphaNum.withIdStartChars('_', '$'),
    number(NumberLiteral.binary),
    number(NumberLiteral.octal),
    number(NumberLiteral.hex),
    number(NumberLiteral.decimalFloat),
    number(NumberLiteral.decimalInt)
  )

  object JSX extends SyntaxHighlighter {

    import TagFormats._

    val language: NonEmptyList[String] = NonEmptyList.of("jsx")

    private def tagCategory(name: String): CodeCategory =
      if (name.head.isUpper) CodeCategory.TypeName else CodeCategory.Tag.Name

    private[languages] val emptyJsxTag: CodeSpanParser = emptyTag.withCategory(tagCategory(_))

    private[languages] lazy val element: CodeSpanParser = CodeSpanParser {

      val substitution = StringLiteral.Substitution.between("{", "}")

      val startTag = customTag("<", ">")
        .withCategory(tagCategory(_))
        .embed(
          stringWithEntities,
          name(CodeCategory.AttributeName),
          substitution
        )

      val embedded = Seq(
        CodeSpanParser.recursive(element),
        emptyJsxTag,
        substitution
      )

      startTag >> { startSpans =>
        val tagName = startSpans.tail.head.content
        elementRest(tagName, embedded, tagCategory(tagName)).map(startSpans ++ _)
      }
    }

    lazy val spanParsers: Seq[CodeSpanParser] = JavaScriptSyntax.spanParsers ++ Seq(
      element,
      emptyJsxTag
    )

  }

}
