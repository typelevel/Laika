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
import cats.data.NonEmptyList
import laika.parse.code.CodeSpanParser
import laika.parse.implicits._
import laika.parse.code.CodeCategory.{ BooleanLiteral, LiteralValue }
import laika.parse.code.common.{ Comment, Identifier, Keywords, NumberLiteral, StringLiteral }
import laika.parse.code.implicits._
import laika.parse.builders._
import laika.parse.code.CodeCategory
import laika.bundle.SpanParser
import laika.ast.CodeSpans
import scala.io.Codec
import laika.ast.CodeSpan
import laika.ast.NoOpt
import laika.ast.CodeSpanSequence
import laika.parse.code.common.EmbeddedCodeSpans
import laika.parse.code.common.TagParser
import laika.parse.markup.RecursiveSpanParser
import laika.parse.markup.RecursiveSpanParsers
import laika.parse.markup.RecursiveParsers

object ShellSyntax extends SyntaxHighlighter {

  val language: NonEmptyList[String] = NonEmptyList.of[String]("sh")

  /** A backslash cannot be used to escape a single-quote in a single-quoted string.
    */
  val singleQuoteEscape = CodeSpanParser(CodeCategory.EscapeSequence) {
    ("\\" ~ anyNot('\'')).source
  }

  /** The backslash retains its special meaning as an escape character (see Escape Character (Backslash) ) only when followed by one of the characters:
    *  $   `   "   \   <newline>
    *
    * for example, `"\$"` is turned into `$`, but `"\a"` remains `"\a"`
    */
  val doubleQuoteEscape                = CodeSpanParser(CodeCategory.EscapeSequence) {
    ("\\" ~ oneOf('$', '`', '"', '\\')).source
  }

  val spanParsers: Seq[CodeSpanParser] = Seq(
    Comment.singleLine("#"),
    // note: function and select are reserved word in some systems.
    Keywords(BooleanLiteral)("true", "false"),
    Keywords(
      "break",
      "case",
      "continue",
      "do",
      "done",
      "echo",
      "else",
      "esac",
      "eval",
      "exec",
      "exit",
      "export",
      "fi",
      "for",
      "function",
      "if",
      "read",
      "readonly",
      "return",
      "set",
      "select",
      "shift",
      "then",
      "trap",
      "unlimit",
      "unmask",
      "unset",
      "until",
      "wait",
      "while",
      "!"
    ),
    Identifier.alphaNum,
    NumberLiteral.decimalInt,
    NumberLiteral.decimalFloat,
    StringLiteral.Substitution.between("$(", ")"),
    StringLiteral.Substitution.between("${", "}"),
    StringLiteral.singleLine('\'').embed(
      singleQuoteEscape
    ),
    StringLiteral.singleLine('"').embed(
      doubleQuoteEscape,
      StringLiteral.Substitution.between("${", "}")
    ),
    StringLiteral.singleLine('\''),
    StringLiteral.singleLine('"')
  )

}
