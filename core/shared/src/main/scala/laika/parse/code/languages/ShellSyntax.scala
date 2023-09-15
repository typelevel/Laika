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
import laika.parse.builders._
import laika.parse.code.CodeCategory.BooleanLiteral
import laika.parse.code.{ CodeCategory, CodeSpanParser }
import laika.parse.code.common.{ Comment, Keywords, NumberLiteral, StringLiteral }
import laika.parse.implicits._
import laika.parse.text.CharGroup

object ShellSyntax extends SyntaxHighlighter {

  val language: NonEmptyList[String] = NonEmptyList.of[String]("shell", "sh", "bash")

  /** A backslash cannot be used to escape a single-quote in a single-quoted string.
    */
  private val singleQuoteEscape = CodeSpanParser(CodeCategory.EscapeSequence) {
    ("\\" ~ anyNot('\'')).source
  }

  /** The backslash retains its special meaning as an escape character only when followed by one of the characters:
    *  $   `   "   \   <newline>
    *
    * for example, `"\$"` is turned into `$`, but `"\a"` remains `"\a"`
    */
  private val doubleQuoteEscape = CodeSpanParser(CodeCategory.EscapeSequence) {
    ("\\" ~ oneOf('$', '`', '"', '\\')).source
  }

  private val substitutions = Seq(
    StringLiteral.Substitution.between("$((", "))"),
    StringLiteral.Substitution.between("$(", ")"),
    StringLiteral.Substitution.between("${", "}"),
    StringLiteral.Substitution(("$" ~ someOf(CharGroup.alphaNum.add('_'))).source),
    StringLiteral.Substitution(("$" ~ oneOf('@', '*', '#', '?', '-', '$', '!')).source)
  )

  val spanParsers: Seq[CodeSpanParser] = Seq(
    Comment.singleLine("#"),
    Keywords(BooleanLiteral)("true", "false"),
    Keywords(
      "if",
      "then",
      "else",
      "elif",
      "fi",
      "case",
      "esac",
      "for",
      "select",
      "while",
      "until",
      "do",
      "done",
      "in",
      "function",
      "time"
    ),
    Keywords(CodeCategory.Identifier)(
      "alias",
      "break",
      "builtin",
      "cd",
      "command",
      "continue",
      "declare",
      "echo",
      "eval",
      "exec",
      "exit",
      "export",
      "kill",
      "let",
      "logout",
      "printf",
      "pwd",
      "read",
      "return",
      "set",
      "source",
      "type",
      "ulimit",
      "umask",
      "unalias",
      "unset",
      "wait"
    ),
    NumberLiteral.decimalInt,
    NumberLiteral.decimalFloat,
    StringLiteral.singleLine('\'').embed(
      singleQuoteEscape
    ),
    StringLiteral.singleLine('"').embed(
      (substitutions :+ doubleQuoteEscape): _*
    )
  ) ++ substitutions

}
