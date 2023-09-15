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
import laika.parse.implicits._
import laika.parse.code.CodeCategory.{ BooleanLiteral, LiteralValue, TypeName }
import laika.parse.code.{ CodeCategory, CodeSpanParser }
import laika.parse.code.common.{
  CharLiteral,
  Comment,
  Identifier,
  Keywords,
  NumberLiteral,
  StringLiteral
}

/** @author Jens Halm
  */
object JavaSyntax extends SyntaxHighlighter {

  val language: NonEmptyList[String] = NonEmptyList.of("java")

  private[languages] val annotation: CodeSpanParser = CodeSpanParser {
    "@" ~> Identifier.alphaNum.withCategory(CodeCategory.Annotation).map { name =>
      Seq(name.copy(content = "@" + name.content))
    }
  }

  private val charEscapes: CodeSpanParser =
    StringLiteral.Escape.unicode ++
      StringLiteral.Escape.octal ++
      StringLiteral.Escape.char

  val spanParsers: Seq[CodeSpanParser] = Seq(
    Comment.singleLine("//"),
    Comment.multiLine("/*", "*/"),
    CharLiteral.standard.embed(charEscapes),
    StringLiteral.singleLine('"').embed(charEscapes),
    annotation,
    Keywords(BooleanLiteral)("true", "false"),
    Keywords(LiteralValue)("null"),
    Keywords(TypeName)("boolean", "byte", "char", "double", "float", "int", "long", "short"),
    Keywords(
      "abstract",
      "assert",
      "break",
      "case",
      "catch",
      "class",
      "const",
      "continue",
      "default",
      "do",
      "else",
      "enum",
      "extends",
      "finally",
      "final",
      "for",
      "if",
      "implements",
      "import",
      "instanceof",
      "interface",
      "module",
      "native",
      "new",
      "package",
      "private",
      "protected",
      "public",
      "requires",
      "return",
      "static",
      "strictfp",
      "super",
      "switch",
      "synchronized",
      "this",
      "throws",
      "throw",
      "transient",
      "try",
      "var",
      "void",
      "volatile",
      "while"
    ),
    Identifier.alphaNum.withIdStartChars('_', '$').withCategoryChooser(
      Identifier.upperCaseTypeName
    ),
    NumberLiteral.binary.withUnderscores.withSuffix(NumberLiteral.suffix.long),
    NumberLiteral.octal.withUnderscores.withSuffix(NumberLiteral.suffix.long),
    NumberLiteral.hexFloat.withUnderscores.withSuffix(NumberLiteral.suffix.float),
    NumberLiteral.hex.withUnderscores.withSuffix(NumberLiteral.suffix.long),
    NumberLiteral.decimalFloat.withUnderscores.withSuffix(NumberLiteral.suffix.float),
    NumberLiteral.decimalInt.withUnderscores.withSuffix(
      NumberLiteral.suffix.long | NumberLiteral.suffix.float
    )
  )

}
