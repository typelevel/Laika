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
import laika.parse.code.common.Identifier.IdParser
import laika.parse.code.common.{CharLiteral, Comment, Keywords, NumberLiteral, StringLiteral}
import laika.parse.code.languages.ScalaSyntax.charEscapes
import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.text.CharGroup.{digit, lowerAlpha, upperAlpha}

/**
 * @author Michał Sitko
 */
object HaskellSyntax extends SyntaxHighlighter {
  /** The names of the language (and its optional aliases) as used in text markup */
  override def language: NonEmptyList[String] = NonEmptyList.of("hs", "haskell")

  val comment: CodeSpanParser = Comment.singleLine("--") ++ Comment.multiLine("{-", "-}")

  val keywords =
    Keywords("as", "case", "data", "data family", "data instance", "default", "deriving",
      "deriving instance", "do", "else", "forall", "foreign", "hiding", "if", "import", "in",
      "infix", "infixl", "infixr", "instance", "let", "mdo", "module", "newtype", "of",
      "proc", "qualified", "rec", "then", "type", "type family", "type instance", "where"
    )

  val stringLiteral =
    StringLiteral.singleLine('"').embed(charEscapes)

  val numberLiteral =
    NumberLiteral.hex ++ NumberLiteral.octal ++ NumberLiteral.decimalFloat ++ NumberLiteral.decimalInt

  val identifiers = IdParser(lowerAlpha.add('_'), digit ++ upperAlpha.add('\''))

  val types = IdParser(upperAlpha.add('_'), digit ++ lowerAlpha.add('\'')).withCategory(CodeCategory.TypeName)

  /** The parsers for individual code spans written in this language */
  override def spanParsers: Seq[CodeSpanParser] = Seq(
    comment,
    CharLiteral.standard.embed(StringLiteral.Escape.char),
    stringLiteral,
    keywords,
    identifiers,
    types,
    numberLiteral
  )
}
