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
import laika.parse.code.CodeSpanParser
import laika.parse.code.common.{ Comment, Identifier, Keywords, NumberLiteral }
import laika.parse.text.CharGroup.{ digit, lowerAlpha, upperAlpha }

/** https://alloytools.org/download/alloy-language-reference.pdf
  *
  * @author Michał Sitko
  */
object AlloySyntax extends SyntaxHighlighter {

  /** The names of the language (and its optional aliases) as used in text markup */
  override def language: NonEmptyList[String] = NonEmptyList.of("alloy")

  val comment: CodeSpanParser =
    Comment.singleLine("--") ++ Comment.singleLine("//") ++ Comment.multiLine("/*", "*/")

  val keywords = Keywords(
    "abstract",
    "all",
    "and",
    "as",
    "assert",
    "but",
    "check",
    "disj",
    "else",
    "exactly",
    "extends",
    "fact",
    "for",
    "fun",
    "iden",
    "iff",
    "implies",
    "in",
    "Int",
    "let",
    "lone",
    "module",
    "no",
    "none",
    "not",
    "one",
    "open",
    "or",
    "pred",
    "run",
    "set",
    "sig",
    "some",
    "sum",
    "univ",
    "enum"
  )

  val numberLiteral = NumberLiteral.decimalInt

  val identifiers =
    Identifier.forCharacterSets(lowerAlpha ++ upperAlpha, digit.add('_').add('\'').add('"'))

  /** The parsers for individual code spans written in this language */
  override def spanParsers: Seq[CodeSpanParser] = Seq(
    comment,
    keywords,
    identifiers,
    numberLiteral
  )

}
