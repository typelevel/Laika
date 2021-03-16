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
import cats.implicits._
import laika.ast.~
import laika.ast.CodeSpan
import laika.bundle.SyntaxHighlighter
import laika.parse.code.common.NumberLiteral.NumericParser
import laika.parse.code.common._
import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.implicits._
import laika.parse.text.{CharGroup, PrefixedParser}
import laika.parse.text.TextParsers._

/**
 * https://github.com/dhall-lang/dhall-lang/blob/master/standard/dhall.abnf
 * @author Michał Sitko
 */
object DhallSyntax extends SyntaxHighlighter {
  /** The names of the language (and its optional aliases) as used in text markup */
  override def language: NonEmptyList[String] = NonEmptyList.of("dhall")

  val comment =
    Comment.singleLine("--") ++ Comment.multiLine("{-", "-}")

  val keywords = Keywords(
    "if", "then", "else", "let", "in",
    "using", "missing", "assert", "as",
    "Infinity", "NaN", "merge", "Some",
    "toMap", "forall-keyword", "with"
  )

  val stringLiteral = StringLiteral.singleLine('"') ++ StringLiteral.multiLine("''")

  val numberLiteral = NumberLiteral.hex ++
    NumberLiteral.decimalFloat ++
    NumericParser(CharGroup.digit, someOf('-', '+').max(1).some) ++
    NumberLiteral.decimalInt

  val identifier = Identifier.alphaNum

  val anyOfWs = anyOf(' ', '\t', '\n')

  val tpe: PrefixedParser[Seq[CodeSpan]] = {
    val t = someOf(CharGroup.alphaNum.add('_'))
    val nonKindedType = (t ~ ("." ~ t).rep).map { case a ~ seq =>
      seq.lastOption match {
        case Some(id ~ typeName) =>
          CodeSpan(a, CodeCategory.Identifier) +:
            (seq.dropRight(1).flatMap { case sep ~ id => Seq(CodeSpan(sep), CodeSpan(id, CodeCategory.Identifier)) } ++
            Seq(CodeSpan(id), CodeSpan(typeName, CodeCategory.TypeName)))
        case None =>
          Seq(CodeSpan(a, CodeCategory.TypeName))
      }
    }

    (nonKindedType ~ ((" -> " | someOf(' ', '\t')).source.map(CodeSpan(_)) ~ nonKindedType).rep.map(_.flatMap { case h ~ t =>
      h +: t
    })).concat
  }

  val beginningOfLet =
    ((literal("let").map(s => Seq(CodeSpan(s, CodeCategory.Keyword))) ~ someOf(' ', '\t').map(s => Seq(CodeSpan(s)))).concat ~
      Identifier.alphaNum.withCategory(CodeCategory.DeclarationName).map(Seq(_))).concat

  val equals = (anyOfWs ~ "=").source.map(s => Seq(CodeSpan(s)))

  val colon = (anyOfWs ~ ": " ~ anyOfWs).source.map(s => Seq(CodeSpan(s)))
  val typedDeclaration =
    CodeSpanParser((beginningOfLet ~ colon ~ tpe ~ equals).concat)

  val untypedDeclaration =
    CodeSpanParser((beginningOfLet ~ equals).concat)

  val attrName = identifier.withCategory(CodeCategory.AttributeName)

  val recordEntry = CodeSpanParser((attrName ~ (anyOfWs ~ oneOf('=')).source.map(CodeSpan(_))).mapN(Seq(_, _)))

  val recordTypeEntry =
    CodeSpanParser((attrName ~ colon ~ tpe).concat)

  /** The parsers for individual code spans written in this language */
  override def spanParsers: Seq[CodeSpanParser] = Seq(
    comment,
    stringLiteral,
    typedDeclaration,
    untypedDeclaration,
    recordTypeEntry,
    recordEntry,
    keywords,
    identifier,
    numberLiteral
  )

}
