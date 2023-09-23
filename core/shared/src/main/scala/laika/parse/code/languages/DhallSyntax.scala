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
import laika.ast.CodeSpan
import laika.parse.code.common.NumberLiteral.digits
import laika.parse.code.common._
import laika.parse.code.syntax._
import laika.parse.code.{ CodeCategory, CodeSpanParser }
import laika.parse.builders.~
import laika.parse.syntax._
import laika.parse.text.{ CharGroup, PrefixedParser }
import laika.parse.text.TextParsers._

/** https://github.com/dhall-lang/dhall-lang/blob/master/standard/dhall.abnf
  * @author Michał Sitko
  */
object DhallSyntax extends SyntaxHighlighter {

  /** The names of the language (and its optional aliases) as used in text markup */
  override def language: NonEmptyList[String] = NonEmptyList.of("dhall")

  private val comment: CodeSpanParser =
    Comment.singleLine("--") ++ Comment.multiLine("{-", "-}")

  private val keywords: CodeSpanParser = Keywords(
    "if",
    "then",
    "else",
    "let",
    "in",
    "using",
    "missing",
    "assert",
    "as",
    "Infinity",
    "NaN",
    "merge",
    "Some",
    "toMap",
    "forall-keyword",
    "with"
  )

  private val bracedUnicodeEscape: CodeSpanParser = CodeSpanParser(CodeCategory.EscapeSequence) {
    ("\\u{" ~ anyOf('0') ~ digits.hex.min(1).max(6) ~ "}").source
  }

  private val singleLineEscapes: CodeSpanParser =
    bracedUnicodeEscape ++ StringLiteral.Escape.unicode ++ StringLiteral.Escape.char

  private val multiLineEscapes: CodeSpanParser =
    Keywords(CodeCategory.EscapeSequence)("'''", "''${")

  val substitutions: CodeSpanParser = StringLiteral.Substitution.between("${", "}")

  private val stringLiteral: CodeSpanParser =
    StringLiteral.singleLine('"').embed(substitutions, singleLineEscapes) ++
      StringLiteral.multiLine("''").embed(substitutions, multiLineEscapes)

  private val numberLiteral: CodeSpanParser = NumberLiteral.hex ++
    NumberLiteral.decimalFloat ++
    NumberLiteral.decimalInt.withPrefix(someOf('-', '+').max(1)) ++
    NumberLiteral.decimalInt

  private val identifier: Identifier.IdParser = Identifier.alphaNum

  private val anyOfWs = anyOf(' ', '\t', '\n')

  private val tpe: PrefixedParser[Seq[CodeSpan]] = {
    val t             = someOf(CharGroup.alphaNum.add('_'))
    val nonKindedType = (t ~ ("." ~ t).rep).map { case a ~ seq =>
      seq.lastOption match {
        case Some(id ~ typeName) =>
          CodeSpan(a, CodeCategory.Identifier) +:
            (seq.dropRight(1).flatMap { case sep ~ id =>
              Seq(CodeSpan(sep), CodeSpan(id, CodeCategory.Identifier))
            } ++
              Seq(CodeSpan(id), CodeSpan(typeName, CodeCategory.TypeName)))
        case None                =>
          Seq(CodeSpan(a, CodeCategory.TypeName))
      }
    }

    (nonKindedType ~ ((" -> " | someOf(' ', '\t')).source.asCode() ~ nonKindedType).rep.map(
      _.flatMap { case h ~ t =>
        h +: t
      }
    )).concat
  }

  private val beginningOfLet =
    (literal("let").asCode(CodeCategory.Keyword) ~ someOf(' ', '\t').asCode() ~
      Identifier.alphaNum.withCategory(CodeCategory.DeclarationName)).mapN(Seq(_, _, _))

  private val equals = (anyOfWs ~ "=").source.map(s => Seq(CodeSpan(s)))

  private val colon = (anyOfWs ~ ": " ~ anyOfWs).source.map(s => Seq(CodeSpan(s)))

  private val typedDeclaration: CodeSpanParser = CodeSpanParser(
    (beginningOfLet ~ colon ~ tpe ~ equals).concat
  )

  private val untypedDeclaration: CodeSpanParser = CodeSpanParser((beginningOfLet ~ equals).concat)

  private val attributeName: Identifier.IdParser =
    identifier.withCategory(CodeCategory.AttributeName)

  private val recordEntry: CodeSpanParser = CodeSpanParser(
    (attributeName ~ (anyOfWs ~ oneOf('=')).source.asCode()).mapN(Seq(_, _))
  )

  private val recordTypeEntry: CodeSpanParser =
    CodeSpanParser((attributeName ~ colon ~ tpe).concat)

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
