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
import laika.ast.{ CodeSpan, ~ }
import laika.bundle.SyntaxHighlighter
import laika.parse.Parser
import laika.parse.code.common._
import laika.parse.code.{ CodeCategory, CodeSpanParser }
import laika.parse.text.Characters
import laika.parse.builders._
import laika.parse.code.common.NumberLiteral.digits
import laika.parse.code.implicits._
import laika.parse.implicits._

/** @author Jens Halm
  */
object CSSSyntax extends SyntaxHighlighter {

  private val ws: Characters[String] = anyOf('\n', ' ')

  private def idBase(category: CodeCategory): Identifier.IdParser =
    Identifier.alphaNum
      .withIdStartChars('_', '-')
      .withCategory(category)

  private def identifier(category: CodeCategory): CodeSpanParser =
    identifier(idBase(category))

  private def identifier(base: Identifier.IdParser): CodeSpanParser = {
    base.withPrefix("@" | "#") ++ base
  }

  private lazy val escape: CodeSpanParser =
    CodeSpanParser(CodeCategory.EscapeSequence)(
      ("\\" ~ digits.hex.min(1)).source
    ) ++ StringLiteral.Escape.char

  private lazy val url: CodeSpanParser = CodeSpanParser {
    (literal("url(") ~ ws ~ anyNot('"', '\'', '(', ')', ' ', '\n') ~ ws ~ ")").map {
      case _ ~ ws1 ~ value ~ ws2 ~ _ =>
        Seq(
          CodeSpan("url", CodeCategory.Identifier),
          CodeSpan("(" + ws1),
          CodeSpan(value, CodeCategory.StringLiteral),
          CodeSpan(ws2 + ")")
        )
    }
  }

  private val color: CodeSpanParser =
    CodeSpanParser(CodeCategory.NumberLiteral)(("#" ~ digits.hex.min(1).max(6)).source)

  private val string: CodeSpanParser = StringLiteral.singleLine('"').embed(escape) ++
    StringLiteral.singleLine('\'').embed(escape)

  private val number: CodeSpanParser = NumberLiteral.decimalFloat.allowFollowingLetter ++
    NumberLiteral.decimalInt.allowFollowingLetter

  private val declaration: CodeSpanParser = {

    val embedded: Seq[CodeSpanParser]                        = Seq(
      Comment.multiLine("/*", "*/"),
      string,
      color,
      url,
      number,
      identifier(idBase(CodeCategory.Identifier).allowDigitBeforeStart)
    )
    def valueParser(inBlock: Boolean): Parser[Seq[CodeSpan]] = {
      val separator = (ws ~ ":").source.asCode()
      val delim     = oneChar.asCode()
      val text      = if (inBlock) delimitedBy('}', ';', ')') else delimitedBy('}', ';')
      (separator ~ EmbeddedCodeSpans.parser(text.keepDelimiter, embedded) ~ delim).map {
        case sep ~ con ~ del => sep +: con :+ del
      }
    }

    val attrName = idBase(CodeCategory.AttributeName)
    CodeSpanParser {
      (attrName ~ valueParser(inBlock = false)).concat
    } ++
      CodeSpanParser {
        ("(" ~> attrName ~ valueParser(inBlock = true)).concat.map(spans => CodeSpan("(") +: spans)
      }
  }

  val language: NonEmptyList[String] = NonEmptyList.of("css")

  val spanParsers: Seq[CodeSpanParser] = Seq(
    Comment.multiLine("/*", "*/"),
    string,
    declaration,
    identifier(CodeCategory.Identifier),
    Keywords("!important")
  )

}
