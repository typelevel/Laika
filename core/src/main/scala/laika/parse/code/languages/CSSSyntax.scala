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
import laika.ast.{CodeSpan, ~}
import laika.bundle.SyntaxHighlighter
import laika.parse.Parser
import laika.parse.code.common._
import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.text.Characters
import laika.parse.api._

/**
  * @author Jens Halm
  */
object CSSSyntax extends SyntaxHighlighter {
  
  import NumberLiteral._
  
  private val ws: Characters[String] = anyOf('\n', ' ')
  
  private def idBase (category: CodeCategory, allowDigitBeforeStart: Boolean): Identifier.IdParser = 
    Identifier.alphaNum
      .withIdStartChars('_','-')
      .withCategory(category)
      .copy(allowDigitBeforeStart = allowDigitBeforeStart)
  
  def identifier (category: CodeCategory, allowDigitBeforeStart: Boolean): CodeSpanParser = {
    val base = idBase(category, allowDigitBeforeStart)
    base.withPrefix("@" | "#") ++ base
  }
  
  lazy val escape: CodeSpanParser = 
    CodeSpanParser(CodeCategory.EscapeSequence)(("\\" ~ DigitParsers.hex.min(1)).source) ++ StringLiteral.Escape.char

  lazy val url: CodeSpanParser = CodeSpanParser {
    (literal("url(") ~ ws ~ anyNot('"', '\'', '(', ')', ' ', '\n') ~ ws ~ ")").map {
      case _ ~ ws1 ~ value ~ ws2 ~ _ => Seq(
        CodeSpan("url", CodeCategory.Identifier),
        CodeSpan("(" + ws1),
        CodeSpan(value, CodeCategory.StringLiteral),
        CodeSpan(ws2 + ")"),
      )
    }
  }
  
  val color: CodeSpanParser = CodeSpanParser(CodeCategory.NumberLiteral)(("#" ~ DigitParsers.hex.min(1).max(6)).source)

  val string: CodeSpanParser = StringLiteral.singleLine('"').embed(escape) ++
    StringLiteral.singleLine('\'').embed(escape)
  
  val number: CodeSpanParser = NumberLiteral.decimalFloat.copy(allowFollowingLetter = true) ++ 
    NumberLiteral.decimalInt.copy(allowFollowingLetter = true)
  
  val declaration: CodeSpanParser = {
    
    val embedded: Seq[CodeSpanParser] = Seq(
      Comment.multiLine("/*", "*/"),
      string,
      color,
      url,
      number,
      identifier(CodeCategory.Identifier, allowDigitBeforeStart = true),
    )
    def valueParser(inBlock: Boolean): Parser[Seq[CodeSpan]] = {
      val separator = (ws ~ ":").source.map(CodeSpan(_))
      val delim = oneChar.map(CodeSpan(_))
      val text = if (inBlock) delimitedBy('}',';', ')') else delimitedBy('}',';')
      (separator ~ EmbeddedCodeSpans.parser(text.keepDelimiter, embedded) ~ delim).map {
        case sep ~ con ~ del => sep +: con :+ del
      }
    }

    val attrName = idBase(CodeCategory.AttributeName, allowDigitBeforeStart = false)
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
    identifier(CodeCategory.Identifier, allowDigitBeforeStart = false),
    Keywords("!important")
  )
  
}
