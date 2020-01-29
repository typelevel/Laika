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
import laika.parse.code.{CodeCategory, CodeSpanParsers}
import laika.parse.text.Characters
import laika.parse.text.TextParsers._

/**
  * @author Jens Halm
  */
object CSSSyntax extends SyntaxHighlighter {
  
  import NumberLiteral._
  
  private val ws: Characters[String] = anyOf('\n', ' ')
  
  private def idChars (category: CodeCategory, allowDigitBeforeStart: Boolean): Identifier.IdParser = 
    Identifier.alphaNum.withIdStartChars('_','-').withCategoryChooser(_ => category).copy(allowDigitBeforeStart = allowDigitBeforeStart)
  
  def identifier (category: CodeCategory, allowDigitBeforeStart: Boolean): CodeSpanParsers = {

    def prefixedId(prefix: String): CodeSpanParsers = CodeSpanParsers(prefix.head) {
      (literal(prefix) ~> idChars(category, allowDigitBeforeStart).standaloneParser).map { id =>
        Seq(id.copy(content = prefix + id.content)) // TODO - add prefixedBy() to Identifier parser
      }
    }
  
    prefixedId("@") ++ prefixedId("#") ++ idChars(category, allowDigitBeforeStart)
  }
  
  lazy val escape: CodeSpanParsers = 
    CodeSpanParsers(CodeCategory.EscapeSequence, '\\')(("\\" ~ DigitParsers.hex.min(1)).concat) ++ StringLiteral.Escape.char

  lazy val url: CodeSpanParsers = CodeSpanParsers('u') {
    (literal("url(") ~ ws ~ anyBut('"', '\'', '(', ')', ' ', '\n') ~ ws ~ ")").map {
      case _ ~ ws1 ~ value ~ ws2 ~ _ => Seq(
        CodeSpan("url", CodeCategory.Identifier),
        CodeSpan("(" + ws1),
        CodeSpan(value, CodeCategory.StringLiteral),
        CodeSpan(ws2 + ")"),
      )
    }
  }
  
  val color: CodeSpanParsers = CodeSpanParsers(CodeCategory.NumberLiteral, '#')(("#" ~ DigitParsers.hex.min(1).max(6)).concat)

  val string: CodeSpanParsers = StringLiteral.singleLine('"').embed(escape) ++
    StringLiteral.singleLine('\'').embed(escape)
  
  val number: CodeSpanParsers = NumberLiteral.decimalFloat.copy(allowFollowingLetter = true) ++ 
    NumberLiteral.decimalInt.copy(allowFollowingLetter = true)
  
  val declaration: CodeSpanParsers = {
    
    val embedded: Seq[CodeSpanParsers] = Seq(
      Comment.multiLine("/*", "*/"),
      string,
      color,
      url,
      number,
      identifier(CodeCategory.Identifier, allowDigitBeforeStart = true),
    )
    def valueParser(inBlock: Boolean): Parser[Seq[CodeSpan]] = {
      val separator = (ws ~ ":").concat.map(CodeSpan(_))
      val delim = any.take(1).map(CodeSpan(_))
      val text = if (inBlock) delimitedBy('}',';', ')') else delimitedBy('}',';')
      (separator ~ EmbeddedCodeSpans.parser(text.keepDelimiter, embedded) ~ delim).map {
        case sep ~ con ~ del => sep +: con :+ del
      }
    }

    // TODO - add prefixedBy() to Identifier parser
    val attrName = idChars(CodeCategory.AttributeName, allowDigitBeforeStart = false)
    CodeSpanParsers(attrName.idStartChars) {
      val attribute = (any.take(1) ~ attrName.idRestParser).concat.map(CodeSpan(_, CodeCategory.AttributeName))
      (attribute ~ valueParser(inBlock = false)).concat
    } ++
    CodeSpanParsers('(') {
      val attribute = attrName.standaloneParser
      ("(" ~> attribute ~ valueParser(inBlock = true)).concat.map(spans => CodeSpan("(") +: spans)
    }
  }
  
  val language: NonEmptyList[String] = NonEmptyList.of("css")

  val spanParsers: Seq[CodeSpanParsers] = Seq(
    Comment.multiLine("/*", "*/"),
    string,
    declaration,
    identifier(CodeCategory.Identifier, allowDigitBeforeStart = false),
    Keywords("!important")
  )

}
