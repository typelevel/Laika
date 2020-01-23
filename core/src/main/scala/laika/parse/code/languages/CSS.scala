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

import laika.ast.~
import laika.bundle.SyntaxHighlighter
import laika.parse.Parser
import laika.parse.code.{CodeCategory, CodeSpan, CodeSpanParsers}
import laika.parse.code.common.{Comment, EmbeddedCodeSpans, Identifier, Keywords, NumberLiteral, StringLiteral}
import laika.parse.text.Characters
import laika.parse.text.TextParsers._

/**
  * @author Jens Halm
  */
object CSS {
  
  import NumberLiteral._
  
  private val ws: Characters[String] = anyOf('\n', ' ')
  
  private def idChars (category: CodeCategory): Identifier.IdParser = 
    Identifier.standard.withIdStartChars('_','-').withCategoryChooser(_ => category)
  
  def identifier (category: CodeCategory): CodeSpanParsers = {

    def prefixedId(prefix: String): CodeSpanParsers = CodeSpanParsers(prefix.head) {
      (literal(prefix.tail) ~> idChars(category).standaloneParser).map { id =>
        Seq(id.copy(content = prefix + id.content)) // TODO - add prefixedBy() to Identifier parser
      }
    }
  
    prefixedId("@") ++ prefixedId("#") ++ idChars(category).build
  }
  
  lazy val escape: CodeSpanParsers = 
    CodeSpanParsers(CodeCategory.EscapeSequence, '\\')(DigitParsers.hex.min(1)) ++ StringLiteral.Escape.char

  lazy val url: CodeSpanParsers = CodeSpanParsers('u') {
    (literal("rl(") ~ ws ~ anyBut('"', '\'', '(', ')', ' ', '\n') ~ ws ~ ")").map {
      case _ ~ ws1 ~ value ~ ws2 ~ _ => Seq(
        CodeSpan("url", CodeCategory.Identifier),
        CodeSpan("(" + ws1),
        CodeSpan(value, CodeCategory.StringLiteral),
        CodeSpan(ws2 + ")"),
      )
    }
  }
  
  val color: CodeSpanParsers = CodeSpanParsers(CodeCategory.NumberLiteral, '#')(DigitParsers.hex.min(1).max(6))

  val string: CodeSpanParsers = StringLiteral.singleLine('"').embed(escape).build ++
    StringLiteral.singleLine('\'').embed(escape).build
  
  val number: CodeSpanParsers = NumberLiteral.decimalFloat.build ++ NumberLiteral.decimalInt.build
  
  val declaration: CodeSpanParsers = {
    
    val embedded: Seq[CodeSpanParsers] = Seq(
      Comment.multiLine("/*", "*/"),
      string,
      color,
      url,
      number,
      identifier(CodeCategory.Identifier),
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
    val attrName = idChars(CodeCategory.AttributeName)
    CodeSpanParsers(attrName.idStartChars) {
      val attribute = (lookBehind(1, any.take(1)) ~ attrName.idRestParser).concat.map(CodeSpan(_, CodeCategory.AttributeName))
      (attribute ~ valueParser(inBlock = false)).concat
    } ++
    CodeSpanParsers('(') {
      val attribute = attrName.standaloneParser
      (attribute ~ valueParser(inBlock = true)).concat.map(spans => CodeSpan("(") +: spans)
    }
  }

  val highlighter: SyntaxHighlighter = SyntaxHighlighter.build("css")(
    Comment.multiLine("/*", "*/"),
    string,
    declaration,
    identifier(CodeCategory.Identifier),
    Keywords("!important")
  )

}
