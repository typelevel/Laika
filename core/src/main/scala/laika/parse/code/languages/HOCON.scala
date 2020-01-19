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

import laika.bundle.SyntaxHighlighter
import laika.parse.code.CodeCategory.{BooleanLiteral, LiteralValue}
import laika.parse.code.common.StringLiteral.StringParser
import laika.parse.code.common.{Comment, Identifier, Keywords, NumberLiteral, StringLiteral}
import laika.parse.code.{CodeCategory, CodeSpanParsers}
import laika.parse.text.TextParsers.{lookAhead, ws, _}

/**
  * @author Jens Halm
  */
object HOCON {

  val string: StringParser = StringLiteral.singleLine('"').embed(
    StringLiteral.Escape.unicode,
    StringLiteral.Escape.char
  )

  val quotedAttributeName: StringParser = string
    .withPostCondition(lookAhead(ws ~ anyOf(':','=','{').take(1)) ^^^ (()))
    .copy(defaultCategories = Set(CodeCategory.AttributeName))
  
  val substitution: CodeSpanParsers = CodeSpanParsers(CodeCategory.Substitution, "${", "}")

  private val unquotedChar = {
    val validChar = anyBut('$', '"', '{', '}', '[', ']', ':', '=', ',', '+', '#', '`', '^', '?', '!', '@', '*', '&', '\\', ' ', '\t','\n').take(1) 
    
    validChar | anyOf(' ').take(1) <~ lookAhead(ws ~ validChar)
  }.rep.map(_.mkString)
  
  val unquotedAttributeName: CodeSpanParsers = CodeSpanParsers(CodeCategory.AttributeName, Identifier.idStartChars) {
    unquotedChar <~ lookAhead(ws ~ anyOf(':','=','{').take(1))
  }
  
  val unquotedStringValue: CodeSpanParsers = CodeSpanParsers(CodeCategory.StringLiteral, Identifier.idStartChars) {
    unquotedChar
  }
  
  def functionNames(names: String*): CodeSpanParsers = names.map { name =>
    CodeSpanParsers(CodeCategory.Identifier, name.head) {
      literal(name.tail) <~ lookAhead('(')
    }
  }.reduceLeft(_ ++ _)
  
  val includeStatement: CodeSpanParsers = CodeSpanParsers(CodeCategory.Keyword, 'i') {
    literal("nclude") <~ lookAhead(ws.min(1) ~ (literal("\"") | literal("required(") | literal("file(") | literal("url(") | literal("classpath(")))
  }
  
  val highlighter: SyntaxHighlighter = SyntaxHighlighter.build("hocon")(
    Keywords(BooleanLiteral)("true", "false"),
    Keywords(LiteralValue)("null"),
    NumberLiteral.decimalFloat.build,
    NumberLiteral.decimalInt.build,
    StringLiteral.multiLine("\"\"\"").build,
    quotedAttributeName.build,
    string.build,
    substitution,
    Comment.singleLine("//"),
    Comment.singleLine("#"),
    functionNames("required", "file", "url", "classpath"),
    includeStatement,
    unquotedAttributeName,
    unquotedStringValue
  )
  
}
