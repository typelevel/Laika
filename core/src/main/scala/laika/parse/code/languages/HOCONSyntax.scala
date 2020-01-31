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
import laika.parse.code.CodeCategory.{BooleanLiteral, LiteralValue}
import laika.parse.code.common.StringLiteral.StringParser
import laika.parse.code.common.{Comment, Keywords, NumberLiteral, StringLiteral}
import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.text.{CharGroup, PrefixedParser}
import laika.parse.text.TextParsers.{lookAhead, ws, _}

/**
  * @author Jens Halm
  */
object HOCONSyntax extends SyntaxHighlighter {

  val string: StringParser = StringLiteral.singleLine('"').embed(
    StringLiteral.Escape.unicode,
    StringLiteral.Escape.char
  )

  val quotedAttributeName: StringParser = string
    .withPostCondition(lookAhead(ws ~ anyOf(':','=','{').take(1)) ^^^ (()))
    .copy(defaultCategories = Set(CodeCategory.AttributeName))
  
  val substitution: CodeSpanParser = CodeSpanParser(CodeCategory.Substitution, "${", "}")

  private val unquotedChar = {
    val validChar = anyBut('$', '"', '{', '}', '[', ']', ':', '=', ',', '+', '#', '`', '^', '?', '!', '@', '*', '&', '\\', ' ', '\t','\n').take(1) 
    
    validChar | anyOf(' ').take(1) <~ lookAhead(ws ~ validChar)
  }.rep.map(_.mkString)
  
  val unquotedAttributeName: CodeSpanParser = CodeSpanParser(CodeCategory.AttributeName) {
    PrefixedParser(CharGroup.alpha)(unquotedChar <~ lookAhead(ws ~ anyOf(':','=','{').take(1))) // TODO - 0.14 - this is inaccurate
  }
  
  val unquotedStringValue: CodeSpanParser = CodeSpanParser(CodeCategory.StringLiteral) {
    PrefixedParser(CharGroup.alpha)(unquotedChar) // TODO - 0.14 - this is inaccurate
  }
  
  def functionNames(names: String*): CodeSpanParser = names.map { name =>
    CodeSpanParser(CodeCategory.Identifier) {
      literal(name) <~ lookAhead('(')
    }
  }.reduceLeft(_ ++ _)
  
  val includeStatement: CodeSpanParser = CodeSpanParser(CodeCategory.Keyword) {
    literal("include") <~ lookAhead(ws.min(1) ~ (literal("\"") | literal("required(") | literal("file(") | literal("url(") | literal("classpath(")))
  }

  val language: NonEmptyList[String] = NonEmptyList.of("hocon")

  val spanParsers: Seq[CodeSpanParser] = Seq(
    Keywords(BooleanLiteral)("true", "false"),
    Keywords(LiteralValue)("null"),
    NumberLiteral.decimalInt,
    NumberLiteral.decimalFloat,
    StringLiteral.multiLine("\"\"\""),
    quotedAttributeName,
    string,
    substitution,
    Comment.singleLine("//"),
    Comment.singleLine("#"),
    functionNames("required", "file", "url", "classpath"),
    includeStatement,
    unquotedAttributeName,
    unquotedStringValue
  )
  
}
