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

import cats.data.{ NonEmptyList, NonEmptySet }
import laika.api.bundle.SyntaxHighlighter
import laika.ast.CodeSpan
import laika.parse.builders.~
import laika.parse.code.CodeCategory.{ BooleanLiteral, LiteralValue }
import laika.parse.code.common.StringLiteral.StringParser
import laika.parse.code.common.{
  Comment,
  EmbeddedCodeSpans,
  Keywords,
  NumberLiteral,
  StringLiteral
}
import laika.parse.code.implicits._
import laika.parse.code.{ CodeCategory, CodeSpanParser }
import laika.parse.text.PrefixedParser
import laika.parse.text.TextParsers.*

import scala.collection.immutable.SortedSet

/** @author Jens Halm
  */
object HOCONSyntax extends SyntaxHighlighter {

  private val string: StringParser = StringLiteral.singleLine('"').embed(
    StringLiteral.Escape.unicode,
    StringLiteral.Escape.char
  )

  private val quotedAttributeName: StringParser = string
    .withPostCondition(lookAhead(ws ~ oneOf(':', '=', '{')).void)
    .withCategory(CodeCategory.AttributeName)

  private val substitution: CodeSpanParser = CodeSpanParser(CodeCategory.Substitution, "${", "}")

  private val invalidUnquotedChar: NonEmptySet[Char] =
    NonEmptySet.of('$', '"', '{', '}', '[', ']', ':', '=', ',', '+', '#', '`', '^', '?', '!', '@',
      '*', '&', '\\', ' ', '\t', '\n')

  private val unquotedStartChar: NonEmptySet[Char] = {
    // this will need a different approach of optimization for full unicode support
    val startChars = (for (i <- 33 to 255) yield {
      Character.toChars(i)(0)
    })
      .filterNot(c => invalidUnquotedChar.contains(c) || Character.isWhitespace(c))
    NonEmptySet.fromSetUnsafe(SortedSet(startChars: _*))
  }

  private val unquotedChar = {
    val validChar = oneNot(invalidUnquotedChar)

    validChar | oneOf(' ') <~ lookAhead(ws ~ validChar)
  }.rep.source

  private val unquotedAttributeName: CodeSpanParser = CodeSpanParser(CodeCategory.AttributeName) {
    PrefixedParser(unquotedStartChar)(unquotedChar <~ lookAhead(ws ~ oneOf(':', '=', '{')))
  }

  private val unquotedStringValue: CodeSpanParser = CodeSpanParser(CodeCategory.StringLiteral) {
    PrefixedParser(unquotedStartChar)(unquotedChar)
  }

  private def functionNames(names: String*): CodeSpanParser = names.map { name =>
    CodeSpanParser(CodeCategory.Identifier) {
      literal(name) <~ nextIn('(')
    }
  }.reduceLeft(_ ++ _)

  val includeStatement: CodeSpanParser = CodeSpanParser {

    val resourceFunctions: PrefixedParser[Seq[CodeSpan]] = {
      val fName =
        (literal("file") | literal("url") | literal("classpath")).asCode(CodeCategory.Identifier)
      (fName ~ literal("(").asCode() ~ EmbeddedCodeSpans.parser(
        delimitedBy(")"),
        Seq(CodeSpanParser(string))
      )).map { case name ~ paren ~ rest =>
        name +: paren +: rest :+ CodeSpan(")")
      }
    }

    val requiredFunction: PrefixedParser[Seq[CodeSpan]] = {
      val fName = literal("required").asCode(CodeCategory.Identifier)
      val fArg  =
        EmbeddedCodeSpans.parser(delimitedBy(")"), Seq(CodeSpanParser(resourceFunctions | string)))
      (fName ~ literal("(").asCode() ~ fArg).map { case name ~ paren ~ rest =>
        name +: paren +: rest :+ CodeSpan(")")
      }
    }

    (literal("include").asCode(CodeCategory.Keyword) ~ ws.min(
      1
    ).asCode() ~ (string | requiredFunction | resourceFunctions)).map { case inc ~ space ~ rest =>
      inc +: space +: rest
    }
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
