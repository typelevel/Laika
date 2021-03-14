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
import laika.ast.CodeSpan
import laika.bundle.SyntaxHighlighter
import laika.parse.Parser
import laika.parse.builders._
import laika.parse.code.common.{Comment, Identifier, NumberLiteral, StringLiteral}
import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.code.common.Keywords
import laika.parse.implicits._
import laika.parse.text.PrefixedParser
import laika.parse.text.TextParsers.{literal, oneOf}

/**
 * @author Michał Sitko
 */
object YAMLSyntax extends SyntaxHighlighter {
  /** The names of the language (and its optional aliases) as used in text markup */
  override def language: NonEmptyList[String] = NonEmptyList.of("yaml", "yml")

  private val keyName = Identifier.alphaNum.withIdStartChars('_', '-', '.').withCategory(CodeCategory.AttributeName)

  private val valueLiteral = {
    val doubleQuotedStrLiteral = StringLiteral.singleLine('"')
    val singleQuotedStrLiteral = StringLiteral.singleLine('\'')
    val number = NumberLiteral.decimalFloat | NumberLiteral.decimalInt | NumberLiteral.octal | NumberLiteral.hex
    val boolean =
      (literal("true") | "True" | "TRUE" | "false" | "False" | "FALSE" | "on" | "On" | "ON" | "off" | "Off" | "OFF" | "yes" | "Yes" | "YES" | "no" | "No" | "NO")
        .map(b => Seq(CodeSpan(b, CodeCategory.BooleanLiteral)))
    val nullLiteral = (literal("null")).map(n => Seq(CodeSpan(n, CodeCategory.LiteralValue)))
    val unquotedStrLiteral = delimitedBy(literal(" #") | literal("\n")).acceptEOF.keepDelimiter.map(s => List(CodeSpan(s, CodeCategory.StringLiteral)))

    doubleQuotedStrLiteral | singleQuotedStrLiteral | number | boolean | nullLiteral | unquotedStrLiteral
  }

  private val keyAndValue = {
    val separator = (anyOf(' ','\t') ~ ":" ~ someOf(' ','\t')).source.map(s => List(CodeSpan(s)))
    (keyName ~ separator ~ valueLiteral).concat
  }
  private val key = {
    val separator = (anyOf(' ','\t') ~ ":" ~ anyOf(' ','\t') ~ "\n").source.map(s => List(CodeSpan(s)))
    (keyName ~ separator).concat
  }

  private val seqItem = {
    val parser = (literal("- ").map(s => CodeSpan(s)) ~ (keyAndValue | valueLiteral)).concat
    CodeSpanParser(parser)
  }

  /** The parsers for individual code spans written in this language */
  override def spanParsers: Seq[CodeSpanParser] = Seq(
    Comment.singleLine("#"),
    CodeSpanParser(keyAndValue),
    CodeSpanParser(key),
    seqItem
  )
}
