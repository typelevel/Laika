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
import laika.parse.code.common.{NumberLiteral, StringLiteral}
import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.text.PrefixedParser
import laika.parse.text.TextParsers._

/**
  * @author Jens Halm
  */
object MarkdownSyntax extends SyntaxHighlighter {

  import NumberLiteral._

  def span (category: CodeCategory, delim: String): CodeSpanParser =
    StringLiteral
      .multiLine(delimiter(delim).nextNot(' '), delimiter(delim).prevNot(' '))
      .withCategory(category)

  def singleLine (category: CodeCategory, start: String, end: Char): CodeSpanParser =
    StringLiteral
      .singleLine(start, end.toString)
      .withCategory(category)

  private def linkParser (prefix: String): PrefixedParser[Seq[CodeSpan]] = {

    val url = '(' ~> delimitedBy(')').nonEmpty.failOn('\n').map(url => CodeSpan(s"($url)", CodeCategory.Markup.LinkTarget))
    val ref = '[' ~> delimitedBy(']').failOn('\n').map(ref => CodeSpan(s"[$ref]", CodeCategory.Markup.LinkTarget))
    val link = (literal(prefix) ~ delimitedBy(']').failOn('\n')).concat.map(_ + "]")

    (link ~ opt(url | ref)).map {
      case linkText ~ Some(target) if target.content == "[]" => Seq(CodeSpan(linkText + "[]", CodeCategory.Markup.LinkTarget))
      case linkText ~ Some(target) => Seq(CodeSpan(linkText, CodeCategory.Markup.LinkText), target)
      case linkText ~ None         => Seq(CodeSpan(linkText, CodeCategory.Markup.LinkTarget))
    }
  }

  val link: CodeSpanParser = CodeSpanParser(linkParser("["))
  val image: CodeSpanParser = CodeSpanParser(linkParser("!["))
  
  val startOfLine: Parser[String] = atStart.as("") | "\n"

  val linkTarget: CodeSpanParser = CodeSpanParser.onLineStart {
    (startOfLine ~> '[' ~> delimitedBy("]:").failOn('\n') ~ restOfLine).map {
      case ref ~ target => Seq(
        CodeSpan("\n"),
        CodeSpan(s"[$ref]:", CodeCategory.Identifier),
        CodeSpan(target, CodeCategory.Markup.LinkTarget),
        CodeSpan("\n"),
      )
    }
  }

  val atxHeader: CodeSpanParser = CodeSpanParser.onLineStart(CodeCategory.Markup.Headline) {
    (startOfLine ~ someOf('#').max(6) ~ restOfLine).concat.map(_ + "\n")
  }

  val setexHeader: CodeSpanParser = CodeSpanParser.onLineStart(CodeCategory.Markup.Headline) {
    val deco = (someOf('=') | someOf('-')) <~ wsEol
    (startOfLine ~ restOfLine.map(_ + "\n") ~ deco).concat.map(_ + "\n")
  }

  val codeFence: CodeSpanParser = CodeSpanParser.onLineStart(CodeCategory.Markup.Fence) {
    (startOfLine ~ anyOf('`').take(3) ~ restOfLine).concat
  }

  val rules: CodeSpanParser = CodeSpanParser.onLineStart(CodeCategory.Markup.Fence) {
    Seq('*', '-', '_').map { decoChar =>
      (char(decoChar) ~ (anyOf(' ') ~ literal(decoChar.toString)).concat.rep.min(2) ~ ws ~ '\n').map {
        case start ~ pattern ~ s ~ nl => start.toString + pattern.mkString + s + nl
      }
    }.reduceLeft(_ | _)
  }

  val quoteChars: CodeSpanParser = CodeSpanParser.onLineStart(CodeCategory.Markup.Quote) {
    (startOfLine ~ someOf('>')).concat
  }

  val mdSpans: CodeSpanParser = 
    span(CodeCategory.Markup.Emphasized, "**") ++
    span(CodeCategory.Markup.Emphasized, "*") ++
    span(CodeCategory.Markup.Emphasized, "__") ++
    span(CodeCategory.Markup.Emphasized, "_") ++
    span(CodeCategory.StringLiteral, "``") ++
    span(CodeCategory.StringLiteral, "`")

  val language: NonEmptyList[String] = NonEmptyList.of("markdown", "md")
  
  val spanParsers: Seq[CodeSpanParser] = Seq(mdSpans,
    singleLine(CodeCategory.Markup.LinkTarget, "<", '>'),
    image,
    link,
    StringLiteral.Escape.char,
    linkTarget,
    codeFence,
    atxHeader,
    setexHeader,
    rules,
    quoteChars)

}
