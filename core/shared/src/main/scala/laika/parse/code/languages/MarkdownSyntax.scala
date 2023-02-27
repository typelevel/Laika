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
import laika.parse.code.common.StringLiteral
import laika.parse.code.{ CodeCategory, CodeSpanParser }
import laika.parse.text.PrefixedParser
import laika.parse.builders._
import laika.parse.implicits._
import laika.parse.code.implicits._

/** @author Jens Halm
  */
object MarkdownSyntax extends SyntaxHighlighter {

  def span(category: CodeCategory, delim: String): CodeSpanParser =
    StringLiteral
      .multiLine(delimiter(delim).nextNot(' '), delimiter(delim).prevNot(' '))
      .withCategory(category)

  def singleLine(category: CodeCategory, start: String, end: Char): CodeSpanParser =
    StringLiteral
      .singleLine(start, end.toString)
      .withCategory(category)

  private def linkParser(prefix: String): PrefixedParser[Seq[CodeSpan]] = {

    val url =
      ("(" ~> delimitedBy(')').nonEmpty.failOn('\n')).source.asCode(CodeCategory.Markup.LinkTarget)

    val ref = ("[" ~> delimitedBy(']').failOn('\n')).source.asCode(CodeCategory.Markup.LinkTarget)

    val link = (literal(prefix) ~ delimitedBy(']').failOn('\n')).source

    (link ~ opt(url | ref)).map {
      case linkText ~ Some(target) if target.content == "[]" =>
        Seq(CodeSpan(linkText + "[]", CodeCategory.Markup.LinkTarget))
      case linkText ~ Some(target) => Seq(CodeSpan(linkText, CodeCategory.Markup.LinkText), target)
      case linkText ~ None         => Seq(CodeSpan(linkText, CodeCategory.Markup.LinkTarget))
    }
  }

  val link: CodeSpanParser  = CodeSpanParser(linkParser("["))
  val image: CodeSpanParser = CodeSpanParser(linkParser("!["))

  val linkTarget: CodeSpanParser = CodeSpanParser.onLineStart {
    ("[" ~> delimitedBy("]:").failOn('\n') ~ restOfLine).map { case ref ~ target =>
      Seq(
        CodeSpan(s"[$ref]:", CodeCategory.Identifier),
        CodeSpan(target, CodeCategory.Markup.LinkTarget),
        CodeSpan("\n")
      )
    }
  }

  val atxHeader: CodeSpanParser = CodeSpanParser.onLineStart(CodeCategory.Markup.Headline) {
    (someOf('#').max(6) ~ anyNot('\n')).source
  }

  val setexHeader: CodeSpanParser = CodeSpanParser.onLineStart(CodeCategory.Markup.Headline) {
    val deco = (someOf('=') | someOf('-')) <~ lookAhead(wsEol)
    (restOfLine ~ deco).source
  }

  val codeFence: CodeSpanParser = CodeSpanParser.onLineStart(CodeCategory.Markup.Fence) {
    (anyOf('`').take(3) ~ anyNot('\n')).source
  }

  val rules: CodeSpanParser = CodeSpanParser.onLineStart(CodeCategory.Markup.Fence) {
    Seq('*', '-', '_').map { decoChar =>
      (oneOf(decoChar) ~ (anyOf(' ') ~ oneOf(decoChar)).rep.min(2) ~ ws ~ "\n").source
    }.reduceLeft(_ | _)
  }

  val quoteChars: CodeSpanParser = CodeSpanParser.onLineStart(CodeCategory.Markup.Quote) {
    someOf('>').source
  }

  val mdSpans: CodeSpanParser =
    span(CodeCategory.Markup.Emphasized, "**") ++
      span(CodeCategory.Markup.Emphasized, "*") ++
      span(CodeCategory.Markup.Emphasized, "__") ++
      span(CodeCategory.Markup.Emphasized, "_") ++
      span(CodeCategory.StringLiteral, "``") ++
      span(CodeCategory.StringLiteral, "`")

  val language: NonEmptyList[String] = NonEmptyList.of("markdown", "md")

  val spanParsers: Seq[CodeSpanParser] = Seq(
    mdSpans,
    singleLine(CodeCategory.Markup.LinkTarget, "<", '>'),
    image,
    link,
    StringLiteral.Escape.char,
    linkTarget,
    codeFence,
    atxHeader,
    setexHeader,
    rules,
    quoteChars
  )

}
