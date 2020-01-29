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
import laika.parse.code.{CodeCategory, CodeSpanParsers}
import laika.parse.code.common.{NumberLiteral, StringLiteral}
import laika.parse.text.TextParsers._

/**
  * @author Jens Halm
  */
object MarkdownSyntax extends SyntaxHighlighter {

  import NumberLiteral._

  private def noSpaceAhead (delimChar: Char): Parser[String] = lookAhead(anyBut(' ', '\n', delimChar).take(1)).map(_ => "")

  private def noSpaceBefore (numDelimChars: Int): Parser[Unit] = lookBehind(numDelimChars + 1, anyBut(' ', '\n').take(1)).map(_ => ())

  def span (category: CodeCategory, delim: String): CodeSpanParsers =
    StringLiteral
      .multiLine(delim)
      .withCategory(category)
      .withPrefix(noSpaceAhead(delim.head))
      .withPostCondition(noSpaceBefore(delim.length))

  def singleLine (category: CodeCategory, start: String, end: Char): CodeSpanParsers =
    StringLiteral
      .singleLine(Set(start.head), end)
      .withCategory(category)
      .withPrefix(literal(start.tail))

  private def linkParser (prefix: String): Parser[Seq[CodeSpan]] = {

    val url = '(' ~> delimitedBy(')').nonEmpty.failOn('\n').map(url => CodeSpan(s"($url)", CodeCategory.Markup.LinkTarget))
    val ref = '[' ~> delimitedBy(']').failOn('\n').map(ref => CodeSpan(s"[$ref]", CodeCategory.Markup.LinkTarget))
    val link = (literal(prefix) ~ delimitedBy(']').failOn('\n')).concat.map(_ + "]")

    (link ~ opt(url | ref)).map {
      case link ~ Some(target) if target.content == "[]" => Seq(CodeSpan(link + "[]", CodeCategory.Markup.LinkTarget))
      case link ~ Some(target) => Seq(CodeSpan(link, CodeCategory.Markup.LinkText), target)
      case link ~ None => Seq(CodeSpan(link, CodeCategory.Markup.LinkTarget))
    }
  }

  val link: CodeSpanParsers = CodeSpanParsers('[')(linkParser("["))
  val image: CodeSpanParsers = CodeSpanParsers('!')(linkParser("!["))
  
  val startOfLine: Parser[String] = atStart ^^^ "" | "\n"

  val linkTarget: CodeSpanParsers = CodeSpanParsers('\n') {
    (startOfLine ~> '[' ~> delimitedBy("]:").failOn('\n') ~ restOfLine).map {
      case ref ~ target => Seq(
        CodeSpan("\n"),
        CodeSpan(s"[$ref]:", CodeCategory.Identifier),
        CodeSpan(target, CodeCategory.Markup.LinkTarget),
        CodeSpan("\n"),
      )
    }
  }

  val atxHeader: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Headline, '\n') {
    (startOfLine ~ anyOf('#').min(1).max(6) ~ restOfLine).concat.map(_ + "\n")
  }

  val setexHeader: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Headline, '\n') {
    val deco = (anyOf('=').min(1) | anyOf('-').min(1)) <~ wsEol
    (startOfLine ~ restOfLine.map(_ + "\n") ~ deco).concat.map(_ + "\n")
  }

  val codeFence: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Fence, '\n') {
    (startOfLine ~ anyOf('`').take(3) ~ restOfLine).concat
  }

  val rules: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Fence, '\n') {
    Seq('*', '-', '_').map { decoChar =>
      (char(decoChar) ~ (anyOf(' ') ~ literal(decoChar.toString)).concat.rep.min(2) ~ ws ~ '\n').map {
        case start ~ pattern ~ s ~ nl => start.toString + pattern.mkString + s + nl
      }
    }.reduceLeft(_ | _)
  }

  val quoteChars: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Quote, '\n')((startOfLine ~ anyOf('>').min(1)).concat)

  val mdSpans: CodeSpanParsers = span(CodeCategory.Markup.Emphasized, "**") ++
    span(CodeCategory.Markup.Emphasized, "*") ++
    span(CodeCategory.Markup.Emphasized, "__") ++
    span(CodeCategory.Markup.Emphasized, "_") ++
    span(CodeCategory.StringLiteral, "``") ++
    span(CodeCategory.StringLiteral, "`")

  val language: NonEmptyList[String] = NonEmptyList.of("markdown", "md")
  
  val spanParsers: Seq[CodeSpanParsers] = Seq(mdSpans,
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
