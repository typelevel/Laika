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
import laika.parse.code.common.{NumberLiteral, StringLiteral}
import laika.parse.code.{CodeCategory, CodeSpan, CodeSpanParsers}
import laika.parse.text.TextParsers._

/**
  * @author Jens Halm
  */
object TextMarkup {
  
  import NumberLiteral._
  
  private def noSpaceAhead(delimChar: Char): Parser[String] = lookAhead(anyBut(' ','\n',delimChar).take(1)).map(_ => "")
  private def noSpaceBefore(numDelimChars: Int): Parser[Unit] = lookBehind(numDelimChars + 1, anyBut(' ','\n').take(1)).map(_ => ())
  
  def span(category: CodeCategory, delim: String): CodeSpanParsers =
    StringLiteral
      .multiLine(delim)
      .withCategory(category)
      .withPrefix(noSpaceAhead(delim.head))
      .withPostCondition(noSpaceBefore(delim.length))
      .build
  
  def singleLine(category: CodeCategory, start: String, end: Char): CodeSpanParsers =
    StringLiteral
      .singleLine(Set(start.head), end)
      .withCategory(category)
      .withPrefix(literal(start.tail))
      .build
  
  private def linkRest(prefix: String): Parser[Seq[CodeSpan]] = {
    
    val url = '(' ~> delimitedBy(')').nonEmpty.failOn('\n').map(url => CodeSpan(s"($url)", CodeCategory.Markup.LinkTarget))
    val ref = '[' ~> delimitedBy(']').failOn('\n').map(ref => CodeSpan(s"[$ref]", CodeCategory.Identifier))
    val link = (literal(prefix.tail) ~ delimitedBy(']').failOn('\n')).concat.map(prefix.head.toString+_+"]")
    
    (link ~ opt(url | ref)).map {
      case link ~ Some(target) if target.content == "[]" => Seq(CodeSpan(link+"[]", CodeCategory.Identifier))  
      case link ~ Some(target) => Seq(CodeSpan(link, CodeCategory.Markup.LinkText), target)  
      case link ~ None => Seq(CodeSpan(link, CodeCategory.Identifier))  
    } 
  }
  
  val link: CodeSpanParsers = CodeSpanParsers('[')(linkRest("["))
  val image: CodeSpanParsers = CodeSpanParsers('!')(linkRest("!["))
  
  val linkTarget: CodeSpanParsers = CodeSpanParsers('\n') {
    ('[' ~> delimitedBy("]:").failOn('\n') ~ restOfLine).map {
      case ref ~ target => Seq(
        CodeSpan("\n"),
        CodeSpan(s"[$ref]:", CodeCategory.Identifier),
        CodeSpan(target, CodeCategory.Markup.LinkTarget),
        CodeSpan("\n"),
      )
    }
  }

  val atxHeader: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Headline, '\n') {
    (anyOf('#').min(1).max(6) ~ restOfLine).concat.map(_+"\n")
  }

  val setexHeader: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Headline, '\n') {
    val deco = (anyOf('=').min(1) | anyOf('-').min(1)) <~ wsEol
    (restOfLine.map(_+"\n") ~ deco).concat.map(_+"\n")
  }
  
  val codeFence: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Fence, '\n') {
    (anyOf('`').take(3) ~ restOfLine).concat
  }

  val rules: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Fence, '\n') {
    Seq('*', '-', '_').map { decoChar =>
      (char(decoChar) ~ (anyOf(' ') ~ literal(decoChar.toString)).concat.rep.min(2) ~ ws ~ '\n').map {
        case start ~ pattern ~ s ~ nl => start.toString + pattern.mkString + s + nl
      }
    }.reduceLeft(_ | _)
  }
  
  val quoteChars: CodeSpanParsers = CodeSpanParsers(CodeCategory.Markup.Quote, '\n')(anyOf('>').min(1))

  lazy val markdown: SyntaxHighlighter = SyntaxHighlighter.build("markdown", "md")(
    span(CodeCategory.Markup.Emphasized, "**"),
    span(CodeCategory.Markup.Emphasized, "*"),
    span(CodeCategory.Markup.Emphasized, "__"),
    span(CodeCategory.Markup.Emphasized, "_"),
    span(CodeCategory.StringLiteral, "``"),
    span(CodeCategory.StringLiteral, "`"),
    singleLine(CodeCategory.Markup.LinkTarget, "<", '>'),
    image,
    link,
    StringLiteral.Escape.char,
    linkTarget,
    codeFence,
    atxHeader,
    setexHeader,
    rules,
    quoteChars,
  )

  lazy val rst: SyntaxHighlighter = SyntaxHighlighter.build("reStructuredText", "rst")(
    /*
    InlineParsers.strong,
    InlineParsers.em,
    InlineParsers.inlineLiteral,
    InlineParsers.phraseLinkRef,
    InlineParsers.simpleLinkRef,
    InlineParsers.footnoteRef,
    InlineParsers.citationRef,
    InlineParsers.substitutionRef,
    InlineParsers.internalTarget,
    InlineParsers.interpretedTextWithRolePrefix,
    InlineParsers.uri,
    InlineParsers.email
    
    ListParsers.bulletList,
    ListParsers.enumList,
    ListParsers.fieldList,
    ListParsers.lineBlock,
    ListParsers.optionList,
    ExplicitBlockParsers.allBlocks,
    ExplicitBlockParsers.shortAnonymousLinkTarget,
    BlockParsers.doctest,
    BlockParsers.blockQuote,
    BlockParsers.headerWithOverline,
    BlockParsers.transition,
    BlockParsers.headerWithUnderline,
    ListParsers.definitionList,
    
    presentation:
    examples for highlighters: VSCode no DTD, Scala docs: bad EBNF highlighting
     */
  )

  lazy val laikaMarkdown: SyntaxHighlighter = SyntaxHighlighter.build("laikaMarkdown", "laika-md")(

  )

  lazy val laikaRst: SyntaxHighlighter = SyntaxHighlighter.build("laikaReStructuredText", "laika-rst")(

  )

  lazy val laikaHTML: SyntaxHighlighter = SyntaxHighlighter.build("laikaHTML", "laika-html")(

  )
  
}
