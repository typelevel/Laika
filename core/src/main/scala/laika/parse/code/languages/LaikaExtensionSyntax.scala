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
import laika.parse.code.common.{EmbeddedCodeSpans, Identifier, Keywords, NumberLiteral, StringLiteral}
import laika.parse.code.{CodeCategory, CodeSpanParsers}
import laika.parse.text.TextParsers._
import laika.rst.BaseParsers
import laika.rst.InlineParsers.{delimitedByMarkupEnd, markupStart}

/**
  * @author Jens Halm
  */
object LaikaExtensionSyntax {
  
  val substitution: CodeSpanParsers = StringLiteral.Substitution.between("${", "}")

  val hoconBlock: CodeSpanParsers = CodeSpanParsers('{') {
    embeddedHocon("%", "%}", Set(CodeCategory.Keyword)).map(res => CodeSpan("{", CodeCategory.Keyword) +: res)
  }

  def embeddedHocon (start: String, end: String, delimCategory: Set[CodeCategory] = Set()): Parser[Seq[CodeSpan]] = {
    (literal(start) ~> EmbeddedCodeSpans.parser(delimitedBy(end), HOCONSyntax)).map { hocon => // TODO - support nested objects
      CodeSpan(start, delimCategory) +: hocon :+ CodeSpan(end, delimCategory)
    }
  }

  val directive: CodeSpanParsers = CodeSpanParsers('@') {
    (':' ~> Identifier.standard.standaloneParser ~ opt(ws.min(1) ~ embeddedHocon("{", "}"))).map {
      case name ~ Some(spaces ~ hocon) => CodeSpan("@:", CodeCategory.Keyword) +: name +: CodeSpan(spaces) +: hocon
      case name ~ None => Seq(CodeSpan("@:", CodeCategory.Keyword), name)
    }
  }

  val fence: CodeSpanParsers = Keywords("@:@")

  val allExtensions: Seq[CodeSpanParsers] = Seq(substitution, directive, fence, hoconBlock)

  lazy val forMarkdown: SyntaxHighlighter = new SyntaxHighlighter {
    val language: NonEmptyList[String] = NonEmptyList.of("laikaMarkdown", "laika-md")
    lazy val spanParsers: Seq[CodeSpanParsers] = allExtensions ++ MarkdownSyntax.spanParsers
  }

  lazy val forRst: SyntaxHighlighter = new SyntaxHighlighter {
    val language: NonEmptyList[String] = NonEmptyList.of("laikaReStructuredText", "laika-rst")
    lazy val spanParsers: Seq[CodeSpanParsers] = allExtensions ++ ReStructuredTextSyntax.spanParsers
  }

  val enhancedStartTag: CodeSpanParsers = HTMLSyntax.TagParser(CodeCategory.Tag.Name, "<", ">", HTMLSyntax.nameParser).embed(
    StringLiteral.singleLine('\'').embed(HTMLSyntax.ref, substitution),
    StringLiteral.singleLine('"').embed(HTMLSyntax.ref, substitution),
    HTMLSyntax.name(CodeCategory.AttributeName)
  )
  
  val modifiedHTMLParsers: Seq[CodeSpanParsers] = Seq(
    HTMLSyntax.docType,
    HTMLSyntax.comment,
    HTMLSyntax.ref,
    HTMLSyntax.emptyTag,
    HTMLSyntax.scriptTag,
    HTMLSyntax.styleTag,
    enhancedStartTag,
    HTMLSyntax.endTag
  )

  lazy val forHTML: SyntaxHighlighter = new SyntaxHighlighter {
    val language: NonEmptyList[String] = NonEmptyList.of("laikaHTML", "laika-html")
    lazy val spanParsers: Seq[CodeSpanParsers] = allExtensions ++ modifiedHTMLParsers
  }
  
}
