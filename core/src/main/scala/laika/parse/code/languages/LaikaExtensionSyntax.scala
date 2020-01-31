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
import laika.parse.code.common.{EmbeddedCodeSpans, Identifier, Keywords, StringLiteral}
import laika.parse.code.{CodeCategory, CodeSpanParser}
import laika.parse.text.PrefixedParser
import laika.parse.text.TextParsers._

/**
  * @author Jens Halm
  */
object LaikaExtensionSyntax {
  
  val substitution: CodeSpanParser = StringLiteral.Substitution.between("${", "}")

  val hoconBlock: CodeSpanParser = CodeSpanParser('{') {
    embeddedHocon("{%", "%}", Set(CodeCategory.Keyword))
  }

  def embeddedHocon (start: String, end: String, delimCategory: Set[CodeCategory] = Set()): PrefixedParser[Seq[CodeSpan]] = {
    (literal(start) ~> EmbeddedCodeSpans.parser(delimitedBy(end), HOCONSyntax)).map { hocon => // TODO - support nested objects
      CodeSpan(start, delimCategory) +: hocon :+ CodeSpan(end, delimCategory)
    }
  }

  val directive: CodeSpanParser = CodeSpanParser('@') {
    ("@:" ~> Identifier.alphaNum.standaloneParser ~ opt(ws.min(1) ~ embeddedHocon("{", "}"))).map {
      case name ~ Some(spaces ~ hocon) => CodeSpan("@:", CodeCategory.Keyword) +: name +: CodeSpan(spaces) +: hocon
      case name ~ None => Seq(CodeSpan("@:", CodeCategory.Keyword), name)
    }
  }

  val fence: CodeSpanParser = Keywords("@:@")

  val allExtensions: Seq[CodeSpanParser] = Seq(substitution, directive, fence, hoconBlock)

  lazy val forMarkdown: SyntaxHighlighter = new SyntaxHighlighter {
    val language: NonEmptyList[String] = NonEmptyList.of("laikaMarkdown", "laika-md")
    lazy val spanParsers: Seq[CodeSpanParser] = allExtensions ++ MarkdownSyntax.spanParsers
  }

  lazy val forRst: SyntaxHighlighter = new SyntaxHighlighter {
    val language: NonEmptyList[String] = NonEmptyList.of("laikaReStructuredText", "laika-rst")
    lazy val spanParsers: Seq[CodeSpanParser] = allExtensions ++ ReStructuredTextSyntax.spanParsers
  }

  val enhancedStartTag: CodeSpanParser = HTMLSyntax.TagParser(CodeCategory.Tag.Name, "<", ">").embed(
    StringLiteral.singleLine('\'').embed(HTMLSyntax.ref, substitution),
    StringLiteral.singleLine('"').embed(HTMLSyntax.ref, substitution),
    HTMLSyntax.name(CodeCategory.AttributeName)
  )
  
  val modifiedHTMLParsers: Seq[CodeSpanParser] = Seq(
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
    lazy val spanParsers: Seq[CodeSpanParser] = allExtensions ++ modifiedHTMLParsers
  }
  
}
