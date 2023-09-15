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
import laika.api.bundle.SyntaxHighlighter
import laika.ast.CodeSpan
import laika.parse.code.common.{
  EmbeddedCodeSpans,
  Identifier,
  Keywords,
  StringLiteral,
  TagFormats
}
import laika.parse.code.{ CodeCategory, CodeSpanParser }
import laika.parse.text.PrefixedParser
import laika.parse.builders.*
import laika.parse.implicits.*
import laika.parse.code.implicits.*

/** @author Jens Halm
  */
object LaikaExtensionSyntax {

  private val substitution: CodeSpanParser = StringLiteral.Substitution.between("${", "}")

  private val hoconBlock: CodeSpanParser = CodeSpanParser {
    embeddedHocon("{%", "%}", Set(CodeCategory.Keyword))
  }

  private def embeddedHocon(
      start: String,
      end: String,
      delimCategory: Set[CodeCategory] = Set()
  ): PrefixedParser[Seq[CodeSpan]] = {
    (literal(start) ~> EmbeddedCodeSpans.parser(delimitedBy(end), HOCONSyntax)).map {
      hocon => // TODO - support nested objects
        CodeSpan(start, delimCategory) +: hocon :+ CodeSpan(end, delimCategory)
    }
  }

  private val directive: CodeSpanParser = CodeSpanParser {
    val nameParser =
      Identifier.alphaNum.map(name => Seq(CodeSpan("@:", CodeCategory.Keyword), name))
    val whiteSpace = ws.min(1).asCode()

    val defaultAttribute = (ws.asCode() ~ ("(" ~> delimitedBy(")")).map { attr =>
      Seq(CodeSpan("("), CodeSpan(attr, CodeCategory.StringLiteral), CodeSpan(")"))
    }).concat.rep.max(1).map(_.flatten)

    val hoconParser = (whiteSpace ~ embeddedHocon("{", "}")).concat.rep.max(1).map(_.flatten)

    ("@:" ~> nameParser ~ defaultAttribute ~ hoconParser).concat
  }

  private val fence: CodeSpanParser = Keywords("@:@")

  private val allExtensions: Seq[CodeSpanParser] = Seq(substitution, directive, fence, hoconBlock)

  lazy val forMarkdown: SyntaxHighlighter = new SyntaxHighlighter {
    val language: NonEmptyList[String]        = NonEmptyList.of("laikaMarkdown", "laika-md")
    lazy val spanParsers: Seq[CodeSpanParser] = allExtensions ++ MarkdownSyntax.spanParsers
  }

  lazy val forRst: SyntaxHighlighter = new SyntaxHighlighter {
    val language: NonEmptyList[String] = NonEmptyList.of("laikaReStructuredText", "laika-rst")
    lazy val spanParsers: Seq[CodeSpanParser] = allExtensions ++ ReStructuredTextSyntax.spanParsers
  }

  private val enhancedStartTag: CodeSpanParser = TagFormats.customTag("<", ">").embed(
    StringLiteral.singleLine('\'').embed(TagFormats.ref, substitution),
    StringLiteral.singleLine('"').embed(TagFormats.ref, substitution),
    TagFormats.name(CodeCategory.AttributeName)
  )

  private val modifiedHTMLParsers: Seq[CodeSpanParser] = Seq(
    HTMLSyntax.docType,
    TagFormats.comment,
    TagFormats.ref,
    TagFormats.emptyTag,
    HTMLSyntax.scriptTag,
    HTMLSyntax.styleTag,
    enhancedStartTag,
    TagFormats.endTag
  )

  lazy val forHTML: SyntaxHighlighter = new SyntaxHighlighter {
    val language: NonEmptyList[String]        = NonEmptyList.of("laikaHTML", "laika-html")
    lazy val spanParsers: Seq[CodeSpanParser] = allExtensions ++ modifiedHTMLParsers
  }

}
