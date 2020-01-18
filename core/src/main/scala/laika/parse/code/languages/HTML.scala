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
import laika.parse.code.common.{EmbeddedCodeSpans, Keywords, TagBasedFormats, NumberLiteral}
import laika.parse.code.{CodeCategory, CodeSpan, CodeSpanParsers}
import laika.parse.text.TextParsers._

/**
  * @author Jens Halm
  */
object HTML extends TagBasedFormats {
  
  import NumberLiteral._

  val docType: CodeSpanParsers = TagParser(CodeCategory.XML.DTDTagName, "<!", ">", "DOCTYPE").embed(
    Keywords("SYSTEM", "PUBLIC"),
    string,
    comment,
    name(CodeCategory.Identifier)
  ).build

  object EmbeddedJs extends EmbeddedCodeSpans {
    val embedded: Seq[CodeSpanParsers] = JavaScript.highlighter.spanParsers
    val defaultCategories: Set[CodeCategory] = Set()

    private val endTag: Seq[CodeSpan] = Seq(
      CodeSpan("</", CodeCategory.XML.Punctuation),
      CodeSpan("script", CodeCategory.XML.TagName)
    )
    val rootParser: Parser[Seq[CodeSpan]] =
      (contentParser(delimitedBy("</script")) ~ (ws ~ ">").concat).map {
        case content ~ close => content ++ endTag :+ CodeSpan(close, CodeCategory.XML.Punctuation)
      }
  }

  object EmbeddedCSS extends EmbeddedCodeSpans {
    val embedded: Seq[CodeSpanParsers] = CSS.highlighter.spanParsers
    val defaultCategories: Set[CodeCategory] = Set()

    private val endTag: Seq[CodeSpan] = Seq(
      CodeSpan("</", CodeCategory.XML.Punctuation),
      CodeSpan("style", CodeCategory.XML.TagName)
    )
    val rootParser: Parser[Seq[CodeSpan]] =
      (contentParser(delimitedBy("</style")) ~ (ws ~ ">").concat).map {
        case content ~ close => content ++ endTag :+ CodeSpan(close, CodeCategory.XML.Punctuation)
      }
  }
  
  val scriptTag: CodeSpanParsers = CodeSpanParsers('<') {
    
    val startTag: Parser[Seq[CodeSpan]] = TagParser(CodeCategory.XML.TagName, "<", ">", literal("script")).embed(
      stringWithEntities,
      name(CodeCategory.AttributeName)
    ).standaloneParser
    
    (startTag ~ EmbeddedJs.rootParser).map { case tag ~ js => tag ++ js }
  }

  val styleTag: CodeSpanParsers = CodeSpanParsers('<') {

    val startTag: Parser[Seq[CodeSpan]] = TagParser(CodeCategory.XML.TagName, "<", ">", literal("style")).embed(
      stringWithEntities,
      name(CodeCategory.AttributeName)
    ).standaloneParser

    (startTag ~ EmbeddedCSS.rootParser).map { case tag ~ js => tag ++ js }
  }
  
  val highlighter: SyntaxHighlighter = SyntaxHighlighter.build("html")(
    docType,
    comment,
    ref,
    emptyTag,
    scriptTag,
    styleTag,
    startTag,
    endTag
  )
  
}
