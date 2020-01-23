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

  val embeddedJs: Parser[Seq[CodeSpan]] = {
    val endTag: Seq[CodeSpan] = Seq(
      CodeSpan("</", CodeCategory.Tag.Punctuation),
      CodeSpan("script", CodeCategory.Tag.Name)
    )
    (EmbeddedCodeSpans.parser(delimitedBy("</script"), JavaScript.highlighter.spanParsers) ~ (ws ~ ">").concat).map {
      case content ~ close => content ++ endTag :+ CodeSpan(close, CodeCategory.Tag.Punctuation)
    }
  }

  val embeddedCSS: Parser[Seq[CodeSpan]] = {
    val endTag: Seq[CodeSpan] = Seq(
      CodeSpan("</", CodeCategory.Tag.Punctuation),
      CodeSpan("style", CodeCategory.Tag.Name)
    )
    (EmbeddedCodeSpans.parser(delimitedBy("</style"), CSS.highlighter.spanParsers) ~ (ws ~ ">").concat).map {
      case content ~ close => content ++ endTag :+ CodeSpan(close, CodeCategory.Tag.Punctuation)
    }
  }
  
  val scriptTag: CodeSpanParsers = CodeSpanParsers('<') {
    
    val startTag: Parser[Seq[CodeSpan]] = TagParser(CodeCategory.Tag.Name, "<", ">", literal("script")).embed(
      stringWithEntities,
      name(CodeCategory.AttributeName)
    ).standaloneParser
    
    (startTag ~ embeddedJs).map { case tag ~ js => tag ++ js }
  }

  val styleTag: CodeSpanParsers = CodeSpanParsers('<') {

    val startTag: Parser[Seq[CodeSpan]] = TagParser(CodeCategory.Tag.Name, "<", ">", literal("style")).embed(
      stringWithEntities,
      name(CodeCategory.AttributeName)
    ).standaloneParser

    (startTag ~ embeddedCSS).map { case tag ~ js => tag ++ js }
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
