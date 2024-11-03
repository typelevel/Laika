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
import laika.parse.code.common.{ Keywords, TagFormats }
import laika.parse.code.common.TagFormats.*
import laika.parse.code.{ CodeCategory, CodeSpanParser }
import laika.parse.syntax.*

/** @author Jens Halm
  */
object HTMLSyntax extends SyntaxHighlighter {

  private[languages] val docType: CodeSpanParser = customTag("<!", ">")
    .forTagName("DOCTYPE")
    .withCategory(CodeCategory.XML.DTDTagName)
    .embed(
      Keywords("SYSTEM", "PUBLIC"),
      string,
      comment,
      name(CodeCategory.Identifier)
    )

  private def startTag(tagName: String): TagParser =
    customTag("<", ">")
      .forTagName(tagName)
      .embed(
        stringWithEntities,
        name(CodeCategory.AttributeName)
      )

  private[languages] val scriptTag: CodeSpanParser = CodeSpanParser {
    (startTag("script") ~ elementRest("script", JavaScriptSyntax.spanParsers)).concat
  }

  private[languages] val styleTag: CodeSpanParser = CodeSpanParser {
    (startTag("style") ~ elementRest("style", CSSSyntax.spanParsers)).concat
  }

  val language: NonEmptyList[String] = NonEmptyList.of("html")

  val spanParsers: Seq[CodeSpanParser] = Seq(
    docType,
    comment,
    ref,
    emptyTag,
    scriptTag,
    styleTag,
    TagFormats.startTag,
    endTag
  )

}
