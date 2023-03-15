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

package laika.webtool

import laika.factory.MarkupFormat
import laika.format.{ Markdown, ReStructuredText }
import laika.parse.markup.DocumentParser.{ ParserError, TransformationError }

import scala.scalajs.js.annotation.{ JSExport, JSExportTopLevel }

/** @author Jens Halm
  */
@JSExportTopLevel("ClientTransformer")
object ClientTransformer {

  private val delegate = Transformer

  private val formats: Map[String, MarkupFormat] = Map(
    "md"  -> Markdown,
    "rst" -> ReStructuredText
  )

  @JSExport
  def transformToRenderedHTML(format: String, input: String): String =
    wrapError(format, input, delegate.transformToRenderedHTML)

  @JSExport
  def transformToHTMLSource(format: String, input: String): String =
    wrapError(format, input, delegate.transformToHTMLSource)

  @JSExport
  def transformToUnresolvedAST(format: String, input: String): String =
    wrapError(format, input, delegate.transformToUnresolvedAST)

  @JSExport
  def transformToResolvedAST(format: String, input: String): String =
    wrapError(format, input, delegate.transformToResolvedAST)

  private def wrapError(
      formatName: String,
      input: String,
      f: (MarkupFormat, String) => Either[TransformationError, String]
  ): String =
    formats.get(formatName).fold(
      s"<p>Invalid Format ($formatName)</p>"
    ) { format =>
      f(format, input).fold(
        err => s"<p>Client Transformer Error (${err.message})</p>",
        identity
      )
    }

}
