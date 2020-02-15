/*
 * Copyright 2013-2018 the original author or authors.
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

import io.circe.Json
import io.circe.syntax._
import laika.api.{MarkupParser, Renderer}
import laika.ast.Document
import laika.factory.MarkupFormat
import laika.format.HTML
import laika.parse.markup.DocumentParser.ParserError

/**
  * @author Jens Halm
  */
object Transformer {

  /** Note that in application code the transformation is usually done
   *  in one line. Here we want to first obtain the raw document tree
   *  and then rewrite it manually (which is usually performed automatically)
   *  as we want to show both in the result.
   */
  def transform (format: MarkupFormat, input: String): Either[ParserError, String] = {

    val parser = MarkupParser.of(format).build
    
    for {
      unresolvedDoc <- parser.parseUnresolved(input)
      resolvedDoc   <- parser.parse(input)
    } yield {

      val html = Renderer.of(HTML).build.render(resolvedDoc).toString
  
      def jsonAST (doc: Document): Json = Json.fromString(doc.content.toString)
      def jsonString (str: String): Json = Json.fromString(str.trim)
  
      Map(
        "rawTree"       -> jsonAST(unresolvedDoc.document),
        "rewrittenTree" -> jsonAST(resolvedDoc),
        "html"          -> jsonString(html)
      ).asJson.spaces2
    }
  }

}
