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

import laika.api.{Parse, Render}
import laika.factory.ParserFactory
import laika.render.HTML
import laika.rewrite.{DocumentCursor, RewriteRules}
import laika.tree.Documents.Document
import play.api.libs.json.JsValue
import play.api.libs.json.Json.{prettyPrint, toJson}

/**
  * @author Jens Halm
  */
object Transformer {

  /** Note that in application code the transformation is usually done
   *  in one line. Here we want to first obtain the raw document tree
   *  and then rewrite it manually (which is usually performed automatically)
   *  as we want to show both in the result.
   */
  def transform (format: ParserFactory, input: String): String = {

    val rawDoc = Parse.as(format).withoutRewrite.fromString(input)
    val rules = RewriteRules.defaultsFor(format)(DocumentCursor(rawDoc))

    val rewrittenDoc = rawDoc.rewrite(rules)
    val html = Render.as(HTML).from(rewrittenDoc).toString

    def jsonAST (doc: Document): JsValue = jsonString(doc.content.toString)
    def jsonString (str: String): JsValue = toJson(str.trim)

    prettyPrint(toJson(Map(
      "rawTree"       -> jsonAST(rawDoc),
      "rewrittenTree" -> jsonAST(rewrittenDoc),
      "html"          -> jsonString(html)
    )))
  }


}
