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

import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import laika.parse.markdown.Markdown
import laika.parse.rst.ReStructuredText

/**
  * @author Jens Halm
  */
object TransformerRoutes {

  def all: Route = {

    (post & path("transform" / Map("md" -> Markdown, "rst" -> ReStructuredText))) { format =>
      entity(as[String]) { input =>
        complete(HttpEntity(ContentTypes.`application/json`, Transformer.transform(format, input)))
      }
    }

  }

}
