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

import cats.effect.IO
import laika.factory.MarkupFormat
import laika.format.{Markdown, ReStructuredText}
import laika.parse.markup.DocumentParser.ParserError
import org.http4s.HttpRoutes
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.headers.`Content-Type`

/**
  * @author Jens Halm
  */
object TransformerRoutes {
  
  object InputFormat {
    def unapply(str: String): Option[MarkupFormat] = str match {
      case "md" => Some(Markdown)
      case "rst" => Some(ReStructuredText)
      case _ => None
    }
  }
  
  type TransformFunction = (MarkupFormat, String) => Either[ParserError, String]

  object OutputFormat {
    def unapply(str: String): Option[TransformFunction] = str match {
      case "html-rendered"  => Some(Transformer.transformToRenderedHTML)
      case "html-source"    => Some(Transformer.transformToHTMLSource)
      case "ast-resolved"   => Some(Transformer.transformToResolvedAST)
      case "ast-unresolved" => Some(Transformer.transformToUnresolvedAST)
      case _ => None
    }
  }

  private def transform(format: MarkupFormat, f: TransformFunction, body: IO[String]): IO[Response[IO]] = for {
    input  <- body
    result <- IO.fromEither(f(format, input))
    resp   <- Ok(result)
  } yield resp.withContentType(`Content-Type`(MediaType.text.html))

  val all: HttpRoutes[IO] = HttpRoutes.of[IO] {

    case req @ POST -> Root / "transform" / InputFormat(inFormat) / OutputFormat(outFormat) =>
      transform(inFormat, outFormat, req.as[String])
    
  }

}
