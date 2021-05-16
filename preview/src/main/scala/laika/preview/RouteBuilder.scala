/*
 * Copyright 2012-2021 the original author or authors.
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

package laika.preview

import java.io.InputStream

import cats.syntax.all._
import cats.effect.{Async, Resource, Sync}
import fs2.concurrent.Topic
import fs2.io.readInputStream
import laika.preview.ServerBuilder.Logger
import org.http4s.EntityEncoder
import org.http4s.dsl.Http4sDsl
import org.http4s.{CacheDirective, EntityEncoder, Headers, HttpRoutes, MediaType, ServerSentEvent}
import org.http4s.headers.{`Cache-Control`, `Content-Type`}

import scala.concurrent.duration.DurationInt

private [preview] class RouteBuilder[F[_]: Async](cache: Cache[F, SiteResults[F]],
                                                  sseTopic: Topic[F, String],
                                                  logger: Logger[F]) extends Http4sDsl[F] {

  implicit def inputStreamResourceEncoder[G[_]: Sync, IS <: InputStream]: EntityEncoder[G, Resource[G, IS]] =
    EntityEncoder.entityBodyEncoder[G].contramap { (in: Resource[G, IS]) =>
      fs2.Stream.resource(in).flatMap { stream =>
        readInputStream[G](Sync[G].pure(stream), 4096, closeAfterUse = false)
      }
    }
  
  private def mediaTypeFor (suffix: String): Option[MediaType] =
    MediaType.forExtension(suffix).orElse(MediaType.forExtension(suffix.split("\\.").last))
  
  private val noCache = `Cache-Control`(CacheDirective.`no-store`)
  
  def build: HttpRoutes[F] = HttpRoutes.of[F] {

    case GET -> Root / "laika" / "events" =>
      val msg = Async[F].pure("keepAlive")
      val keepAlive = fs2.Stream.repeatEval(msg).metered(10.seconds)
      Ok(sseTopic.subscribe(10).merge(keepAlive).map(ServerSentEvent(_)))
        
    case GET -> path =>
      val laikaPath = laika.ast.Path.parse(path.toString)
      cache.get.map(_.get(laikaPath.withoutFragment)).flatMap {
        case Some(RenderedResult(content)) =>
          logger(s"serving path $laikaPath - transformed markup") *>
          Ok(content).map(_
            .withHeaders(noCache)
            .withContentType(`Content-Type`(MediaType.text.html))
          )
        case Some(StaticResult(input)) =>
          logger(s"serving path $laikaPath - static input") *> {
            val mediaType = laikaPath.suffix.flatMap(mediaTypeFor).map(`Content-Type`(_))
            Ok(input).map(_.withHeaders(Headers(mediaType, noCache)))
          }
        case None =>
          logger(s"serving path $laikaPath - not found") *> NotFound()
      }
  }
  
}
