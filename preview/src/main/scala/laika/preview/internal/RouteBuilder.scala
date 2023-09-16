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

package laika.preview.internal

import cats.effect.{ Async, Resource, Sync }
import cats.syntax.all.*
import fs2.concurrent.Topic
import fs2.io.readInputStream
import laika.preview.ServerBuilder.Logger
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.{ `Cache-Control`, `Content-Type` }
import org.http4s.{
  CacheDirective,
  EntityEncoder,
  Headers,
  HttpRoutes,
  MediaType,
  Response,
  ServerSentEvent
}

import java.io.InputStream
import scala.concurrent.duration.DurationInt

private[preview] class RouteBuilder[F[_]: Async](
    cache: Cache[F, SiteResults[F]],
    sseTopic: Topic[F, String],
    logger: Logger[F]
) extends Http4sDsl[F] {

  implicit def inputStreamResourceEncoder[G[_]: Sync, IS <: InputStream]
      : EntityEncoder[G, Resource[G, IS]] =
    EntityEncoder.entityBodyEncoder[G].contramap { (in: Resource[G, IS]) =>
      fs2.Stream.resource(in).flatMap { stream =>
        readInputStream[G](Sync[G].pure(stream), 4096, closeAfterUse = false)
      }
    }

  private def mediaTypeFor(suffix: String): Option[MediaType] =
    MediaType.forExtension(suffix).orElse(MediaType.forExtension(suffix.split("\\.").last))

  private val noCache = `Cache-Control`(CacheDirective.`no-store`)

  def serve(laikaPath: laika.ast.Path, result: Option[SiteResult[F]]): F[Response[F]] =
    result match {
      case Some(RenderedResult(content)) =>
        logger(s"serving path $laikaPath - transformed markup") *>
          Ok(content).map(
            _
              .withHeaders(noCache)
              .withContentType(`Content-Type`(MediaType.text.html))
          )
      case Some(StaticResult(input))     =>
        logger(s"serving path $laikaPath - static input") *> {
          val mediaType = laikaPath.suffix.flatMap(mediaTypeFor).map(`Content-Type`(_))
          Ok(input).map(_.withHeaders(Headers(mediaType, noCache)))
        }
      case Some(LazyResult(res))         => res.flatMap(serve(laikaPath, _))
      case None                          =>
        logger(s"serving path $laikaPath - not found") *> NotFound()
    }

  def build: HttpRoutes[F] = HttpRoutes.of[F] {

    case GET -> Root / "laika" / "events" =>
      val keepAlive = fs2.Stream.fixedRate(10.seconds).as("keepAlive")
      Ok(sseTopic.subscribe(10).merge(keepAlive).map(msg => ServerSentEvent(Some(msg))))

    case GET -> path =>
      val laikaPath = laika.ast.Path.parse(path.toString)
      cache.get.map(_.get(laikaPath.withoutFragment)).flatMap(serve(laikaPath, _))
  }

}
