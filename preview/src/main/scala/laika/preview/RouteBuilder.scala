package laika.preview

import java.io.InputStream

import cats.syntax.all._
import cats.effect.{Async, Resource, Sync}
import fs2.io.readInputStream
import laika.{ast, config}
import org.http4s.EntityEncoder.entityBodyEncoder
import org.http4s.dsl.Http4sDsl
import org.http4s.{EntityEncoder, Headers, HttpRoutes, MediaType}
import org.http4s.headers.`Content-Type`

class RouteBuilder[F[_]: Async](cache: Cache[F, Map[ast.Path, Either[Resource[F, InputStream], String]]]) extends Http4sDsl[F] {

  implicit def inputStreamResourceEncoder[G[_]: Sync, IS <: InputStream]: EntityEncoder[G, Resource[G, IS]] =
    entityBodyEncoder[G].contramap { (in: Resource[G, IS]) =>
      fs2.Stream.resource(in).flatMap { stream =>
        readInputStream[G](Sync[G].pure(stream), 4096, closeAfterUse = false)
      }
    }
  
  private def mediaTypeFor (suffix: String): Option[MediaType] =
    MediaType.forExtension(suffix).orElse(MediaType.forExtension(suffix.split("\\.").last))
  
  def build: HttpRoutes[F] = HttpRoutes.of[F] {

    case GET -> path =>
      val laikaPath = laika.ast.Path.parse(path.toString)
      cache.get.map(_.get(laikaPath.withoutFragment)).flatMap {
        case Some(Right(content)) =>
          Async[F].delay(println(s"serving path $laikaPath - transformed markup")) *>
          Ok(content).map(_.withContentType(`Content-Type`(MediaType.text.html)))
        case Some(Left(input)) =>
          Async[F].delay(println(s"serving path $laikaPath - static input")) *> {
            val mediaType = laikaPath.suffix.flatMap(mediaTypeFor).map(`Content-Type`(_))
            Ok(input).map(_.withHeaders(Headers(mediaType)))
          }
        case None =>
          Async[F].delay(println(s"serving path $laikaPath - not found")) *> NotFound()
      }
  }
  
}
