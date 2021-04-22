package laika.preview

import cats.syntax.all._
import cats.effect._
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.{Router, Server}
import org.http4s.server.blaze.BlazeServerBuilder

class ServerBuilder[F[_]: Async] extends Http4sDsl[F] {

  def build: Resource[F, Server] = {
    
    Resource.eval(Async[F].executionContext).flatMap { ctx =>
      
      val helloWorldService = HttpRoutes.of[F] {
        case GET -> Root / "hello" / name => Ok(s"Hello, $name.")
      }
    
      val httpApp = Router("/" -> helloWorldService).orNotFound
      
      val serverBuilder = 
        BlazeServerBuilder[F](ctx)
          .bindHttp(4242, "localhost")
          .withHttpApp(httpApp)

      serverBuilder.resource
  
    }
  }
  
}

object ServerBuilder {
  
  def apply[F[_]: Async](): ServerBuilder[F] = new ServerBuilder[F]
  
}
