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
import cats.effect._
import fs2.io.readInputStream
import laika.api.Renderer
import laika.api.builder.OperationConfig
import laika.format.HTML
import laika.helium.Helium
import laika.io.implicits._
import laika.io.api.{TreeParser, TreeRenderer}
import laika.io.model.{InputTreeBuilder, StringTreeOutput}
import laika.theme.ThemeProvider
import org.http4s.EntityEncoder.entityBodyEncoder
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.`Content-Type`
import org.http4s.implicits._
import org.http4s.server.{Router, Server}
import org.http4s.server.blaze.BlazeServerBuilder

/**
  * @author Jens Halm
  */
class ServerBuilder[F[_]: Async] (parser: Resource[F, TreeParser[F]], 
                                  inputs: InputTreeBuilder[F],
                                  theme: ThemeProvider,
                                  port: Int) extends Http4sDsl[F] {

  implicit def inputStreamResourceEncoder[G[_]: Sync, IS <: InputStream]: EntityEncoder[G, Resource[G, IS]] =
    entityBodyEncoder[G].contramap { (in: Resource[G, IS]) =>
      fs2.Stream.resource(in).flatMap { stream =>
        readInputStream[G](Sync[G].pure(stream), 4096, closeAfterUse = false)
      }
    }

  def build: Resource[F, Server] = {
    
    def mediaTypeFor (suffix: String): Option[MediaType] =
      MediaType.forExtension(suffix).orElse(MediaType.forExtension(suffix.split("\\.").last))
    
    def htmlRenderer (config: OperationConfig): Resource[F, TreeRenderer[F]] = Renderer
      .of(HTML)
      .withConfig(config)
      .parallel[F]
      .withTheme(theme)
      .build
    
    (for {
      ctx <- Resource.eval(Async[F].executionContext)
      p   <- parser
      r   <- htmlRenderer(p.config)
    } yield (ctx, p, r)).flatMap { case (ctx, parser, htmlRenderer) =>

      val tree = {
//        val apiPath = validated(SiteConfig.apiPath(baseConfig))
//        val inputs = generateAPI.value.foldLeft(laikaInputs.value.delegate) {
//          (inputs, path) => inputs.addProvidedPath(apiPath / path)
//        }
        parser.fromInput(inputs).parse
      }
      
      val docMap = tree.flatMap { tree =>
        htmlRenderer
          .from(tree.root)
          .copying(tree.staticDocuments)
          .toOutput(StringTreeOutput)
          .render
          .map { root =>
            (root.allDocuments.map { doc =>
              (doc.path, Right(doc.content))
            } ++
            root.staticDocuments.map { doc =>
              (doc.path, Left(doc.input))
            })
            .toMap // TODO - map root
          }
      }

      Resource.eval(Cache.create(docMap)).flatMap { cache =>
      
        val routes = HttpRoutes.of[F] {
          
          case GET -> path => 
            val laikaPath = laika.ast.Path.parse(path.toString)
            cache.get.map(_.get(laikaPath.withoutFragment)).flatMap {
              case Some(Right(content)) => 
                println(s"serving path $laikaPath - transformed markup")
                Ok(content).map(_.withContentType(`Content-Type`(MediaType.text.html)))
              case Some(Left(input)) =>
                println(s"serving path $laikaPath - static input")
                val mediaType = laikaPath.suffix.flatMap(mediaTypeFor).map(`Content-Type`(_))
                Ok(input).map(_.withHeaders(Headers(mediaType)))
              case None => 
                println(s"serving path $laikaPath - not found")
                NotFound()
            }
        }
      
        val httpApp = Router("/" -> routes).orNotFound
        
        BlazeServerBuilder[F](ctx)
          .bindHttp(port, "localhost")
          .withHttpApp(httpApp)
          .resource
      }
    }
  }

  private def copy (newTheme: ThemeProvider = theme,
                    newPort: Int = port): ServerBuilder[F] = 
    new ServerBuilder[F](parser, inputs, newTheme, newPort)

  def withTheme (theme: ThemeProvider): ServerBuilder[F] = copy(newTheme = theme)
  def withPort (port: Int): ServerBuilder[F] = copy(newPort = port)
  
}

object ServerBuilder {
  
  def apply[F[_]: Async](parser: Resource[F, TreeParser[F]], inputs: InputTreeBuilder[F]): ServerBuilder[F] = 
    new ServerBuilder[F](parser, inputs, Helium.defaults.build, 4242)
  
}
