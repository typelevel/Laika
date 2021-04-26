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

import cats.syntax.all._
import cats.effect._
import laika.api.Renderer
import laika.api.builder.OperationConfig
import laika.format.HTML
import laika.helium.Helium
import laika.io.implicits._
import laika.io.api.{TreeParser, TreeRenderer}
import laika.io.model.{InputTreeBuilder, StringTreeOutput}
import laika.theme.ThemeProvider
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.{Router, Server}
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.duration._

/**
  * @author Jens Halm
  */
class ServerBuilder[F[_]: Async] (parser: Resource[F, TreeParser[F]],
                                  inputs: InputTreeBuilder[F],
                                  theme: ThemeProvider,
                                  port: Int,
                                  pollInterval: FiniteDuration) extends Http4sDsl[F] {

  def build: Resource[F, Server] = {
    
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
          
        SourceChangeWatcher.create(
          inputs.fileRoots.toList, 
          cache.update, 
          pollInterval, 
          inputs.exclude, 
          parser.config.docTypeMatcher
        ).flatMap { _ =>

          val httpApp = Router("/" -> new RouteBuilder[F](cache).build).orNotFound

          BlazeServerBuilder[F](ctx)
            .bindHttp(port, "localhost")
            .withHttpApp(httpApp)
            .resource
        }
      }
    }
  }

  private def copy (newTheme: ThemeProvider = theme,
                    newPort: Int = port,
                    newPollInterval: FiniteDuration = pollInterval): ServerBuilder[F] = 
    new ServerBuilder[F](parser, inputs, newTheme, newPort, newPollInterval)

  def withTheme (theme: ThemeProvider): ServerBuilder[F] = copy(newTheme = theme)
  def withPort (port: Int): ServerBuilder[F] = copy(newPort = port)
  def withPollInterval (interval: FiniteDuration): ServerBuilder[F] = copy(newPollInterval = interval)
  
}

object ServerBuilder {
  
  val defaultPort: Int = 4242
  
  val defaultPollInterval: FiniteDuration = 3.seconds

  def apply[F[_]: Async](parser: Resource[F, TreeParser[F]], inputs: InputTreeBuilder[F]): ServerBuilder[F] = 
    new ServerBuilder[F](parser, inputs, Helium.defaults.build, defaultPort, defaultPollInterval)
  
}
