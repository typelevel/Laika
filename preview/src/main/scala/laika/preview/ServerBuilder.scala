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
import laika.ast
import laika.ast.DocumentType
import laika.helium.Helium
import laika.io.api.TreeParser
import laika.io.model.InputTreeBuilder
import laika.theme.ThemeProvider
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.{Router, Server}
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/**
  * @author Jens Halm
  */
class ServerBuilder[F[_]: Async] (parser: Resource[F, TreeParser[F]],
                                  inputs: InputTreeBuilder[F],
                                  theme: ThemeProvider,
                                  port: Int,
                                  pollInterval: FiniteDuration,
                                  artifactBasename: String) extends Http4sDsl[F] {

  private def copy (newTheme: ThemeProvider = theme,
                    newPort: Int = port,
                    newPollInterval: FiniteDuration = pollInterval,
                    newArtifactBasename: String = artifactBasename): ServerBuilder[F] =
    new ServerBuilder[F](parser, inputs, newTheme, newPort, newPollInterval, newArtifactBasename)
  
  private def createSourceChangeWatcher (cache: Cache[F, SiteResults[F]],
                                         docTypeMatcher: ast.Path => DocumentType): Resource[F, Unit] =
    SourceChangeWatcher.create(inputs.fileRoots.toList, cache.update, pollInterval, inputs.exclude, docTypeMatcher)
    
  private def createServer (cache: Cache[F, SiteResults[F]],
                            ctx: ExecutionContext): Resource[F, Server] = {
    val httpApp = Router("/" -> new RouteBuilder[F](cache).build).orNotFound

    BlazeServerBuilder[F](ctx)
      .bindHttp(port, "localhost")
      .withHttpApp(httpApp)
      .resource
  }
  
  def build: Resource[F, Server] = for {
    transf <- SiteTransformer.create(parser, inputs, theme, artifactBasename)
    ctx    <- Resource.eval(Async[F].executionContext)
    cache  <- Resource.eval(Cache.create(transf.transform))
    _      <- createSourceChangeWatcher(cache, transf.parser.config.docTypeMatcher)
    server <- createServer(cache, ctx)
  } yield server

  def withTheme (theme: ThemeProvider): ServerBuilder[F] = copy(newTheme = theme)
  def withPort (port: Int): ServerBuilder[F] = copy(newPort = port)
  def withPollInterval (interval: FiniteDuration): ServerBuilder[F] = copy(newPollInterval = interval)
  def withArtifactBasename (name: String): ServerBuilder[F] = copy(newArtifactBasename = name)
  
}

object ServerBuilder {
  
  val defaultPort: Int = 4242
  
  val defaultPollInterval: FiniteDuration = 3.seconds
  
  val defaultArtifactBasename: String = "docs"

  def apply[F[_]: Async](parser: Resource[F, TreeParser[F]], inputs: InputTreeBuilder[F]): ServerBuilder[F] = 
    new ServerBuilder[F](parser, inputs, Helium.defaults.build, defaultPort, defaultPollInterval, defaultArtifactBasename)
  
}

private[laika] case class PreviewConfig ()