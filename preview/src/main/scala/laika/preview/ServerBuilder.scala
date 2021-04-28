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

import java.io.{File, InputStream}

import cats.syntax.all._
import cats.effect._
import com.comcast.ip4s.Literals.port
import laika.ast
import laika.ast.DocumentType
import laika.helium.Helium
import laika.io.api.TreeParser
import laika.io.model.InputTreeBuilder
import laika.preview.ServerBuilder.Logger
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
                                  logger: Option[Logger[F]], 
                                  config: ServerConfig) extends Http4sDsl[F] {

  private def copy (newTheme: ThemeProvider = theme,
                    newLogger: Option[Logger[F]] = logger,
                    newConfig: ServerConfig = config): ServerBuilder[F] =
    new ServerBuilder[F](parser, inputs, newTheme, newLogger, newConfig)
  
  private def createSourceChangeWatcher (cache: Cache[F, SiteResults[F]],
                                         docTypeMatcher: ast.Path => DocumentType): Resource[F, Unit] =
    SourceChangeWatcher.create(inputs.fileRoots.toList, cache.update, config.pollInterval, inputs.exclude, docTypeMatcher)
    
  private def createServer (cache: Cache[F, SiteResults[F]],
                            ctx: ExecutionContext): Resource[F, Server] = {
    val routeLogger = 
      if (config.isVerbose) logger.getOrElse((s: String) => Async[F].delay(println(s)))
      else (s: String) => Async[F].unit
    val httpApp = Router("/" -> new RouteBuilder[F](cache, routeLogger).build).orNotFound

    BlazeServerBuilder[F](ctx)
      .bindHttp(config.port, "localhost")
      .withHttpApp(httpApp)
      .resource
  }
  
  def build: Resource[F, Server] = for {
    transf <- SiteTransformer.create(parser, inputs, theme, config.artifactBasename)
    ctx    <- Resource.eval(Async[F].executionContext)
    cache  <- Resource.eval(Cache.create(transf.transform))
    _      <- createSourceChangeWatcher(cache, transf.parser.config.docTypeMatcher)
    server <- createServer(cache, ctx)
  } yield server

  def withTheme (theme: ThemeProvider): ServerBuilder[F] = copy(newTheme = theme)
  def withLogger (logger: Logger[F]): ServerBuilder[F] = copy(newLogger = Some(logger))
  def withConfig (config: ServerConfig): ServerBuilder[F] = copy(newConfig = config)
  
}

object ServerBuilder {
  
  type Logger[F[_]] = String => F[Unit]
  
  def apply[F[_]: Async](parser: Resource[F, TreeParser[F]], inputs: InputTreeBuilder[F]): ServerBuilder[F] = 
    new ServerBuilder[F](parser, inputs, Helium.defaults.build, None, ServerConfig.defaults)
  
}

class ServerConfig private (val port: Int,
                            val pollInterval: FiniteDuration,
                            val artifactBasename: String,
                            val includeEPUB: Boolean,
                            val includePDF: Boolean,
                            val isVerbose: Boolean,
                            val targetDir: Option[File],
                            val apiFiles: Seq[String]) {

  private def copy (newPort: Int = port,
                    newPollInterval: FiniteDuration = pollInterval,
                    newArtifactBasename: String = artifactBasename,
                    newIncludeEPUB: Boolean = includeEPUB,
                    newIncludePDF: Boolean = includePDF,
                    newVerbose: Boolean = isVerbose,
                    newTargetDir: Option[File] = targetDir,
                    newApiFiles: Seq[String] = apiFiles): ServerConfig =
    new ServerConfig(newPort, newPollInterval, newArtifactBasename, newIncludeEPUB, newIncludePDF, newVerbose, newTargetDir, newApiFiles)
    
  def withPort (port: Int): ServerConfig = copy(newPort = port)
  def withPollInterval (interval: FiniteDuration): ServerConfig = copy(newPollInterval = interval)
  def withEPUBDownloads: ServerConfig = copy(newIncludeEPUB = true)
  def withPDFDownloads: ServerConfig = copy(newIncludePDF = true)
  def withArtifactBasename (name: String): ServerConfig = copy(newArtifactBasename = name)
  def withTargetDirectory (dir: File): ServerConfig = copy(newTargetDir = Some(dir))
  def withApiFiles (apiFiles: Seq[String]): ServerConfig = copy(newApiFiles = apiFiles)
  def verbose: ServerConfig = copy(newVerbose = true)
  
}

object ServerConfig {

  val defaultPort: Int = 4242

  val defaultPollInterval: FiniteDuration = 3.seconds

  val defaultArtifactBasename: String = "docs"
  
  val defaults = new ServerConfig(defaultPort, defaultPollInterval, defaultArtifactBasename, false, false, false, None, Nil)
  
}
