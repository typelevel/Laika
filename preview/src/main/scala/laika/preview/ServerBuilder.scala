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

import java.io.{ File, PrintWriter, StringWriter }
import cats.data.{ Kleisli, OptionT }
import cats.effect._
import cats.syntax.all._
import com.comcast.ip4s._
import fs2.concurrent.Topic
import laika.ast
import laika.ast.DocumentType
import laika.format.{ EPUB, PDF }
import laika.io.api.TreeParser
import laika.io.model.{ FilePath, InputTreeBuilder }
import laika.preview.ServerBuilder.Logger
import org.http4s.dsl.Http4sDsl
import org.http4s.{ HttpApp, HttpRoutes, Request }
import org.http4s.implicits._
import org.http4s.server.{ Router, Server }
import org.http4s.ember.server.EmberServerBuilder

import scala.concurrent.duration._

/** Configures and instantiates a resource for a preview server.
  *
  * Any of the provided inputs which originate in the file system will be watched,
  * and any change will trigger a new transformation.
  * Other input types, such as those generated in memory, will require creating and launching
  * a new server instance.
  *
  * @author Jens Halm
  */
class ServerBuilder[F[_]: Async](
    parser: Resource[F, TreeParser[F]],
    inputs: InputTreeBuilder[F],
    logger: Option[Logger[F]],
    config: ServerConfig
) extends Http4sDsl[F] {

  private val RefreshEvent = "refresh"

  private def copy(
      newLogger: Option[Logger[F]] = logger,
      newConfig: ServerConfig = config
  ): ServerBuilder[F] =
    new ServerBuilder[F](parser, inputs, newLogger, newConfig)

  private def createSourceChangeWatcher(
      cache: Cache[F, SiteResults[F]],
      topic: Topic[F, String],
      docTypeMatcher: ast.Path => DocumentType
  ): Resource[F, Unit] = {
    val update = cache.update >> topic.publish1(RefreshEvent).void
    SourceChangeWatcher.create(
      inputs.fileRoots.toList,
      update,
      config.pollInterval,
      inputs.exclude,
      docTypeMatcher
    )
  }

  private def createApp(cache: Cache[F, SiteResults[F]], topic: Topic[F, String]): HttpApp[F] = {
    def renderStacktrace(service: HttpRoutes[F]): HttpRoutes[F] = Kleisli { (req: Request[F]) =>
      service(req).recoverWith { case err =>
        OptionT.liftF(Async[F].delay {
          val sw = new StringWriter()
          err.printStackTrace(new PrintWriter(sw))
          sw.toString
        }.flatMap(Ok(_)))
      }
    }
    val routeLogger                                             =
      if (config.isVerbose) logger.getOrElse((s: String) => Async[F].delay(println(s)))
      else (s: String) => Async[F].unit
    Router("/" -> renderStacktrace(new RouteBuilder[F](cache, topic, routeLogger).build)).orNotFound
  }

  private def createServer(httpApp: HttpApp[F]): Resource[F, Server] =
    EmberServerBuilder.default[F]
      .withShutdownTimeout(2.seconds)
      .withPort(config.port)
      .withHost(config.host)
      .withHttpApp(httpApp)
      .build

  private def binaryRenderFormats =
    List(EPUB).filter(_ => config.includeEPUB) ++
      List(PDF).filter(_ => config.includePDF)

  private[preview] def buildRoutes: Resource[F, HttpApp[F]] = for {
    transf <- SiteTransformer.create(
      parser,
      inputs,
      binaryRenderFormats,
      config.apiDir,
      config.artifactBasename
    )
    cache  <- Resource.eval(Cache.create(transf.transform))
    topic  <- Resource.eval(Topic[F, String])
    _      <- createSourceChangeWatcher(cache, topic, transf.parser.config.docTypeMatcher)
  } yield createApp(cache, topic)

  def build: Resource[F, Server] = for {
    routes <- buildRoutes
    server <- createServer(routes)
  } yield server

  def withLogger(logger: Logger[F]): ServerBuilder[F]    = copy(newLogger = Some(logger))
  def withConfig(config: ServerConfig): ServerBuilder[F] = copy(newConfig = config)

}

/** Companion for creating a builder for a preview server.
  */
object ServerBuilder {

  type Logger[F[_]] = String => F[Unit]

  /** Creates a new builder for a preview server based on the provided parser and inputs.
    * Further aspects like theme, port, poll interval and other details can optionally be configured
    * with the API of the returned instance.
    */
  def apply[F[_]: Async](
      parser: Resource[F, TreeParser[F]],
      inputs: InputTreeBuilder[F]
  ): ServerBuilder[F] =
    new ServerBuilder[F](parser, inputs, None, ServerConfig.defaults)

}

/** Additional configuration options for a preview server.
  *
  * @param port the port the server should run on (default 4242)
  * @param pollInterval the interval at which input file resources are polled for changes (default 1 second)
  * @param artifactBasename the base name for PDF and EPUB artifacts linked by the generated site (default "docs")
  * @param includeEPUB indicates whether EPUB downloads should be included on a download page (default false)
  * @param includePDF indicates whether PDF downloads should be included on a download page (default false)
  * @param isVerbose whether each served page and each detected file change should be logged (default false)
  * @param apiDir an optional API directory from which API documentation should be served (default None)
  */
class ServerConfig private (
    val port: Port,
    val host: Host,
    val pollInterval: FiniteDuration,
    val artifactBasename: String,
    val includeEPUB: Boolean,
    val includePDF: Boolean,
    val isVerbose: Boolean,
    val apiDir: Option[FilePath]
) {

  private def copy(
      newPort: Port = port,
      newHost: Host = host,
      newPollInterval: FiniteDuration = pollInterval,
      newArtifactBasename: String = artifactBasename,
      newIncludeEPUB: Boolean = includeEPUB,
      newIncludePDF: Boolean = includePDF,
      newVerbose: Boolean = isVerbose,
      newAPIDir: Option[FilePath] = apiDir
  ): ServerConfig =
    new ServerConfig(
      newPort,
      newHost,
      newPollInterval,
      newArtifactBasename,
      newIncludeEPUB,
      newIncludePDF,
      newVerbose,
      newAPIDir
    )

  /** Specifies the port the server should run on (default 4242).
    */
  def withPort(port: Port): ServerConfig = copy(newPort = port)

  /** Specifies the host the server should run on (default localhost).
    */
  def withHost(host: Host): ServerConfig = copy(newHost = host)

  /** Specifies the interval at which input file resources are polled for changes (default 1 second).
    */
  def withPollInterval(interval: FiniteDuration): ServerConfig = copy(newPollInterval = interval)

  /** Indicates that EPUB downloads should be included on the download page.
    */
  def withEPUBDownloads: ServerConfig = copy(newIncludeEPUB = true)

  /** Indicates that PDF downloads should be included on the download page.
    */
  def withPDFDownloads: ServerConfig = copy(newIncludePDF = true)

  /** Specifies the base name for PDF and EPUB artifacts linked by the generated site (default "docs").
    * Additional classifiers might be added to the base name (apart from the file suffix), depending on configuration.
    */
  def withArtifactBasename(name: String): ServerConfig = copy(newArtifactBasename = name)

  /** Specifies a directory from which API documentation of the site can be served.
    * This step is only necessary if you want to test links to API documentation with the preview server.
    */
  def withAPIDirectory(dir: FilePath): ServerConfig = copy(newAPIDir = Some(dir))

  @deprecated("use withAPIDirectory(FilePath)", "0.19.0")
  def withAPIDirectory(dir: File): ServerConfig = copy(newAPIDir = Some(FilePath.fromJavaFile(dir)))

  /** Indicates that each served page and each detected file change should be logged to the console.
    */
  def verbose: ServerConfig = copy(newVerbose = true)

}

/** Companion for preview server configuration instances.
  */
object ServerConfig {

  val defaultPort: Port = port"4242"

  val defaultHost: Host = host"localhost"

  val defaultPollInterval: FiniteDuration = 1.second

  val defaultArtifactBasename: String = "docs"

  /** A ServerConfig instance populated with default values. */
  val defaults = new ServerConfig(
    defaultPort,
    defaultHost,
    defaultPollInterval,
    defaultArtifactBasename,
    false,
    false,
    false,
    None
  )

}
