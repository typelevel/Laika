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

import cats.effect.{ ExitCode, IO, IOApp, Resource }
import cats.implicits._
import com.comcast.ip4s._
import org.http4s.implicits._
import org.http4s.server.Server
import org.http4s.ember.server.EmberServerBuilder

/** @author Jens Halm
  */
object Main extends IOApp {

  private def service =
    (StaticRoutes.all <+> TransformerRoutes.all <+> StatusRoutes.all).orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    app.use(_ => IO.never).as(ExitCode.Success)

  val app: Resource[IO, Server] =
    EmberServerBuilder.default[IO]
      .withHost(host"0.0.0.0")
      .withPort(port"8080")
      .withHttpApp(service)
      .build

}
