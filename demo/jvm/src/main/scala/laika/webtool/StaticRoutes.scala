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

import cats.effect.{Blocker, ContextShift, IO}
import org.http4s.{HttpRoutes, Request, Response, StaticFile}
import org.http4s.dsl.io._

/**
  * @author Jens Halm
  */
class StaticRoutes(blocker: Blocker)(implicit cs: ContextShift[IO]) {

  def static(file: String, blocker: Blocker, request: Request[IO]): IO[Response[IO]] =
    StaticFile.fromResource("/public/" + file, blocker, Some(request)).getOrElseF(NotFound())

  val all: HttpRoutes[IO] = HttpRoutes.of[IO] {

    case req @ GET -> Root => static("index.html", blocker, req)
      
    case req @ GET -> Root / "bundle.js" => static("bundle.js", blocker, req)
      
    case req @ GET -> Root / "assets" / file => static(s"assets/$file", blocker, req)
    
  }
  
}
