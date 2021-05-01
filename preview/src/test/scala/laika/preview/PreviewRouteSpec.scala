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

import cats.effect.IO
import laika.api.MarkupParser
import laika.ast.Path.Root
import laika.format.Markdown
import laika.io.IOFunSuite
import laika.io.helper.InputBuilder
import laika.io.implicits._
import laika.theme.Theme
import org.http4s.headers.`Content-Type`
import org.http4s.implicits._
import org.http4s.{MediaType, Method, Request, Response, Status, Uri}
import org.scalatest.Assertion

class PreviewRouteSpec extends IOFunSuite with InputBuilder {
  
  def stringBody (expected: String)(response: Response[IO]): IO[Assertion] =
    response.as[String].map(_ shouldBe expected)

  def nonEmptyBody (response: Response[IO]): IO[Assertion] =
    response.body.compile.last.map(_.nonEmpty shouldBe true)

  def ignoreBody (response: Response[IO]): IO[Assertion] = IO.pure(succeed)

  val defaultParser = MarkupParser
    .of(Markdown)
    .parallel[IO]
    .withTheme(Theme.empty)
    .build
  
  val defaultServer = {
    val inputs = build(Seq(
      (Root / "doc.md") -> "foo",
      (Root / "dir" / "README.md") -> "foo",
      (Root / "dir" / "image.jpg") -> "img"
    ))
    ServerBuilder(defaultParser, inputs)
  }

  def check[A](actual:            IO[Response[IO]],
               expectedStatus:    Status,
               expectedMediaType: Option[MediaType] = None,
               bodyCheck:         Response[IO] => IO[Assertion] = ignoreBody): IO[Assertion] =
    actual.flatMap { response =>
      response.status shouldBe expectedStatus
      expectedMediaType.foreach { mt =>
        response.headers.get[`Content-Type`] shouldBe Some(`Content-Type`(mt))
      }
      bodyCheck(response)
    }
  
  def run (uri: Uri,
           expectedStatus:    Status,
           expectedMediaType: Option[MediaType] = None,
           bodyCheck:         Response[IO] => IO[Assertion] = ignoreBody,
           config:            ServerConfig = ServerConfig.defaults): Assertion = 
    defaultServer
      .withConfig(config)
      .buildRoutes
      .use { app =>
        val res = app.run(Request[IO](Method.GET, uri))
        check(res, expectedStatus, expectedMediaType, bodyCheck)
      }
      .run

  
  test("serve a rendered document") {
    run(uri"/doc.html", Status.Ok, Some(MediaType.text.html), stringBody("<p>foo</p>"))
  }

  test("serve a rendered index document") {
    run(uri"/dir", Status.Ok, Some(MediaType.text.html), stringBody("<p>foo</p>"))
  }

  test("serve a static document") {
    run(uri"/dir/image.jpg", Status.Ok, Some(MediaType.image.jpeg), stringBody("img"))
  }

  test("serve a generated EPUB document") {
    val config = ServerConfig.defaults.withEPUBDownloads
    run(uri"/downloads/docs.epub", Status.Ok, Some(MediaType.application.`epub+zip`), nonEmptyBody, config)
  }

  test("return 404 for unknown target path") {
    run(uri"/dir/styles.css", Status.NotFound)
  }
  
}
