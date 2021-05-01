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
import org.http4s.{MediaType, Method, Request, Response, Status}
import org.scalatest.Assertion

class PreviewRouteSpec extends IOFunSuite with InputBuilder {
  
  def stringBody (expected: String)(response: Response[IO]): IO[Assertion] =
    response.as[String].map(_ shouldBe expected)

  def nonEmptyBody (response: Response[IO]): IO[Assertion] =
    response.body.compile.last.map(_.nonEmpty shouldBe true)

  def check[A](actual:         IO[Response[IO]],
               expectedStatus: Status): IO[Assertion] =
    check(actual, expectedStatus, None, _ => IO.pure(succeed))
  
  def check[A](actual:            IO[Response[IO]],
               expectedStatus:    Status,
               expectedMediaType: Option[MediaType],
               bodyCheck:         Response[IO] => IO[Assertion]): IO[Assertion] = 
    actual.flatMap { response =>
      response.status shouldBe expectedStatus
      expectedMediaType.foreach { mt =>
        response.headers.get[`Content-Type`] shouldBe Some(`Content-Type`(mt))
      }
      bodyCheck(response)
    }

  val defaultParser = MarkupParser
    .of(Markdown)
    .parallel[IO]
    .withTheme(Theme.empty)
    .build
  
  test("serve a rendered document") {
    
    val inputs = build(Seq((Root / "doc.md") -> "foo"))
    
    ServerBuilder(defaultParser, inputs).buildRoutes.use { app =>
      val req = Request[IO](method = Method.GET, uri = uri"/doc.html" )
      val res = app.run(req)
      check (res, Status.Ok, Some(MediaType.text.html), stringBody("<p>foo</p>"))
    }.run
    
  }

  test("serve a rendered index document") {

    val inputs = build(Seq((Root / "dir" / "README.md") -> "foo"))

    ServerBuilder(defaultParser, inputs).buildRoutes.use { app =>
      val req = Request[IO](method = Method.GET, uri = uri"/dir" )
      val res = app.run(req)
      check (res, Status.Ok, Some(MediaType.text.html), stringBody("<p>foo</p>"))
    }.run

  }

  test("serve a static document") {

    val inputs = build(Seq(
      (Root / "doc.md") -> "foo",
      (Root / "dir" / "image.jpg") -> "img"
    ))

    ServerBuilder(defaultParser, inputs).buildRoutes.use { app =>
      val req = Request[IO](method = Method.GET, uri = uri"/dir/image.jpg" )
      val res = app.run(req)
      check (res, Status.Ok, Some(MediaType.image.jpeg), stringBody("img"))
    }.run

  }

  test("serve a generated EPUB document") {

    val inputs = build(Seq(
      (Root / "doc.md") -> "foo",
      (Root / "dir" / "image.jpg") -> "img"
    ))

    ServerBuilder(defaultParser, inputs)
      .withConfig(ServerConfig.defaults.withEPUBDownloads.verbose)
      .buildRoutes
      .use { app =>
        val req = Request[IO](method = Method.GET, uri = uri"/downloads/docs.epub" )
        val res = app.run(req)
        check (res, Status.Ok, Some(MediaType.application.`epub+zip`), nonEmptyBody)
      }
      .run
  }

  test("return 404 for unknown target path") {

    val inputs = build(Seq(
      (Root / "doc.md") -> "foo",
      (Root / "dir" / "image.jpg") -> "img"
    ))

    ServerBuilder(defaultParser, inputs).buildRoutes.use { app =>
      val req = Request[IO](method = Method.GET, uri = uri"/dir/styles.css" )
      val res = app.run(req)
      check (res, Status.NotFound)
    }.run

  }
  // static file scanner
  
}
