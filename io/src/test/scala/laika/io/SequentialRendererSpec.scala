/*
 * Copyright 2012-2019 the original author or authors.
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

package laika.io

import java.io.{ByteArrayOutputStream, File}

import cats.effect.IO
import laika.api.Renderer
import laika.ast.helper.ModelBuilder
import laika.format._
import laika.io.helper.OutputBuilder
import laika.io.implicits._
import laika.io.runtime.TestContexts.blocker
import laika.io.text.SequentialRenderer

import scala.io.Codec

class SequentialRendererSpec extends IOSpec 
                             with ModelBuilder {


  val renderer: SequentialRenderer[IO] = Renderer.of(AST).io(blocker).sequential[IO].build
  
  
  val rootElem = root(p("aaö"), p("bbb"))

  val expected = """RootElement - Blocks: 2
      |. Paragraph - Spans: 1
      |. . Text - 'aaö'
      |. Paragraph - Spans: 1
      |. . Text - 'bbb'""".stripMargin


  "The Sequential Renderer" should {

    "render a document to a file" in {
      val res = for {
        f <- IO(File.createTempFile("output", null))
        _ <- renderer.from(rootElem).toFile(f).render
        res <- IO(OutputBuilder.readFile(f))
      } yield res
      res.assertEquals(expected)
    }

    "render a document to an in-memory string" in {
      renderer.from(rootElem).toRenderedString.render.assertEquals(expected)
    }

    "render a document to a java.io.OutputStream" in {
      val res = for {
        stream <- IO(new ByteArrayOutputStream)
        _ <- renderer.from(rootElem).toStream(IO.pure(stream)).render
      } yield stream.toString
      res.assertEquals(expected)
    }

    "render a document to a java.io.OutputStream, specifying the encoding explicitly" in {
      val res = for {
        stream <- IO(new ByteArrayOutputStream)
        _ <- renderer.from(rootElem).toStream(IO.pure(stream))(Codec.ISO8859).render
      } yield stream.toString("ISO-8859-1")
      res.assertEquals(expected)
    }

    "render a document to a java.io.OutputStream, specifying the encoding implicitly" in {
      implicit val codec: Codec = Codec.ISO8859
      val res = for {
        stream <- IO(new ByteArrayOutputStream)
        _ <- renderer.from(rootElem).toStream(IO.pure(stream)).render
      } yield stream.toString("ISO-8859-1")
      res.assertEquals(expected)
    }

  }
}
