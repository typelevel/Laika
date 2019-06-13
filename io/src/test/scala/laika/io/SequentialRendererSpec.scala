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
import laika.io.Sequential.SequentialRenderer
import laika.io.helper.OutputBuilder
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Codec

class SequentialRendererSpec extends FlatSpec 
                    with Matchers
                    with ModelBuilder { self =>

  
  val renderer: SequentialRenderer[IO] = Sequential(Renderer.of(AST)).build[IO]
  
  val rootElem = root(p("aaö"), p("bbb"))

  val expected = """RootElement - Blocks: 2
      |. Paragraph - Spans: 1
      |. . Text - 'aaö'
      |. Paragraph - Spans: 1
      |. . Text - 'bbb'""".stripMargin

//  "The Render API" should "render a document to a string" in {
//    Renderer.of(AST).build.render(rootElem) should be (expected)
//  }

  "The Render API" should "render a document to a file" in {
    val f = File.createTempFile("output", null)

    renderer.from(rootElem).toFile(f).render.unsafeRunSync()

    OutputBuilder.readFile(f) should be (expected)
  }

  it should "render a document to a java.io.OutputStream" in {
    val stream = new ByteArrayOutputStream
    renderer.from(rootElem).toStream(IO.pure(stream)).render.unsafeRunSync()
    stream.toString should be (expected)
  }

  it should "render a document to a java.io.OutputStream, specifying the encoding explicitly" in {
    val stream = new ByteArrayOutputStream
    renderer.from(rootElem).toStream(IO.pure(stream))(Codec.ISO8859).render.unsafeRunSync()
    stream.toString("ISO-8859-1") should be (expected)
  }

  it should "render a document to a java.io.OutputStream, specifying the encoding implicitly" in {
    implicit val codec:Codec = Codec.ISO8859
    val stream = new ByteArrayOutputStream
    renderer.from(rootElem).toStream(IO.pure(stream)).render.unsafeRunSync()
    stream.toString("ISO-8859-1") should be (expected)
  }

//  it should "allow to override the default renderer for specific element types" in {
//    val render = Renderer.of(AST)rendering { case (_, Text(content,_)) => s"String - '$content'" }
//    val modifiedResult = expected.replaceAllLiterally("Text", "String")
//    (render render rootElem) should be (modifiedResult)
//  }


}
  