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

import java.io._

import cats.effect.{ContextShift, IO}
import laika.api.Transformer
import laika.format._
import laika.io.implicits._
import laika.io.text.SequentialTransformer
import laika.runtime.TestContexts.blocker
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext
import scala.io.{Codec, Source}

class SequentialTransformerSpec extends FlatSpec 
                       with Matchers {

  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val transformer: SequentialTransformer[IO] = 
    Transformer.from(Markdown).to(AST).io(blocker).sequential[IO].build
  
  
  val input = """# Title äöü
    |
    |text""".stripMargin 
  
  val output = """RootElement - Blocks: 2
    |. Title(Id(title) + Styles(title)) - Spans: 1
    |. . Text - 'Title äöü'
    |. Paragraph - Spans: 1
    |. . Text - 'text'""".stripMargin
    
  
  
  "The Transform API" should "transform from file to file" in {
    val inFile = getClass.getResource("/testInput2.md").getFile
    val outFile = File.createTempFile("output", null)
    implicit val codec:Codec = Codec.UTF8

    transformer.fromFile(inFile).toFile(outFile).transform.unsafeRunSync()

    val source = Source.fromFile(outFile)
    val fileContent = source.mkString
    source.close()
    outFile.delete()

    fileContent should be (output)
  }
  
  it should "transform from a java.io.InputStream to a java.io.OutputStream" in {
    val inStream = IO(new ByteArrayInputStream(input.getBytes()))
    val outStream = new ByteArrayOutputStream
    transformer.fromStream(inStream).toStream(IO.pure(outStream)).transform.unsafeRunSync()
    outStream.toString should be (output)
  }
  
  it should "transform from a java.io.InputStream to a java.io.OutputStream, specifying the encoding explicitly" in {
    val inStream = IO(new ByteArrayInputStream(input.getBytes("ISO-8859-1")))
    val outStream = new ByteArrayOutputStream
    val codec = Codec.ISO8859
    transformer.fromStream(inStream)(codec).toStream(IO.pure(outStream))(codec).transform.unsafeRunSync()
    outStream.toString("ISO-8859-1") should be (output)
  }
  
  it should "transform from a java.io.InputStream to a java.io.OutputStream, specifying the encoding implicitly" in {
    val inStream = IO(new ByteArrayInputStream(input.getBytes("ISO-8859-1")))
    val outStream = new ByteArrayOutputStream
    implicit val codec:Codec = Codec.ISO8859
    transformer.fromStream(inStream).toStream(IO.pure(outStream)).transform.unsafeRunSync()
    outStream.toString("ISO-8859-1") should be (output)
  }

  it should "transform from string to string" in {
    val res = transformer.fromString(input).toRenderedString.transform.unsafeRunSync()
    res should be (output)
  }
  
}
