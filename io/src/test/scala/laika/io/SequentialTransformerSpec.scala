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

import cats.implicits._
import cats.effect.IO
import laika.api.Transformer
import laika.format._
import laika.io.implicits._
import laika.io.text.SequentialTransformer
import org.scalatest.Matchers

import scala.io.Codec

class SequentialTransformerSpec extends IOSpec 
                                with Matchers
                                with FileIO {

  
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
    
  
  "The Transform API" should {

    "transform from file to file" in {
      val inFile = getClass.getResource("/testInput2.md").getFile
      implicit val codec: Codec = Codec.UTF8
      val res = for {
        outFile <- newTempFile
        _       <- transformer.fromFile(inFile).toFile(outFile).transform
        res     <- readFile(outFile)
        _       <- delete(outFile)
      } yield res
      res.assertEquals(output)
    }

    "transform from a java.io.InputStream to a java.io.OutputStream" in {
      val inStream = IO(new ByteArrayInputStream(input.getBytes()))
      withByteArrayTextOutput { out =>
        transformer.fromStream(inStream).toStream(IO.pure(out)).transform.void
      }.assertEquals(output)
    }

    "transform from a java.io.InputStream to a java.io.OutputStream, specifying the encoding explicitly" in {
      val inStream = IO(new ByteArrayInputStream(input.getBytes("ISO-8859-1")))
      val codec = Codec.ISO8859
      withByteArrayTextOutput("ISO-8859-1") { out =>
        transformer.fromStream(inStream)(codec).toStream(IO.pure(out))(codec).transform.void
      }.assertEquals(output)
    }

    "transform from a java.io.InputStream to a java.io.OutputStream, specifying the encoding implicitly" in {
      val inStream = IO(new ByteArrayInputStream(input.getBytes("ISO-8859-1")))
      implicit val codec: Codec = Codec.ISO8859
      withByteArrayTextOutput("ISO-8859-1") { out =>
        transformer.fromStream(inStream).toStream(IO.pure(out)).transform.void
      }.assertEquals(output)
    }

    "transform from string to string" in {
      transformer.fromString(input).toRenderedString.transform.assertEquals(output)
    }
  }
  
}
