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

import java.io.ByteArrayInputStream

import cats.effect.IO
import laika.api.MarkupParser
import laika.ast.helper.ModelBuilder
import laika.format.Markdown
import laika.io.implicits._
import laika.io.text.SequentialParser
import laika.io.runtime.TestContexts.blocker

import scala.io.Codec


class SequentialParserSpec extends IOSpec
                           with ModelBuilder {

  
  val parser: SequentialParser[IO] = MarkupParser.of(Markdown).io(blocker).sequential[IO].build


  "The SequentialParser" should {

    "parse Markdown from a string" in {
      val input =
        """aaa
          |bbb
          |ccc""".stripMargin
      parser.fromString(input).parse.map(_.content).assertEquals(root(p(input)))
    }

    "parse Markdown from a file" in {
      val input =
        """aaa
          |bbb
          |ccc""".stripMargin
      val filename = getClass.getResource("/testInput.md").getFile
      parser.fromFile(filename).parse.map(_.content).assertEquals(root(p(input)))
    }

    "parse Markdown from an empty file" in {
      val filename = getClass.getResource("/emptyInput.md").getFile
      parser.fromFile(filename).parse.map(_.content).assertEquals(root())
    }

    "parse Markdown from a java.io.InputStream instance" in {
      val input = """aaa
        |bbb
        |ccc""".stripMargin
      val stream = IO(new ByteArrayInputStream(input.getBytes()))
      parser.fromStream(stream).parse.map(_.content).assertEquals(root(p(input)))
    }
    
    "parse Markdown from a java.io.InputStream instance, specifying the encoding explicitly" in {
      val input = """äää
        |ööö
        |üüü""".stripMargin
      val stream = IO(new ByteArrayInputStream(input.getBytes("ISO-8859-1")))
      parser.fromStream(stream)(Codec.ISO8859).parse.map(_.content).assertEquals(root(p(input)))
    }
    
    "parse Markdown from a java.io.InputStream instance, specifying the encoding implicitly" in {
      val input = """äää
        |ööö
        |üüü""".stripMargin
      val stream = IO(new ByteArrayInputStream(input.getBytes("ISO-8859-1")))
      implicit val codec:Codec = Codec.ISO8859
      parser.fromStream(stream).parse.map(_.content).assertEquals(root(p(input)))
    }
  
  }
  
}
