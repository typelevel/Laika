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

import cats.effect.IO
import laika.api.MarkupParser
import laika.ast.helper.ModelBuilder
import laika.format.Markdown
import laika.io.Sequential.SequentialParser
import org.scalatest.{FlatSpec, Matchers}


class SequentialParserSpec extends FlatSpec 
                   with Matchers
                   with ModelBuilder {
  
  
  val parser: SequentialParser[IO] = Sequential(MarkupParser.of(Markdown)).build[IO]


  it should "allow parsing Markdown from a string" in {
    val input = """aaa
      |bbb
      |ccc""".stripMargin
     parser.fromString(input).parse.unsafeRunSync().content should be (root(p(input))) 
  }
  
  it should "allow parsing Markdown from a file" in {
    val input = """aaa
      |bbb
      |ccc""".stripMargin
    val filename = getClass.getResource("/testInput.md").getFile
    parser.fromFile(filename).parse.unsafeRunSync().content should be (root(p(input))) 
  }
  
  it should "allow parsing Markdown from a java.io.Reader instance" ignore {
//    val input = """aaa
//      |bbb
//      |ccc""".stripMargin
//    val reader = new StringReader(input)
//    (Parse as Markdown fromReader reader).execute.content should be (root(p(input)))
  }
  
  it should "allow parsing Markdown from a java.io.InputStream instance" ignore {
//    val input = """aaa
//      |bbb
//      |ccc""".stripMargin
//    val stream = new ByteArrayInputStream(input.getBytes())
//    (Parse as Markdown fromStream stream).execute.content should be (root(p(input)))
  }
  
  it should "allow parsing Markdown from a java.io.InputStream instance, specifying the encoding explicitly" ignore {
//    val input = """äää
//      |ööö
//      |üüü""".stripMargin
//    val stream = new ByteArrayInputStream(input.getBytes("ISO-8859-1"))
//    (Parse as Markdown).fromStream(stream)(Codec.ISO8859).execute.content should be (root(p(input)))
  }
  
  it should "allow parsing Markdown from a java.io.InputStream instance, specifying the encoding implicitly" ignore {
//    val input = """äää
//      |ööö
//      |üüü""".stripMargin
//    val stream = new ByteArrayInputStream(input.getBytes("ISO-8859-1"))
//    implicit val codec:Codec = Codec.ISO8859
//    (Parse as Markdown fromStream stream).execute.content should be (root(p(input)))
  }
  
}
