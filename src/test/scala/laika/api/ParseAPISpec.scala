/*
 * Copyright 2013 the original author or authors.
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

package laika.api

import java.io.ByteArrayInputStream
import java.io.StringReader

import scala.io.Codec
import scala.io.Codec.charset2codec

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import laika.parse.markdown.Markdown
import laika.tree.Elements.LinkDefinition
import laika.tree.Elements.LinkReference
import laika.tree.Elements.Text
import laika.tree.helper.ModelBuilder

class ParseAPISpec extends FlatSpec 
                   with ShouldMatchers
                   with ModelBuilder {

  
  "The Parse API" should "allow parsing Markdown from a string" in {
    val input = """aaa
      |bbb
      |ccc""".stripMargin
    Parse as Markdown fromString input should be (doc(p(input)))
  }
  
  it should "allow parsing Markdown from a file" in {
    val input = """aaa
      |bbb
      |ccc""".stripMargin
    val filename = getClass.getResource("/testInput.md").getFile
    Parse as Markdown fromFile filename should be (doc(p(input))) 
  }
  
  it should "allow parsing Markdown from a java.io.Reader instance" in {
    val input = """aaa
      |bbb
      |ccc""".stripMargin
    val reader = new StringReader(input)
    Parse as Markdown fromReader reader should be (doc(p(input)))
  }
  
  it should "allow parsing Markdown from a java.io.InputStream instance" in {
    val input = """aaa
      |bbb
      |ccc""".stripMargin
    val stream = new ByteArrayInputStream(input.getBytes())
    Parse as Markdown fromStream stream should be (doc(p(input)))
  }
  
  it should "allow parsing Markdown from a java.io.InputStream instance, specifying the encoding explicitly" in {
    val input = """äää
      |ööö
      |üüü""".stripMargin
    val stream = new ByteArrayInputStream(input.getBytes("ISO-8859-1"))
    (Parse as Markdown).fromStream(stream)(Codec.ISO8859) should be (doc(p(input)))
  }
  
  it should "allow parsing Markdown from a java.io.InputStream instance, specifying the encoding implicitly" in {
    val input = """äää
      |ööö
      |üüü""".stripMargin
    val stream = new ByteArrayInputStream(input.getBytes("ISO-8859-1"))
    implicit val codec:Codec = Codec.ISO8859
    Parse as Markdown fromStream stream should be (doc(p(input)))
  }
  
  it should "allow parsing Markdown with all link references resolved through the default rewrite rules" in {
    val input = """[link][id]
      |
      |[id]: http://foo/""".stripMargin
    Parse as Markdown fromString input should be (doc(p(link(txt("link")).url("http://foo/"))))
  }
  
  it should "allow parsing Markdown into a raw document, without applying the default rewrite rules" in {
    val input = """[link][id]
      |
      |[id]: http://foo/""".stripMargin
    (Parse as Markdown asRawDocument) fromString input should be (doc (p (LinkReference(List(Text("link")), "id", "[", "][id]")), LinkDefinition("id","http://foo/",None)))
  }
  

}