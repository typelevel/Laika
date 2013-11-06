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

package laika.template

import java.io.ByteArrayInputStream
import java.io.StringReader
import scala.io.Codec
import scala.io.Codec.charset2codec
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import laika.parse.markdown.Markdown
import laika.tree.Elements.ExternalLinkDefinition
import laika.tree.Elements.LinkReference
import laika.tree.Elements.Text
import laika.tree.helper.ModelBuilder
import laika.tree.Templates.TemplateString
import laika.tree.Templates.ContextReference

class ParseTemplateAPISpec extends FlatSpec 
                   with ShouldMatchers
                   with ModelBuilder {

  
  trait AsciiInput {
    
    val input = """aaa
        |{{document.content}}
        |bbb""".stripMargin
        
    val result = tRoot(
      TemplateString("aaa\n"), 
      ContextReference("document.content"), 
      TemplateString("\nbbb")
    )
        
  }
  
  trait UmlautInput {
    
    val input = """äää
        |{{document.content}}
        |üüü""".stripMargin
        
    val result = tRoot(
      TemplateString("äää\n"), 
      ContextReference("document.content"), 
      TemplateString("\nüüü")
    )
        
  }
      
  "The ParseTemplate API" should "allow parsing a template from a string" in {
    new AsciiInput {
      (ParseTemplate fromString input).content should be (result)
    }
  }
  
  it should "allow parsing a template from a file" in {
    new AsciiInput {
      val filename = getClass.getResource("/test.template.txt").getFile
      (ParseTemplate fromFile filename).content should be (result) 
    }
  }
  
  it should "allow parsing a template from a java.io.Reader instance" in {
    new AsciiInput {
      val reader = new StringReader(input)
      (ParseTemplate fromReader reader).content should be (result)
    }
  }
  
  it should "allow parsing a template from a java.io.InputStream instance" in {
    new AsciiInput {
      val stream = new ByteArrayInputStream(input.getBytes())
      (ParseTemplate fromStream stream).content should be (result)
    }
  }

  it should "allow parsing a template from a java.io.InputStream instance, specifying the encoding explicitly" in {
    new UmlautInput {
      val stream = new ByteArrayInputStream(input.getBytes("ISO-8859-1"))
      ParseTemplate.fromStream(stream)(Codec.ISO8859).content should be (result)
    }
  }
  
  it should "allow parsing a template from a java.io.InputStream instance, specifying the encoding implicitly" in {
    new UmlautInput {
      val stream = new ByteArrayInputStream(input.getBytes("ISO-8859-1"))
      implicit val codec:Codec = Codec.ISO8859
      (ParseTemplate fromStream stream).content should be (result)
    }
  }

  
}