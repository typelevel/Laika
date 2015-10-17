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

package laika.parse.css

import java.io.ByteArrayInputStream
import java.io.StringReader
import scala.io.Codec
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import laika.parse.css.Styles._
import laika.tree.Templates.TemplateString
import laika.tree.Templates.TemplateContextReference
import laika.tree.Documents.Root
import laika.tree.Documents.Path

class ParseStyleSheetAPISpec extends FlatSpec 
                             with Matchers
                             with StyleBuilders {

  
  trait Setup {
    def input: String
    def expected: Set[StyleDeclaration]
    def path: Path = Root
    
    def assert(actual: StyleDeclarationSet): Unit = {
      actual.styles should be (expected)
      actual.paths should be (Set(path))
    }
  }
  
  trait AsciiInput extends Setup {
    
    val input = """Type {
      | foo: bar;
      |}""".stripMargin
      
    val expected = Set(styleDecl(ElementType("Type")))
        
  }

  trait UmlautInput extends Setup {
    
    val input = """Type {
      | foo: bär;
      |}""".stripMargin
        
    val expected = Set(styleDecl(Map("foo" -> "bär"), ElementType("Type")))
        
  }
      
  "The ParseStyleSheet API" should "allow parsing a style sheet from a string" in {
    new AsciiInput {
      assert(ParseStyleSheet fromString input)
    }
  }
  
  it should "allow parsing a style sheet from a file" in {
    new AsciiInput {
      val filename = getClass.getResource("/test.css").getFile
      override val path = Path(filename)
      assert(ParseStyleSheet fromFile filename)
    }
  }
  
  it should "allow parsing a style sheet from a java.io.Reader instance" in {
    new AsciiInput {
      val reader = new StringReader(input)
      assert(ParseStyleSheet fromReader reader)
    }
  }
  
  it should "allow parsing a style sheet from a java.io.InputStream instance" in {
    new AsciiInput {
      val stream = new ByteArrayInputStream(input.getBytes())
      assert(ParseStyleSheet fromStream stream)
    }
  }

  it should "allow parsing a style sheet from a java.io.InputStream instance, specifying the encoding explicitly" in {
    new UmlautInput {
      val stream = new ByteArrayInputStream(input.getBytes("ISO-8859-1"))
      assert(ParseStyleSheet.fromStream(stream)(Codec.ISO8859))
    }
  }
  
  it should "allow parsing a style sheet from a java.io.InputStream instance, specifying the encoding implicitly" in {
    new UmlautInput {
      val stream = new ByteArrayInputStream(input.getBytes("ISO-8859-1"))
      implicit val codec:Codec = Codec.ISO8859
      assert(ParseStyleSheet fromStream stream)
    }
  }

  
}
