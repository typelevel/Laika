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

import java.io.ByteArrayOutputStream
import java.io.File
import java.io.StringWriter

import scala.io.Codec
import scala.io.Codec.charset2codec
import scala.io.Source

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import laika.render.PrettyPrint
import laika.tree.Elements.Text
import laika.tree.helper.ModelBuilder

class RenderAPISpec extends FlatSpec 
										with ShouldMatchers
										with ModelBuilder {

  
  val document = doc(p("aaö"), p("bbb"))
  
  val expected = """Document - Blocks: 2
			|. Paragraph - Spans: 1
			|. . Text - 'aaö'
			|. Paragraph - Spans: 1
			|. . Text - 'bbb'""".stripMargin
  
  
  "The Render API" should "render a document to a string" in {
    (Render as PrettyPrint from document toString) should be (expected)
  }
  
  it should "render a document to a builder" in {
    val builder = new StringBuilder
    Render as PrettyPrint from document toBuilder builder
    builder.toString should be (expected)
  }
  
  it should "render a document to a file" in {
    val f = File.createTempFile("output", null)
    
    Render as PrettyPrint from document toFile f
    
    val source = Source.fromFile(f)
		val fileContent = source.mkString
		source.close()
		f.delete()
		
		fileContent should be (expected)
  }
  
  it should "render a document to a java.io.Writer" in {
    val writer = new StringWriter
 		Render as PrettyPrint from document toWriter writer
 		writer.toString should be (expected)
  }
  
  it should "render a document to a java.io.OutputStream" in {
    val stream = new ByteArrayOutputStream
 		Render as PrettyPrint from document toStream stream
 		stream.toString should be (expected)
  }
  
  it should "render a document to a java.io.OutputStream, specifying the encoding explicitly" in {
    val stream = new ByteArrayOutputStream
 		(Render as PrettyPrint from document).toStream(stream)(Codec.ISO8859)
 		stream.toString("ISO-8859-1") should be (expected)
  }
  
  it should "render a document to a java.io.OutputStream, specifying the encoding implicitly" in {
    implicit val codec:Codec = Codec.ISO8859
    val stream = new ByteArrayOutputStream
 		Render as PrettyPrint from document toStream stream
 		stream.toString("ISO-8859-1") should be (expected)
  }
  
  it should "allow to override the default renderer for specific element types" in {
    val render = Render as PrettyPrint using { out => { case Text(content) => out << "String - '" << content << "'" } }
    val modifiedResult = expected.replaceAllLiterally("Text", "String")
    (render from document toString) should be (modifiedResult)
  }
  

}

	