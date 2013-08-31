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
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.StringReader
import java.io.StringWriter

import scala.io.Codec
import scala.io.Codec.charset2codec
import scala.io.Source

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import laika.parse.markdown.Markdown
import laika.render.PrettyPrint
import laika.tree.Elements.Text
import laika.tree.helper.ModelBuilder
 
class TransformAPISpec extends FlatSpec 
                       with ShouldMatchers
                       with ModelBuilder {

   
  val input = """# Title äöü
    |
    |text""".stripMargin 
  
  val output = """RootElement - Blocks: 1
    |. Section
    |. . Header(1) - Spans: 1
    |. . . Text - 'Title äöü'
    |. . Content - Blocks: 1
    |. . . Paragraph - Spans: 1
    |. . . . Text - 'text'""".stripMargin
    
  val transform = Transform from Markdown to PrettyPrint
  
  
  "The Transform API" should "transform from string to string" in {
    (transform fromString input toString) should be (output)
  }
  
  it should "transform from string to string builder" in {
    val builder = new StringBuilder
    Transform from Markdown to PrettyPrint fromString input toBuilder builder
    builder.toString should be (output)
  }
  
  it should "transform from file to file" in {
    val inFile = getClass.getResource("/testInput2.md").getFile
    val outFile = File.createTempFile("output", null)
    implicit val codec:Codec = Codec.UTF8
    
    transform fromFile inFile toFile outFile
    
    val source = Source.fromFile(outFile)
    val fileContent = source.mkString
    source.close()
    outFile.delete()
    
    fileContent should be (output)
  }
  
  it should "transform from a java.io.Reader to a java.io.Writer" in {
    val reader = new StringReader(input)
    val writer = new StringWriter
    transform fromReader reader toWriter writer
    writer.toString should be (output)
  }
  
  it should "transform from a java.io.InputStream to a java.io.OutputStream" in {
    val inStream = new ByteArrayInputStream(input.getBytes())
    val outStream = new ByteArrayOutputStream
    transform fromStream inStream toStream outStream
    outStream.toString should be (output)
  }
  
  it should "transform from a java.io.InputStream to a java.io.OutputStream, specifying the encoding explicitly" in {
    val inStream = new ByteArrayInputStream(input.getBytes("ISO-8859-1"))
    val outStream = new ByteArrayOutputStream
    val codec = Codec.ISO8859
    transform.fromStream(inStream)(codec).toStream(outStream)(codec)
    outStream.toString("ISO-8859-1") should be (output)
  }
  
  it should "transform from a java.io.InputStream to a java.io.OutputStream, specifying the encoding implicitly" in {
    val inStream = new ByteArrayInputStream(input.getBytes("ISO-8859-1"))
    val outStream = new ByteArrayOutputStream
    implicit val codec:Codec = Codec.ISO8859
    transform fromStream inStream toStream outStream
    outStream.toString("ISO-8859-1") should be (output)
  }
  
  it should "allow to override the default renderer for specific element types" in {
    val modifiedOutput = output.replaceAllLiterally(". Text", ". String")
    val transformCustom = transform rendering { out => { case Text(content,_) => out << "String - '" << content << "'" } }
    (transformCustom fromString input toString) should be (modifiedOutput)
  }
  
  it should "allow to specify a custom rewrite rule" in {
    val modifiedOutput = output.replaceAllLiterally("äöü", "zzz")
    val transformCustom = transform usingRule { case Text("Title äöü",_) => Some(Text("Title zzz")) }
    (transformCustom fromString input toString) should be (modifiedOutput)
  }
  
  it should "allow to specify multiple rewrite rules" in {
    val modifiedOutput = output.replaceAllLiterally("äöü", "zzz").replaceAllLiterally("text", "new")
    val transformCustom = transform usingRule { case Text("Title äöü",_) => Some(Text("Title zzz")) } usingRule
                                              { case Text("text",_) => Some(Text("new")) }
    (transformCustom fromString input toString) should be (modifiedOutput)
  }
  
  it should "allow to specify a custom rewrite rule that depends on the document" in {
    val modifiedOutput = output.replaceAllLiterally("äöü", "2")
    val transformCustom = transform creatingRule { context => { case Text("Title äöü",_) => Some(Text("Title " + context.document.content.content.length)) }}
    (transformCustom fromString input toString) should be (modifiedOutput)
  }
  

}

  