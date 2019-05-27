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

import cats.effect.IO
import laika.api.Transformer
import laika.format._
import laika.io.Sequential.SequentialTransformer
import org.scalatest.{FlatSpec, Matchers}

import scala.io.{Codec, Source}

class SequentialTransformerSpec extends FlatSpec 
                       with Matchers {

   
  val input = """# Title äöü
    |
    |text""".stripMargin 
  
  val output = """RootElement - Blocks: 2
    |. Title(Id(title) + Styles(title)) - Spans: 1
    |. . Text - 'Title äöü'
    |. Paragraph - Spans: 1
    |. . Text - 'text'""".stripMargin
    
  val transformer: SequentialTransformer[IO] = Sequential(Transformer.from(Markdown).to(AST)).build[IO]
  
  
//  "The Transform API" should "transform from string to string" in {
//    (transform transform input) should be (output)
//  }
  
  "The Transform API" should "transform from file to file" ignore {
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
  
  it should "transform from a java.io.Reader to a java.io.Writer" ignore {
//    val reader = new StringReader(input)
//    val writer = new StringWriter
//    (transform fromReader reader toWriter writer).execute
//    writer.toString should be (output)
  }
  
  it should "transform from a java.io.InputStream to a java.io.OutputStream" ignore {
//    val inStream = new ByteArrayInputStream(input.getBytes())
//    val outStream = new ByteArrayOutputStream
//    (transform fromStream inStream toStream outStream).execute
//    outStream.toString should be (output)
  }
  
  it should "transform from a java.io.InputStream to a java.io.OutputStream, specifying the encoding explicitly" ignore {
//    val inStream = new ByteArrayInputStream(input.getBytes("ISO-8859-1"))
//    val outStream = new ByteArrayOutputStream
//    val codec = Codec.ISO8859
//    (transform.fromStream(inStream)(codec).toStream(outStream)(codec)).execute
//    outStream.toString("ISO-8859-1") should be (output)
  }
  
  it should "transform from a java.io.InputStream to a java.io.OutputStream, specifying the encoding implicitly" ignore {
//    val inStream = new ByteArrayInputStream(input.getBytes("ISO-8859-1"))
//    val outStream = new ByteArrayOutputStream
//    implicit val codec:Codec = Codec.ISO8859
//    (transform fromStream inStream toStream outStream).execute
//    outStream.toString("ISO-8859-1") should be (output)
  }
  
  // TODO - 0.12 - move these back to core
//  it should "allow to override the default renderer for specific element types" in {
//    val modifiedOutput = output.replaceAllLiterally(". Text", ". String")
//    val transformCustom = transform rendering { case (_, Text(content,_)) => s"String - '$content'" }
//    (transformCustom transform input) should be (modifiedOutput)
//  }
//  
//  it should "allow to specify a custom rewrite rule" in {
//    val modifiedOutput = output.replaceAllLiterally("äöü", "zzz")
//    val transformCustom = transform usingSpanRule { case Text("Title äöü",_) => Replace(Text("Title zzz")) }
//    (transformCustom transform input) should be (modifiedOutput)
//  }
//  
//  it should "allow to specify multiple rewrite rules" in {
//    val modifiedOutput = output.replaceAllLiterally("äöü", "zzz").replaceAllLiterally("text", "new")
//    val transformCustom = transform usingSpanRule { case Text("Title äöü",_) => Replace(Text("Title zzz")) } usingSpanRule
//                                                  { case Text("text",_)      => Replace(Text("new")) }
//    (transformCustom transform input) should be (modifiedOutput)
//  }
//  
//  it should "allow to specify a custom rewrite rule that depends on the document" in {
//    val modifiedOutput = output.replaceAllLiterally("äöü", "2")
//    val transformCustom = transform creatingRule { cursor => RewriteRules.forSpans { 
//      case Text("Title äöü",_) => Replace(Text("Title " + cursor.target.content.content.length)) 
//    }}
//    (transformCustom transform input) should be (modifiedOutput)
//  }

  
}
  