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
import laika.tree.Documents.DocumentTree
import laika.tree.Documents.Document
import laika.tree.Documents.Path
import laika.tree.Documents.Root
import laika.tree.Templates._
import laika.tree.helper.OutputBuilder._
import laika.io.OutputProvider.OutputConfigBuilder
import laika.io.Input
import laika.io.OutputProvider.Directory

class RenderAPISpec extends FlatSpec 
                    with ShouldMatchers
                    with ModelBuilder {

  
  val document = doc(p("aaö"), p("bbb"))
  
  val expected = """RootElement - Blocks: 2
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
    
    readFile(f) should be (expected)
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
    val render = Render as PrettyPrint using { out => { case Text(content,_) => out << "String - '" << content << "'" } }
    val modifiedResult = expected.replaceAllLiterally("Text", "String")
    (render from document toString) should be (modifiedResult)
  }
  
  def tree (builder: TestProviderBuilder) = new OutputConfigBuilder(builder, Codec.UTF8)
  
  def markupDoc (num: Int, path: Path = Root)  = new Document(path / ("doc"+num), doc(p("Doc"+num)))
  def dynamicDoc (num: Int, path: Path = Root) = new Document(path / ("doc"+num), doc(TemplateRoot(List(TemplateString("Doc"+num)))))
  
  def staticDoc (num: Int, path: Path = Root) = Input.fromString("Static"+num, path / ("static"+num+".txt"))
  
  
  def renderedDynDoc (num: Int) = """RootElement - Blocks: 1
    |. TemplateRoot - Spans: 1
    |. . TemplateString - 'Doc""".stripMargin + num + "'"
    
  def renderedDoc (num: Int) = """RootElement - Blocks: 1
      |. Paragraph - Spans: 1
      |. . Text - 'Doc""".stripMargin + num + "'"
  
  it should "render an empty tree" in {
    val input = new DocumentTree(Root, Nil)
    val builder = new TestProviderBuilder
    Render as PrettyPrint from input toTree tree(builder)
    builder.result should be (RenderedTree(Root, Nil))
  }
  
  it should "render a tree with a single document" in {
    val input = new DocumentTree(Root, List(new Document(Root / "doc", document)))
    val builder = new TestProviderBuilder
    Render as PrettyPrint from input toTree tree(builder)
    builder.result should be (RenderedTree(Root, List(Documents(List(RenderedDocument(Root / "doc.txt", expected))))))
  }
  
  it should "render a tree with a single dynamic document" in {
    val input = new DocumentTree(Root, Nil, dynamicDocuments = List(dynamicDoc(1)))
    val builder = new TestProviderBuilder
    Render as PrettyPrint from input toTree tree(builder)
    builder.result should be (RenderedTree(Root, List(Documents(List(RenderedDocument(Root / "doc1.txt", renderedDynDoc(1)))))))
  }
  
  it should "render a tree with a single static document" in {
    val input = new DocumentTree(Root, Nil, staticDocuments = List(staticDoc(1)))
    val builder = new TestProviderBuilder
    Render as PrettyPrint from input toTree tree(builder)
    builder.result should be (RenderedTree(Root, List(Documents(List(RenderedDocument(Root / "static1.txt", "Static1"))))))
  }
  
  it should "render a tree with all available file types" in {
    val input = new DocumentTree(Root,
      documents = List(markupDoc(1), markupDoc(2)),
      dynamicDocuments = List(dynamicDoc(1), dynamicDoc(2)),
      staticDocuments = List(staticDoc(1), staticDoc(2)),
      subtrees = List(
        new DocumentTree(Root / "dir1",
          documents = List(markupDoc(3), markupDoc(4)),
          dynamicDocuments = List(dynamicDoc(3), dynamicDoc(4)),
          staticDocuments = List(staticDoc(3), staticDoc(4))
        ),
        new DocumentTree(Root / "dir2",
          documents = List(markupDoc(5), markupDoc(6)),
          dynamicDocuments = List(dynamicDoc(5), dynamicDoc(6)),
          staticDocuments = List(staticDoc(5), staticDoc(6))
        )
      )
    )
    val builder = new TestProviderBuilder
    Render as PrettyPrint from input toTree tree(builder)
    builder.result should be (RenderedTree(Root, List(
      Documents(List(
        RenderedDocument(Root / "doc1.txt", renderedDoc(1)),
        RenderedDocument(Root / "doc2.txt", renderedDoc(2)),
        RenderedDocument(Root / "doc1.txt", renderedDynDoc(1)),
        RenderedDocument(Root / "doc2.txt", renderedDynDoc(2)),
        RenderedDocument(Root / "static1.txt", "Static1"),
        RenderedDocument(Root / "static2.txt", "Static2")
      )),
      Subtrees(List(
        RenderedTree(Root / "dir1", List(
          Documents(List(
            RenderedDocument(Root / "dir1" / "doc3.txt", renderedDoc(3)),
            RenderedDocument(Root / "dir1" / "doc4.txt", renderedDoc(4)),
            RenderedDocument(Root / "dir1" / "doc3.txt", renderedDynDoc(3)),
            RenderedDocument(Root / "dir1" / "doc4.txt", renderedDynDoc(4)),
            RenderedDocument(Root / "dir1" / "static3.txt", "Static3"),
            RenderedDocument(Root / "dir1" / "static4.txt", "Static4")
         ))
      )),
       RenderedTree(Root / "dir2", List(
        Documents(List(
          RenderedDocument(Root / "dir2" / "doc5.txt", renderedDoc(5)),
          RenderedDocument(Root / "dir2" / "doc6.txt", renderedDoc(6)),
          RenderedDocument(Root / "dir2" / "doc5.txt", renderedDynDoc(5)),
          RenderedDocument(Root / "dir2" / "doc6.txt", renderedDynDoc(6)),
          RenderedDocument(Root / "dir2" / "static5.txt", "Static5"),
          RenderedDocument(Root / "dir2" / "static6.txt", "Static6")
        ))
      ))))
    )))
  }
  
  trait FileSystemTest {
    val input = new DocumentTree(Root,
      documents = List(markupDoc(1), markupDoc(2)),
      subtrees = List(
        new DocumentTree(Root / "dir1",
          documents = List(markupDoc(3), markupDoc(4))
        ),
        new DocumentTree(Root / "dir2",
          documents = List(markupDoc(5), markupDoc(6))
        )
      )
    )
    
    def readFiles (base: String) = {
      readFile(base+"/doc1.txt") should be (renderedDoc(1))
      readFile(base+"/doc2.txt") should be (renderedDoc(2))
      readFile(base+"/dir1/doc3.txt") should be (renderedDoc(3))
      readFile(base+"/dir1/doc4.txt") should be (renderedDoc(4))
      readFile(base+"/dir2/doc5.txt") should be (renderedDoc(5))
      readFile(base+"/dir2/doc6.txt") should be (renderedDoc(6))
    }
  }
  
  it should "render to a directory using the toDirectory method" in {
    new FileSystemTest {
      val f = createTempDirectory("renderToDir")
      Render as PrettyPrint from input toDirectory f
      readFiles(f.getPath)
    }    
  }
  
  it should "render to a directory using the Directory object" in {
    new FileSystemTest {
      val f = createTempDirectory("renderToTree")
      Render as PrettyPrint from input toTree Directory(f)
      readFiles(f.getPath)
    }    
  }
  
  it should "render to a directory in parallel" in {
    new FileSystemTest {
      val f = createTempDirectory("renderParallel")
      Render as PrettyPrint from input toTree Directory(f).inParallel
      readFiles(f.getPath)
    }    
  }
  

}

  