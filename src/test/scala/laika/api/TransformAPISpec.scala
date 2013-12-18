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
import laika.tree.helper.InputBuilder
import laika.tree.Documents.Root
import laika.tree.Documents.Static
import laika.io.Input
import laika.tree.Templates._
import laika.template.ParseTemplate

class TransformAPISpec extends FlatSpec 
                       with ShouldMatchers {

   
  val input = """# Title äöü
    |
    |text""".stripMargin 
  
  val output = """RootElement - Blocks: 2
    |. Header(1,Id(title) + Styles(title)) - Spans: 1
    |. . Text - 'Title äöü'
    |. Paragraph - Spans: 1
    |. . Text - 'text'""".stripMargin
    
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

  
  trait TreeTransformer extends InputBuilder {
    import laika.io.InputProvider.InputConfigBuilder
    import laika.io.OutputProvider.OutputConfigBuilder
    import laika.tree.helper.OutputBuilder._
    import laika.api.Transform.BatchConfigBuilder
    import laika.tree.Documents.Path
    import laika.tree.Documents.DocumentType
    import laika.template.ParseTemplate
    import laika.directive.Directives.Templates

    val dirs: String
    
    def input (source: String) = new InputConfigBuilder(parseTreeStructure(source), Codec.UTF8)
    def output (builder: TestProviderBuilder) = new OutputConfigBuilder(builder, Codec.UTF8)

    def transformTree = transformWith(identity)
    
    def transformWithConfig (config: String) = transformWith(_.withConfigString(config))
    def transformWithDocTypeMatcher (matcher: Path => DocumentType) = transformWith(_.withDocTypeMatcher(matcher))
    def transformWithTemplates (templates: ParseTemplate) = transformWith(_.withTemplates(templates))
    def transformWithDirective (directive: Templates.Directive) = transformWith(_.withTemplateDirectives(directive))
    def transformInParallel = transformWith(_.inParallel)
    
    private def transformWith (f: BatchConfigBuilder => BatchConfigBuilder) = {
      val builder = new TestProviderBuilder
      transform withConfig f(BatchConfigBuilder(input(dirs), output(builder)))
      builder.result
    }
    
    def root (content: Seq[TreeContent]) = RenderedTree(Root, content)
    
    val contents = Map(
      "name" -> "foo",
      "directive" -> "aa @:foo bar. bb",
      "dynDoc" -> "{{config.value}}",
      "template1" -> "{{document.content}}",
      "template2" -> "({{document.content}})",
      "conf" -> "value: abc"
    )
    
    val simpleResult = """RootElement - Blocks: 1
      |. Paragraph - Spans: 1
      |. . Text - 'foo'""".stripMargin
      
    def docs (values: (Path, String)*) = Documents(values map { case (path, content) => RenderedDocument(path, content) })

    def sorted (tree: RenderedTree): RenderedTree = tree.copy(content = tree.content map (sortedContent(_)))
        
    def sortedContent (content: TreeContent) = content match {
      case Documents(content) => Documents(content.sortBy(_.path.name))
      case Subtrees(content) => Subtrees(content.sortBy(_.path.name) map (sorted(_)))
    }
    
    def trees (values: (Path, Seq[TreeContent])*) = Subtrees(values map { case (path, content) => RenderedTree(path, content) })
  }
  
  
  it should "transform an empty tree" in {
    new TreeTransformer {
      val dirs = ""
      transformTree should be (root(Nil))
    }
  }
  
  it should "transform a tree with a single document" in {
    new TreeTransformer {
      val dirs = """- name.md:name"""
      transformTree should be (root(List(docs((Root / "name.txt", simpleResult)))))
    }
  }
  
  it should "transform a tree with a single subtree" in {
    new TreeTransformer {
      val dirs = """+ subtree"""
      transformTree should be (root(List(trees((Root / "subtree", Nil)))))
    }
  }
  
  it should "transform a tree with a dynamic document populated by a config file in the directory" in {
    new TreeTransformer {
      val dirs = """- main.dynamic.html:dynDoc
          |- directory.conf:conf""".stripMargin
      val result = """RootElement - Blocks: 1
          |. TemplateRoot - Spans: 1
          |. . TemplateString - 'abc'""".stripMargin
      transformTree should be (root(List(docs((Root / "main.txt", result)))))
    }
  }
  
  it should "transform a tree with a dynamic document populated by a root config string" in {
    new TreeTransformer {
      val dirs = """- main.dynamic.html:dynDoc"""
      val result = """RootElement - Blocks: 1
          |. TemplateRoot - Spans: 1
          |. . TemplateString - 'def'""".stripMargin
      transformWithConfig("value: def") should be (root(List(docs((Root / "main.txt", result)))))
    }
  }
  
  it should "transform a tree with a static document" in {
    new TreeTransformer {
      val dirs = """- omg.js:name"""
      transformTree should be (root(List(docs((Root / "omg.js", "foo")))))
    }
  }
  
  it should "ignore directories that match the default exclude pattern" in {
    new TreeTransformer {
      val dirs = """- name.md:name
        |+ .git
        |  - file1:name
        |  - file2:name""".stripMargin
      transformTree should be (root(List(docs((Root / "name.txt", simpleResult)))))
    }
  }
  
  it should "ignore files that match the default exclude pattern" in {
    new TreeTransformer {
      val dirs = """- name.md:name
        |- foo.git:name""".stripMargin
      transformTree should be (root(List(docs((Root / "name.txt", simpleResult)))))
    }
  }
  
  it should "transform a tree with a custom document type matcher" in {
    new TreeTransformer {
      val dirs = """- name.md:name
        |- main.dynamic.html:name""".stripMargin
      transformWithDocTypeMatcher(_ => Static) should be (root(List(docs(
        (Root / "name.md", "foo"),
        (Root / "main.dynamic.html", "foo")
      ))))
    }
  }
  
  it should "transform a tree with a custom template engine" in {
    new TreeTransformer {
      val dirs = """- main1.dynamic.txt:name
        |- main2.dynamic.txt:name""".stripMargin
      val parser: Input => TemplateDocument = 
        input => TemplateDocument(input.path, TemplateRoot(List(TemplateString("$$" + input.asParserInput.source))), null)
      val result = """RootElement - Blocks: 1
        |. TemplateRoot - Spans: 1
        |. . TemplateString - '$$foo'""".stripMargin
      transformWithTemplates(ParseTemplate as parser) should be (root(List(docs(
        (Root / "main1.txt", result),
        (Root / "main2.txt", result)
      ))))
    }
  }
  
  it should "transform a tree with a template directive" in {
    import laika.directive.Directives._
    import laika.directive.Directives.Templates
    import laika.directive.Directives.Templates.Combinators._
    
    val directive = Templates.create("foo") {
      attribute(Default) map { TemplateString(_) }
    }
    new TreeTransformer {
      val dirs = """- main1.dynamic.txt:directive
        |- main2.dynamic.txt:directive""".stripMargin
      val result = """RootElement - Blocks: 1
        |. TemplateRoot - Spans: 3
        |. . TemplateString - 'aa '
        |. . TemplateString - 'bar'
        |. . TemplateString - ' bb'""".stripMargin
      transformWithDirective(directive) should be (root(List(docs(
        (Root / "main1.txt", result),
        (Root / "main2.txt", result)
      ))))
    }
  }
  
  it should "transform a tree with all available file types" in {
    new TreeTransformer {
      val dirs = """- doc1.md:name
        |- doc2.md:name
        |- default.template.html:template1
        |+ dir1
        |  - ignore.svn:name
        |  - default.template.html:template2
        |  - doc3.md:name
        |  - doc4.md:name
        |+ dir2
        |  - omg.js:name
        |  - doc5.md:name
        |  - doc6.md:name
        |+ .git
        |  - file1:name
        |  - file2:name""".stripMargin
      val withTemplate1 = """RootElement - Blocks: 1
        |. Paragraph - Spans: 1
        |. . Text - 'foo'""".stripMargin  
      val withTemplate2 = """RootElement - Blocks: 1
        |. TemplateRoot - Spans: 3
        |. . TemplateString - '('
        |. . EmbeddedRoot(0) - Blocks: 1
        |. . . Paragraph - Spans: 1
        |. . . . Text - 'foo'
        |. . TemplateString - ')'""".stripMargin  
      transformTree should be (root(List(
        docs(
          (Root / "doc1.txt", withTemplate1),
          (Root / "doc2.txt", withTemplate1)
        ),
        trees(
          (Root / "dir1", List(docs(
            (Root / "dir1" / "doc3.txt", withTemplate2),
            (Root / "dir1" / "doc4.txt", withTemplate2)  
          ))),
          (Root / "dir2", List(docs(
            (Root / "dir2" / "doc5.txt", withTemplate1),
            (Root / "dir2" / "doc6.txt", withTemplate1),  
            (Root / "dir2" / "omg.js", "foo")  
          )))
        )
      )))
    }
  }
  
  it should "transform a document tree in parallel" in {
    new TreeTransformer {
      val dirs = """- doc1.md:name
        |- doc2.md:name
        |+ dir1
        |  - doc3.md:name
        |  - doc4.md:name
        |  - doc5.md:name
        |+ dir2
        |  - doc6.md:name
        |  - doc7.md:name
        |  - doc8.md:name""".stripMargin
      sorted(transformInParallel) should be (root(List(
          docs(
            (Root / "doc1.txt", simpleResult),
            (Root / "doc2.txt", simpleResult)
          ),
          trees(
            (Root / "dir1", List(docs(
              (Root / "dir1" / "doc3.txt", simpleResult),
              (Root / "dir1" / "doc4.txt", simpleResult),
              (Root / "dir1" / "doc5.txt", simpleResult)  
            ))),
            (Root / "dir2", List(docs(
              (Root / "dir2" / "doc6.txt", simpleResult),
              (Root / "dir2" / "doc7.txt", simpleResult),  
              (Root / "dir2" / "doc8.txt", simpleResult)  
            )))
          )
        )))
    }
  }
  
  trait FileSystemTest {
    import laika.tree.helper.OutputBuilder.readFile
    
    def renderedDynDoc (num: Int) = """RootElement - Blocks: 1
      |. TemplateRoot - Spans: 1
      |. . TemplateString - 'Doc""".stripMargin + num + "'"
      
    def renderedDoc (num: Int) = """RootElement - Blocks: 1
      |. Paragraph - Spans: 1
      |. . Text - 'Doc""".stripMargin + num + "'"
      
    def readFiles (base: String) = {
      readFile(base+"/doc1.txt") should be (renderedDoc(1))
      readFile(base+"/doc2.txt") should be (renderedDoc(2))
      readFile(base+"/dir1/doc3.txt") should be (renderedDoc(3))
      readFile(base+"/dir1/doc4.txt") should be (renderedDoc(4))
      readFile(base+"/dir2/doc5.txt") should be (renderedDoc(5))
      readFile(base+"/dir2/doc6.txt") should be (renderedDoc(6))
    }
  }
      
  it should "read from and write to directories" in {
    import laika.tree.helper.OutputBuilder.createTempDirectory
    new FileSystemTest {
      val sourceName = getClass.getResource("/trees/a/").getFile
      val targetDir = createTempDirectory("renderToDir")
      transform fromDirectory sourceName toDirectory targetDir
      readFiles(targetDir.getPath)
    }
  }
  

}

  