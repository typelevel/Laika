/*
 * Copyright 2013-2016 the original author or authors.
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

import java.io._

import laika.api.Transform.TransformMappedOutput
import laika.ast.DocumentType.Static
import laika.ast.Path.Root
import laika.ast._
import laika.ast.helper.{InputBuilder, OutputBuilder}
import laika.ast.helper.OutputBuilder.{readFile}
import laika.bundle.{BundleProvider, ExtensionBundle}
import laika.directive.Templates
import laika.format._
import laika.io.{StringTreeOutput, TreeInput}
import laika.parse.Parser
import laika.parse.text.TextParsers
import laika.render.{TextFormatter, TextWriter}
import laika.render.helper.RenderResult
import org.scalatest.{FlatSpec, Matchers}

import scala.io.{Codec, Source}

class TransformAPISpec extends FlatSpec 
                       with Matchers {

   
  val input = """# Title äöü
    |
    |text""".stripMargin 
  
  val output = """RootElement - Blocks: 2
    |. Title(Id(title) + Styles(title)) - Spans: 1
    |. . Text - 'Title äöü'
    |. Paragraph - Spans: 1
    |. . Text - 'text'""".stripMargin
    
  val transform = Transform from Markdown to AST2
  
  
  "The Transform API" should "transform from string to string" in {
    (transform fromString input toString) should be (output)
  }
  
  it should "transform from file to file" ignore {
    val inFile = getClass.getResource("/testInput2.md").getFile
    val outFile = File.createTempFile("output", null)
    implicit val codec:Codec = Codec.UTF8
    
    (transform fromFile inFile toFile outFile).execute
    
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
  
  it should "allow to override the default renderer for specific element types" in {
    val modifiedOutput = output.replaceAllLiterally(". Text", ". String")
    val transformCustom = transform rendering { case (_, Text(content,_)) => s"String - '$content'" }
    (transformCustom fromString input toString) should be (modifiedOutput)
  }
  
  it should "allow to specify a custom rewrite rule" in {
    val modifiedOutput = output.replaceAllLiterally("äöü", "zzz")
    val transformCustom = transform usingSpanRule { case Text("Title äöü",_) => Replace(Text("Title zzz")) }
    (transformCustom fromString input toString) should be (modifiedOutput)
  }
  
  it should "allow to specify multiple rewrite rules" in {
    val modifiedOutput = output.replaceAllLiterally("äöü", "zzz").replaceAllLiterally("text", "new")
    val transformCustom = transform usingSpanRule { case Text("Title äöü",_) => Replace(Text("Title zzz")) } usingSpanRule
                                                  { case Text("text",_)      => Replace(Text("new")) }
    (transformCustom fromString input toString) should be (modifiedOutput)
  }
  
  it should "allow to specify a custom rewrite rule that depends on the document" in {
    val modifiedOutput = output.replaceAllLiterally("äöü", "2")
    val transformCustom = transform creatingRule { cursor => RewriteRules.forSpans { 
      case Text("Title äöü",_) => Replace(Text("Title " + cursor.target.content.content.length)) 
    }}
    (transformCustom fromString input toString) should be (modifiedOutput)
  }

  
  trait TreeTransformer extends InputBuilder {
    import laika.ast.helper.OutputBuilder._
    import laika.ast.{DocumentType, Path}

    val dirs: String
    
    def input (source: String, docTypeMatcher: Path => DocumentType): TreeInput = parseTreeStructure(source, docTypeMatcher)

    def transformTree: RenderedTree = transformWith()
    def transformMultiMarkup: RenderedTree = transformWith(Transform from Markdown or ReStructuredText to AST2)
    
    def transformWithConfig (config: String): RenderedTree = transformWithBundle(BundleProvider.forConfigString(config))
    def transformWithDocTypeMatcher (matcher: PartialFunction[Path, DocumentType]): RenderedTree = transformWithBundle(BundleProvider.forDocTypeMatcher(matcher))
    def transformWithTemplates (parser: Parser[TemplateRoot]): RenderedTree = transformWithBundle(BundleProvider.forTemplateParser(parser))
    def transformWithDirective (directive: Templates.Directive): RenderedTree = transformWithBundle(BundleProvider.forTemplateDirective(directive))
    
    def transformInParallel: RenderedTree = transformWith(transform.inParallel)
    
    private def transformWith (transformer: TransformMappedOutput[TextFormatter] = transform): RenderedTree =
      OutputBuilder.RenderedTree.toTreeView(transformer.fromTreeInput(input(dirs, transformer.config.docTypeMatcher)).toOutputTree(StringTreeOutput).execute.rootTree)

    private def transformWithBundle (bundle: ExtensionBundle, transformer: TransformMappedOutput[TextFormatter] = transform): RenderedTree =
      transformWith(transformer.using(bundle))
    
    def root (content: Seq[TreeContent]) = RenderedTree(Root, content)
    
    val contents = Map(
      "name" -> "foo",
      "style" -> "13",
      "link" -> "[link](foo)",
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
        
    def sortedContent (content: TreeContent): TreeContent = content match {
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
  
  it should "transform a tree with a dynamic document populated by a config file in the directory" ignore {
    new TreeTransformer {
      val dirs = """- main.dynamic.txt:dynDoc
          |- directory.conf:conf""".stripMargin
      val result = """RootElement - Blocks: 1
          |. TemplateRoot - TemplateSpans: 1
          |. . TemplateString - 'abc'""".stripMargin
      transformTree should be (root(List(docs((Root / "main.txt", result)))))
    }
  }
  
  it should "transform a tree with a dynamic document populated by a root config string" ignore {
    new TreeTransformer {
      val dirs = """- main.dynamic.txt:dynDoc"""
      val result = """RootElement - Blocks: 1
          |. TemplateRoot - TemplateSpans: 1
          |. . TemplateString - 'def'""".stripMargin
      transformWithConfig("value: def") should be (root(List(docs((Root / "main.txt", result)))))
    }
  }
  
  it should "transform a tree with a static document" ignore {
    new TreeTransformer {
      val dirs = """- omg.js:name"""
      transformTree should be (root(List(docs((Root / "omg.js", "foo")))))
    }
  }
  
  it should "transform a tree with a custom document type matcher" ignore {
    new TreeTransformer {
      val dirs = """- name.md:name
        |- main.dynamic.html:name""".stripMargin
      transformWithDocTypeMatcher({case _ => Static}) should be (root(List(docs(
        (Root / "name.md", "foo"),
        (Root / "main.dynamic.html", "foo")
      ))))
    }
  }
  
  it should "transform a tree with a custom template engine" ignore { // TODO - 0.12 - switch to regular templates
    new TreeTransformer {
      val dirs = """- main1.dynamic.txt:name
        |- main2.dynamic.txt:name""".stripMargin
      val parser: Parser[TemplateRoot] = TextParsers.any ^^ { str => TemplateRoot(List(TemplateString("$$" + str))) }
      val result = """RootElement - Blocks: 1
        |. TemplateRoot - TemplateSpans: 1
        |. . TemplateString - '$$foo'""".stripMargin
      transformWithTemplates(parser) should be (root(List(docs(
        (Root / "main1.txt", result),
        (Root / "main2.txt", result)
      ))))
    }
  }
  
  it should "transform a tree with a custom style sheet engine" in {
    new TreeTransformer {
      // the AST renderer does not use stylesheets, so we must use XSL-FO here
      def styleDecl(fontSize: String) =
        StyleDeclaration(StylePredicate.ElementType("Paragraph"), "font-size" -> s"${fontSize}pt")
      val parser: Parser[Set[StyleDeclaration]] = TextParsers.any ^^ { n => Set(styleDecl(n)) }
      val dirs = """- doc1.md:name
        |- styles.fo.css:style""".stripMargin
      val result = RenderResult.fo.withDefaultTemplate("""<fo:block font-family="serif" font-size="13pt" space-after="3mm">foo</fo:block>""")
      val transform = Transform.from(Markdown).to(XSLFO2).inParallel.using(BundleProvider.forStyleSheetParser(parser))
      val renderResult = transform.fromTreeInput(input(dirs, transform.config.docTypeMatcher)).toOutputTree(StringTreeOutput).execute
      OutputBuilder.RenderedTree.toTreeView(renderResult.rootTree) should be (root(List(docs(
        (Root / "doc1.fo", result)
      ))))
    }
  }
  
  it should "transform a tree with a template directive" ignore { // TODO - 0.12 - switch to regular templates instead of dynamic documents
    import Templates.dsl._

    val directive = Templates.create("foo") {
      attribute(Default) map { TemplateString(_) }
    }
    new TreeTransformer {
      val dirs = """- main1.dynamic.txt:directive
        |- main2.dynamic.txt:directive""".stripMargin
      val result = """RootElement - Blocks: 1
        |. TemplateRoot - TemplateSpans: 3
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
      val dirs = """- doc1.md:link
        |- doc2.rst:link
        |- default.template.txt:template1
        |+ dir1
        |  - default.template.txt:template2
        |  - doc3.md:name
        |  - doc4.md:name
        |+ dir2
        |  - omg.js:name
        |  - doc5.md:name
        |  - doc6.md:name""".stripMargin
      val withTemplate1 = """RootElement - Blocks: 1
        |. Paragraph - Spans: 1
        |. . Text - 'foo'""".stripMargin  
      val withTemplate2 = """RootElement - Blocks: 1
        |. TemplateRoot - TemplateSpans: 3
        |. . TemplateString - '('
        |. . EmbeddedRoot(0) - Blocks: 1
        |. . . Paragraph - Spans: 1
        |. . . . Text - 'foo'
        |. . TemplateString - ')'""".stripMargin  
      val markdown = """RootElement - Blocks: 1
        |. Paragraph - Spans: 1
        |. . ExternalLink(foo,None) - Spans: 1
        |. . . Text - 'link'""".stripMargin
      val rst = """RootElement - Blocks: 1
        |. Paragraph - Spans: 1
        |. . Text - '[link](foo)'""".stripMargin
      transformMultiMarkup should be (root(List(
        docs(
          (Root / "doc1.txt", markdown),
          (Root / "doc2.txt", rst)
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
  
  trait GatheringTransformer extends InputBuilder {

    val srcRoot = """Title
      |=====
      |
      |bbb""".stripMargin
    
    val srcSub = """Sub Title
      |=========
      |
      |ccc""".stripMargin
      
    val contents = Map(
      "docRoot" -> srcRoot,
      "docSub" -> srcSub
    )
    
    val dirs = """- docRoot.rst:docRoot
        |+ dir
        |  - docSub.rst:docSub""".stripMargin
        
    val expectedResult = """RootElement - Blocks: 2
      |. Title(Id(title) + Styles(title)) - Spans: 1
      |. . Text - 'Title'
      |. Paragraph - Spans: 1
      |. . Text - 'bbb'
      |RootElement - Blocks: 2
      |. Title(Id(sub-title) + Styles(title)) - Spans: 1
      |. . Text - 'Sub Title'
      |. Paragraph - Spans: 1
      |. . Text - 'ccc'
      |""".stripMargin
    
    def input (source: String, docTypeMatcher: Path => DocumentType) = parseTreeStructure(source, docTypeMatcher)
  }
  
  it should "render a tree with a RenderResultProcessor writing to an output stream" ignore new GatheringTransformer {
//    val out = new ByteArrayOutputStream
//    (Transform from ReStructuredText to TestRenderResultProcessor fromTreeInput input(dirs) toStream out).execute
//    out.toString should be (expectedResult)
  }
  
  it should "render a tree with a RenderResultProcessor writing to a file" in new GatheringTransformer {
    val f = File.createTempFile("output", null)
    val transform = Transform.from(ReStructuredText).to(TestRenderResultProcessor)
    transform.fromTreeInput(input(dirs, transform.config.docTypeMatcher)).toFile(f).execute
    readFile(f) should be (expectedResult)
  }
  
  it should "render a tree with a RenderResultProcessor overriding the default renderer for specific element types" ignore new GatheringTransformer {
//    val modifiedResult = expectedResult.replaceAllLiterally(". Text", ". String")
//    val out = new ByteArrayOutputStream
//    (Transform from ReStructuredText to TestRenderResultProcessor rendering { 
//      out => { case Text(content,_) => out << "String - '" << content << "'" } 
//    } fromTreeInput input(dirs) toStream out).execute
//    out.toString should be (modifiedResult)
  }
  
  it should "render a tree with a RenderResultProcessor with a custom rewrite rule" ignore new GatheringTransformer {
//    val modifiedResult = expectedResult.replaceAllLiterally("Title'", "zzz'")
//    val out = new ByteArrayOutputStream
//    (Transform from ReStructuredText to TestRenderResultProcessor usingSpanRule { 
//      case Text(txt,_) => Replace(Text(txt.replaceAllLiterally("Title", "zzz"))) 
//    } fromTreeInput input(dirs) toStream out).execute
//    out.toString should be (modifiedResult)
  }
  
  it should "render a tree with a RenderResultProcessor with multiple custom rewrite rules" ignore new GatheringTransformer {
//    val modifiedResult = expectedResult.replaceAllLiterally("Title'", "zzz'").replaceAllLiterally("bbb", "xxx")
//    val out = new ByteArrayOutputStream
//    (Transform from ReStructuredText to TestRenderResultProcessor usingSpanRule { 
//      case Text(txt,_) => Replace(Text(txt.replaceAllLiterally("Title", "zzz"))) 
//    } usingSpanRule { 
//      case Text("bbb",_) => Replace(Text("xxx")) 
//    } fromTreeInput input(dirs) toStream out).execute
//    out.toString should be (modifiedResult)
  }
  
  it should "render a tree with a RenderResultProcessor with a custom rewrite rule that depends on the document cursor" ignore new GatheringTransformer {
//    val modifiedResult = expectedResult.replaceAllLiterally("Sub Title", "Sub docSub.rst")
//    val out = new ByteArrayOutputStream
//    (Transform from ReStructuredText to TestRenderResultProcessor creatingRule { cursor => RewriteRules.forSpans { 
//      case Text("Sub Title",_) => Replace(Text("Sub " + cursor.target.path.name))
//    }} fromTreeInput input(dirs) toStream out).execute
//    out.toString should be (modifiedResult)
  }
  
  trait FileSystemTest {
    import laika.ast.helper.OutputBuilder.readFile
    
    def resourcePath (path: String): String = getClass.getResource(path).getFile
    
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
    
    def readFilesFiltered (base: String) = {
      new File(base+"/doc1.txt").exists should be (false)
      new File(base+"/dir1").exists should be (false)
      readFile(base+"/doc2.txt") should be (renderedDoc(2))
      readFile(base+"/dir2/doc5.txt") should be (renderedDoc(5))
      readFile(base+"/dir2/doc6.txt") should be (renderedDoc(6))
    }
    
    def readFilesMerged (base: String) = {
      readFile(base+"/doc1.txt") should be (renderedDoc(1))
      readFile(base+"/doc2.txt") should be (renderedDoc(2))
      readFile(base+"/doc9.txt") should be (renderedDoc(9))
      readFile(base+"/dir1/doc3.txt") should be (renderedDoc(3))
      readFile(base+"/dir1/doc4.txt") should be (renderedDoc(4))
      readFile(base+"/dir1/doc7.txt") should be (renderedDoc(7))
      readFile(base+"/dir2/doc5.txt") should be (renderedDoc(5))
      readFile(base+"/dir2/doc6.txt") should be (renderedDoc(6))
      readFile(base+"/dir3/doc8.txt") should be (renderedDoc(8))
    }
  }

  ignore should "read from and write to directories" in {
    import laika.ast.helper.OutputBuilder.createTempDirectory
    new FileSystemTest {
      val sourceName = resourcePath("/trees/a/")
      val targetDir = createTempDirectory("renderToDir")
      transform.fromDirectory(sourceName).toDirectory(targetDir).execute
      readFiles(targetDir.getPath)
    }
  }

  ignore should "allow to specify custom exclude filter" in {
    import laika.ast.helper.OutputBuilder.createTempDirectory
    new FileSystemTest {
      val sourceName = resourcePath("/trees/a/")
      val targetDir = createTempDirectory("renderToDir")
      transform.fromDirectory(sourceName, {f:File => f.getName == "doc1.md" || f.getName == "dir1"}).toDirectory(targetDir).execute
      readFilesFiltered(targetDir.getPath)
    }
  }

  ignore should "read from two root directories" in {
    import laika.ast.helper.OutputBuilder.createTempDirectory
    new FileSystemTest {
      val source1 = new File(resourcePath("/trees/a/"))
      val source2 = new File(resourcePath("/trees/b/"))
      val targetDir = createTempDirectory("renderToDir")
      transform.fromDirectories(Seq(source1, source2)).toDirectory(targetDir).execute
      readFilesMerged(targetDir.getPath)
    }
  }

  it should "allow to use the same directory as input and output" ignore {
    
    // TODO - 0.12 - resurrect
    import laika.ast.helper.OutputBuilder.{createTempDirectory, readFile, writeFile}
    new FileSystemTest {
      val targetDir = createTempDirectory("renderToDir")
      val staticFile = new File(targetDir, "static.txt")
      val inputFile = new File(targetDir, "hello.md")
      writeFile(inputFile, "Hello")
      writeFile(staticFile, "Text")

      val result = """RootElement - Blocks: 1
                     |. Paragraph - Spans: 1
                     |. . Text - 'Hello'""".stripMargin

      transform.fromDirectory(targetDir).toDirectory(targetDir).execute

      readFile(inputFile) shouldBe "Hello"
      readFile(staticFile) shouldBe "Text"
      readFile(new File(targetDir, "hello.txt")) shouldBe result
    }
  }

  it should "not copy files from the output directory if it's nested inside the input directory" ignore {
    import laika.ast.helper.OutputBuilder.{createTempDirectory, readFile, writeFile}
    new FileSystemTest {
      val targetDir = createTempDirectory("renderToDir")
      val staticFile = new File(targetDir, "static.txt")
      val inputFile = new File(targetDir, "hello.md")
      val subdir = new File(targetDir, "sub")
      subdir.mkdir()
      val outputFile = new File(subdir, "hello.js")
      writeFile(inputFile, "Hello")
      writeFile(staticFile, "Text")
      writeFile(outputFile, "Output")

      val result = """RootElement - Blocks: 1
                     |. Paragraph - Spans: 1
                     |. . Text - 'Hello'""".stripMargin

      transform.fromDirectory(targetDir).toDirectory(subdir).execute

      readFile(inputFile) shouldBe "Hello"
      readFile(new File(subdir, "static.txt")) shouldBe "Text"
      readFile(new File(subdir, "hello.txt")) shouldBe result
      new File(subdir, "sub").exists shouldBe false
    }
  }
  

}
  