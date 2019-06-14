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
  
package laika.io

import java.io._

import cats.effect.{Async, IO}
import laika.api.Transformer
import laika.api.builder.OperationConfig
import laika.ast.DocumentType.{Ignored, Static}
import laika.ast.Path.Root
import laika.ast._
import laika.bundle.{BundleProvider, ExtensionBundle}
import laika.directive.Templates
import laika.format._
import laika.io.Parallel.ParallelTransformer
import laika.io.helper.OutputBuilder.{DocumentViews, RenderedDocumentView, RenderedTreeView, SubtreeViews, TreeContentView}
import laika.io.helper.{InputBuilder, OutputBuilder, RenderResult}
import laika.parse.Parser
import laika.parse.text.TextParsers
import org.scalatest.{Assertion, FlatSpec, Matchers}

class ParallelTransformerSpec extends FlatSpec 
                       with Matchers {
   

  private val transformer: ParallelTransformer[IO] = Parallel(Transformer.from(Markdown).to(AST)).build
  private def transformerWithBundle (bundle: ExtensionBundle): ParallelTransformer[IO] = 
    Parallel(Transformer.from(Markdown).to(AST).using(bundle)).build
  
  
  trait TreeTransformer extends InputBuilder {
    import laika.ast.{DocumentType, Path}

    val dirs: String
    
    def input (source: String, docTypeMatcher: Path => DocumentType): TreeInput = parseTreeStructure(source, docTypeMatcher)

    def transformTree: RenderedTreeView = transformWith()
    //def transformMultiMarkup: RenderedTree = transformWith(Transformer.from(Markdown or ReStructuredText to AST)
    
    def transformWithConfig (config: String): RenderedTreeView = transformWithBundle(BundleProvider.forConfigString(config))
    def transformWithDocTypeMatcher (matcher: PartialFunction[Path, DocumentType]): RenderedTreeView = transformWithBundle(BundleProvider.forDocTypeMatcher(matcher))
    def transformWithTemplates (parser: Parser[TemplateRoot]): RenderedTreeView = transformWithBundle(BundleProvider.forTemplateParser(parser))
    def transformWithDirective (directive: Templates.Directive): RenderedTreeView = transformWithBundle(BundleProvider.forTemplateDirective(directive))
    
    private def transformWith (transformer: ParallelTransformer[IO] = transformer): RenderedTreeView =
      OutputBuilder.RenderedTreeView.toTreeView(
        transformer
          .fromInput(IO.pure(input(dirs, transformer.config.docTypeMatcher)))
          .toOutput(IO.pure(StringTreeOutput))
          .transform
          .unsafeRunSync()
          .tree
      )

    private def transformWithBundle (bundle: ExtensionBundle): RenderedTreeView =
      transformWith(Parallel(Transformer.from(Markdown).to(AST).using(bundle)).build)
    
    def root (content: Seq[TreeContentView]): RenderedTreeView = RenderedTreeView(Root, content)
    
    val contents = Map(
      "name" -> "foo",
      "aa" -> "aa",
      "style" -> "13",
      "link" -> "[link](foo)",
      "directive" -> "{{document.content}} @:foo bar. bb",
      "templateConfigRef" -> "{{document.content}}{{config.value}}",
      "template1" -> "{{document.content}}",
      "template2" -> "({{document.content}})",
      "conf" -> "value: abc"
    )
    
    val simpleResult: String = """RootElement - Blocks: 1
      |. Paragraph - Spans: 1
      |. . Text - 'foo'""".stripMargin
      
    def docs (values: (Path, String)*): DocumentViews = DocumentViews(values map { case (path, content) => RenderedDocumentView(path, content) })

    def sorted (tree: RenderedTreeView): RenderedTreeView = tree.copy(content = tree.content map sortedContent)
        
    def sortedContent (content: TreeContentView): TreeContentView = content match {
      case DocumentViews(cnt) => DocumentViews(cnt.sortBy(_.path.name))
      case SubtreeViews(cnt) => SubtreeViews(cnt.sortBy(_.path.name) map sorted)
    }
    
    def trees (values: (Path, Seq[TreeContentView])*) = SubtreeViews(values map { case (path, content) => RenderedTreeView(path, content) })
  }


  "The Transform API" should "transform an empty tree" in {
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
  
  it should "transform a tree with a template document populated by a config file in the directory" in {
    new TreeTransformer {
      val dirs = """- default.template.txt:templateConfigRef
          |- directory.conf:conf
          |- main.md:aa""".stripMargin
      val result = """RootElement - Blocks: 1
          |. TemplateRoot - TemplateSpans: 2
          |. . EmbeddedRoot(0) - Blocks: 1
          |. . . Paragraph - Spans: 1
          |. . . . Text - 'aa'
          |. . TemplateString - 'abc'""".stripMargin
      transformTree should be (root(List(docs((Root / "main.txt", result)))))
    }
  }
  
  it should "transform a tree with a template document populated by a root config string" in {
    new TreeTransformer {
      val dirs =
        """- default.template.txt:templateConfigRef
          |- main.md:aa""".stripMargin
      val result = """RootElement - Blocks: 1
         |. TemplateRoot - TemplateSpans: 2
         |. . EmbeddedRoot(0) - Blocks: 1
         |. . . Paragraph - Spans: 1
         |. . . . Text - 'aa'
         |. . TemplateString - 'def'""".stripMargin
      transformWithConfig("value: def") should be (root(List(docs((Root / "main.txt", result)))))
    }
  }
  
  it should "transform a tree with a custom template engine" in {
    new TreeTransformer {
      val dirs = """- default.template.txt:template1
                   |- main1.md:aa
                   |- main2.md:aa""".stripMargin
      val parser: Parser[TemplateRoot] = OperationConfig.default.templateParser.get.map(root => root.copy(root.content :+ TemplateString("cc")))
      val result = """RootElement - Blocks: 1
        |. TemplateRoot - TemplateSpans: 2
        |. . EmbeddedRoot(0) - Blocks: 1
        |. . . Paragraph - Spans: 1
        |. . . . Text - 'aa'
        |. . TemplateString - 'cc'""".stripMargin
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
      val transform = Parallel(Transformer.from(Markdown).to(XSLFO).using(BundleProvider.forStyleSheetParser(parser))).build[IO]
      val renderResult = transform.fromInput(IO.pure(input(dirs, transformer.config.docTypeMatcher))).toOutput(IO.pure(StringTreeOutput)).transform.unsafeRunSync()
      OutputBuilder.RenderedTreeView.toTreeView(renderResult.tree) should be (root(List(docs(
        (Root / "doc1.fo", result)
      ))))
    }
  }
  
  it should "transform a tree with a template directive" in {
    import Templates.dsl._

    val directive = Templates.create("foo") {
      attribute(Default) map { TemplateString(_) }
    }
    new TreeTransformer {
      val dirs = """- default.template.txt:directive
        |- aa.md:aa""".stripMargin
      val result = """RootElement - Blocks: 1
        |. TemplateRoot - TemplateSpans: 4
        |. . EmbeddedRoot(0) - Blocks: 1
        |. . . Paragraph - Spans: 1
        |. . . . Text - 'aa'
        |. . TemplateString - ' '
        |. . TemplateString - 'bar'
        |. . TemplateString - ' bb'""".stripMargin
      transformWithDirective(directive) should be (root(List(docs(
        (Root / "aa.txt", result)
      ))))
    }
  }

  it should "transform a tree with a static document" ignore {
    new TreeTransformer {
      val dirs = """- omg.js:name"""
      transformTree should be (root(List(docs((Root / "omg.js", "foo")))))
    }
  }
  
  it should "transform a tree with all available file types" ignore {
//    new TreeTransformer {
//      val dirs = """- doc1.md:link
//        |- doc2.rst:link
//        |- default.template.txt:template1
//        |+ dir1
//        |  - default.template.txt:template2
//        |  - doc3.md:name
//        |  - doc4.md:name
//        |+ dir2
//        |  - omg.js:name
//        |  - doc5.md:name
//        |  - doc6.md:name""".stripMargin
//      val withTemplate1 = """RootElement - Blocks: 1
//        |. Paragraph - Spans: 1
//        |. . Text - 'foo'""".stripMargin  
//      val withTemplate2 = """RootElement - Blocks: 1
//        |. TemplateRoot - TemplateSpans: 3
//        |. . TemplateString - '('
//        |. . EmbeddedRoot(0) - Blocks: 1
//        |. . . Paragraph - Spans: 1
//        |. . . . Text - 'foo'
//        |. . TemplateString - ')'""".stripMargin  
//      val markdown = """RootElement - Blocks: 1
//        |. Paragraph - Spans: 1
//        |. . ExternalLink(foo,None) - Spans: 1
//        |. . . Text - 'link'""".stripMargin
//      val rst = """RootElement - Blocks: 1
//        |. Paragraph - Spans: 1
//        |. . Text - '[link](foo)'""".stripMargin
//      transformMultiMarkup should be (root(List(
//        docs(
//          (Root / "doc1.txt", markdown),
//          (Root / "doc2.txt", rst)
//        ),
//        trees(
//          (Root / "dir1", List(docs(
//            (Root / "dir1" / "doc3.txt", withTemplate2),
//            (Root / "dir1" / "doc4.txt", withTemplate2)  
//          ))),
//          (Root / "dir2", List(docs(
//            (Root / "dir2" / "doc5.txt", withTemplate1),
//            (Root / "dir2" / "doc6.txt", withTemplate1),  
//            (Root / "dir2" / "omg.js", "foo")  
//          )))
//        )
//      )))
//    }
  }
  
  trait GatheringTransformer extends InputBuilder {

    val srcRoot: String = """Title
      |=====
      |
      |bbb""".stripMargin
    
    val srcSub: String = """Sub Title
      |=========
      |
      |ccc""".stripMargin
      
    val contents: Map[String, String] = Map(
      "docRoot" -> srcRoot,
      "docSub" -> srcSub
    )
    
    val dirs: String = """- docRoot.rst:docRoot
        |+ dir
        |  - docSub.rst:docSub""".stripMargin
        
    val expectedResult: String = """RootElement - Blocks: 2
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
    
    def input (source: String, docTypeMatcher: Path => DocumentType): TreeInput = parseTreeStructure(source, docTypeMatcher)
  }
  
  it should "render a tree with a RenderResultProcessor writing to an output stream" in new GatheringTransformer {
    val out = new ByteArrayOutputStream
    val transform = Transformer.from(ReStructuredText).to(TestRenderResultProcessor).build
    Parallel(transform)
      .build[IO]
      .fromInput(IO.pure(input(dirs, transform.markupParser.config.docTypeMatcher)))
      .toStream(IO.pure(out))
      .transform
      .unsafeRunSync()
    out.toString should be (expectedResult)
  }
  
  it should "render a tree with a RenderResultProcessor writing to a file" in new GatheringTransformer {
    val f = File.createTempFile("output", null)
    val transform = Transformer.from(ReStructuredText).to(TestRenderResultProcessor).build
    
    Parallel(transform)
      .build[IO]
      .fromInput(IO.pure(input(dirs, transform.markupParser.config.docTypeMatcher)))
      .toFile(f)
      .transform
      .unsafeRunSync()
    OutputBuilder.readFile(f) should be (expectedResult)
  }
  
// TODO - 0.12 - move these tests to the laika-core project  
//  it should "render a tree with a RenderResultProcessor overriding the default renderer for specific element types" in new GatheringTransformer {
//    val modifiedResult = expectedResult.replaceAllLiterally(". Text", ". String")
//    val out = new ByteArrayOutputStream
//    (Transformer.from(ReStructuredText).to(TestRenderResultProcessor rendering { 
//      out => { case Text(content,_) => out << "String - '" << content << "'" } 
//    } fromTreeInput input(dirs) toStream out).execute
//    out.toString should be (modifiedResult)
//  }
//  
//  it should "render a tree with a RenderResultProcessor with a custom rewrite rule" in new GatheringTransformer {
//    val modifiedResult = expectedResult.replaceAllLiterally("Title'", "zzz'")
//    val out = new ByteArrayOutputStream
//    (Transformer.from(ReStructuredText).to(TestRenderResultProcessor usingSpanRule { 
//      case Text(txt,_) => Replace(Text(txt.replaceAllLiterally("Title", "zzz"))) 
//    } fromTreeInput input(dirs) toStream out).execute
//    out.toString should be (modifiedResult)
//  }
//  
//  it should "render a tree with a RenderResultProcessor with multiple custom rewrite rules" in new GatheringTransformer {
//    val modifiedResult = expectedResult.replaceAllLiterally("Title'", "zzz'").replaceAllLiterally("bbb", "xxx")
//    val out = new ByteArrayOutputStream
//    (Transformer.from(ReStructuredText).to(TestRenderResultProcessor usingSpanRule { 
//      case Text(txt,_) => Replace(Text(txt.replaceAllLiterally("Title", "zzz"))) 
//    } usingSpanRule { 
//      case Text("bbb",_) => Replace(Text("xxx")) 
//    } fromTreeInput input(dirs) toStream out).execute
//    out.toString should be (modifiedResult)
//  }
//  
//  it should "render a tree with a RenderResultProcessor with a custom rewrite rule that depends on the document cursor" in new GatheringTransformer {
//    val modifiedResult = expectedResult.replaceAllLiterally("Sub Title", "Sub docSub.rst")
//    val out = new ByteArrayOutputStream
//    (Transformer.from(ReStructuredText).to(TestRenderResultProcessor creatingRule { cursor => RewriteRules.forSpans { 
//      case Text("Sub Title",_) => Replace(Text("Sub " + cursor.target.path.name))
//    }} fromTreeInput input(dirs) toStream out).execute
//    out.toString should be (modifiedResult)
// }
  
  trait FileSystemTest {
    
    import OutputBuilder._
    
    def resourcePath (path: String): String = getClass.getResource(path).getFile
    
    def renderedDynDoc (num: Int): String = """RootElement - Blocks: 1
      |. TemplateRoot - Spans: 1
      |. . TemplateString - 'Doc""".stripMargin + num + "'"
      
    def renderedDoc (num: Int): String = """RootElement - Blocks: 1
      |. Paragraph - Spans: 1
      |. . Text - 'Doc""".stripMargin + num + "'"
      
    def readFiles (base: String): Assertion = {
      readFile(base+"/doc1.txt") should be (renderedDoc(1))
      readFile(base+"/doc2.txt") should be (renderedDoc(2))
      readFile(base+"/dir1/doc3.txt") should be (renderedDoc(3))
      readFile(base+"/dir1/doc4.txt") should be (renderedDoc(4))
      readFile(base+"/dir2/doc5.txt") should be (renderedDoc(5))
      readFile(base+"/dir2/doc6.txt") should be (renderedDoc(6))
    }
    
    def readFilesFiltered (base: String): Assertion = {
      new File(base+"/doc1.txt").exists should be (false)
      new File(base+"/dir1").exists should be (false)
      readFile(base+"/doc2.txt") should be (renderedDoc(2))
      readFile(base+"/dir2/doc5.txt") should be (renderedDoc(5))
      readFile(base+"/dir2/doc6.txt") should be (renderedDoc(6))
    }
    
    def readFilesMerged (base: String): Assertion = {
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

  it should "read from and write to directories" in {
    new FileSystemTest {
      val sourceName = resourcePath("/trees/a/")
      val targetDir = OutputBuilder.createTempDirectory("renderToDir")
      transformer.fromDirectory(sourceName).toDirectory(targetDir).transform.unsafeRunSync()
      readFiles(targetDir.getPath)
    }
  }

  it should "transform a directory with a custom document type matcher" in {
    new FileSystemTest {
      val sourceName = resourcePath("/trees/a/")
      val targetDir = OutputBuilder.createTempDirectory("renderToDir")
      val transform = transformerWithBundle(BundleProvider.forDocTypeMatcher{ case Root / "doc1.md" => Ignored; case Root / "dir1" / _ => Ignored })
      transform.fromDirectory(sourceName).toDirectory(targetDir).transform.unsafeRunSync()
      readFilesFiltered(targetDir.getPath)
    }
  }

  it should "allow to specify custom exclude filter" in {
    new FileSystemTest {
      val sourceName = resourcePath("/trees/a/")
      val targetDir = OutputBuilder.createTempDirectory("renderToDir")
      transformer.fromDirectory(sourceName, {f:File => f.getName == "doc1.md" || f.getName == "dir1"}).toDirectory(targetDir).transform.unsafeRunSync()
      readFilesFiltered(targetDir.getPath)
    }
  }

  it should "read from two root directories" in {
    new FileSystemTest {
      val source1 = new File(resourcePath("/trees/a/"))
      val source2 = new File(resourcePath("/trees/b/"))
      val targetDir = OutputBuilder.createTempDirectory("renderToDir")
      transformer.fromDirectories(Seq(source1, source2)).toDirectory(targetDir).transform.unsafeRunSync()
      readFilesMerged(targetDir.getPath)
    }
  }

  it should "allow to use the same directory as input and output" ignore {
    import OutputBuilder._
    
    // TODO - 0.12 - resurrect
    new FileSystemTest {
      val targetDir = OutputBuilder.createTempDirectory("renderToDir")
      val staticFile = new File(targetDir, "static.txt")
      val inputFile = new File(targetDir, "hello.md")
      writeFile(inputFile, "Hello")
      writeFile(staticFile, "Text")

      val result = """RootElement - Blocks: 1
                     |. Paragraph - Spans: 1
                     |. . Text - 'Hello'""".stripMargin

      transformer.fromDirectory(targetDir).toDirectory(targetDir).transform.unsafeRunSync()

      readFile(inputFile) shouldBe "Hello"
      readFile(staticFile) shouldBe "Text"
      readFile(new File(targetDir, "hello.txt")) shouldBe result
    }
  }

  it should "not copy files from the output directory if it's nested inside the input directory" ignore {
    new FileSystemTest {
      import OutputBuilder._
      
      val targetDir = OutputBuilder.createTempDirectory("renderToDir")
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

      transformer.fromDirectory(targetDir).toDirectory(subdir).transform.unsafeRunSync()

      readFile(inputFile) shouldBe "Hello"
      readFile(new File(subdir, "static.txt")) shouldBe "Text"
      readFile(new File(subdir, "hello.txt")) shouldBe result
      new File(subdir, "sub").exists shouldBe false
    }
  }
  

}
  