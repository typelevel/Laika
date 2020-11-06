/*
 * Copyright 2012-2020 the original author or authors.
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

import cats.effect.{IO, Resource}
import laika.api.builder.OperationConfig
import laika.api.{MarkupParser, Transformer}
import laika.ast.DocumentType.Ignored
import laika.ast.Path.Root
import laika.ast._
import laika.bundle.{BundleProvider, ExtensionBundle}
import laika.config.Config
import laika.directive.Templates
import laika.format._
import laika.io.api.{BinaryTreeTransformer, TreeTransformer}
import laika.io.descriptor.TransformerDescriptor
import laika.io.helper.{InputBuilder, RenderResult, RenderedTreeAssertions, TestThemeBuilder}
import laika.io.implicits._
import laika.io.model.{InputTree, RenderContent, RenderedDocument, RenderedTree, RenderedTreeRoot, StringTreeOutput}
import laika.parse.Parser
import laika.parse.code.SyntaxHighlighting
import laika.parse.text.TextParsers
import laika.render.fo.TestTheme
import laika.rewrite.DefaultTemplatePath
import laika.rewrite.link.SlugBuilder
import laika.theme.ThemeProvider

class TreeTransformerSpec extends IOWordSpec with FileIO with RenderedTreeAssertions {

  
  private val transformer: Resource[IO, TreeTransformer[IO]] = Transformer.from(Markdown).to(AST).io(blocker).parallel[IO].build
  private def transformerWithBundle (bundle: ExtensionBundle): Resource[IO, TreeTransformer[IO]] = 
    Transformer.from(Markdown).to(AST).using(bundle).io(blocker).parallel[IO].build
  
  
  trait TreeTransformerSetup extends InputBuilder {
    import laika.ast.{DocumentType, Path}
    
    val astDefaultTemplatePath: Path = DefaultTemplatePath.forSuffix("txt")

    def inputs: Seq[(Path, String)]

    def input (in: Seq[(Path, String)], docTypeMatcher: Path => DocumentType): IO[InputTree[IO]] = build(in, docTypeMatcher)

    def transformTree: IO[RenderedTreeRoot[IO]] = transformWith()
    
    def transformWithConfig (config: String): IO[RenderedTreeRoot[IO]] = transformWithBundle(BundleProvider.forConfigString(config))
    def transformWithDocTypeMatcher (matcher: PartialFunction[Path, DocumentType]): IO[RenderedTreeRoot[IO]] = transformWithBundle(BundleProvider.forDocTypeMatcher(matcher))
    def transformWithTemplates (parser: Parser[TemplateRoot]): IO[RenderedTreeRoot[IO]] = transformWithBundle(BundleProvider.forTemplateParser(parser))
    def transformWithSlugBuilder (f: String => String): IO[RenderedTreeRoot[IO]] = transformWithBundle(BundleProvider.forSlugBuilder(f))
    def transformWithDirective (directive: Templates.Directive): IO[RenderedTreeRoot[IO]] = transformWithBundle(BundleProvider.forTemplateDirective(directive))
    def transformWithDocumentMapper (f: Document => Document): IO[RenderedTreeRoot[IO]] = 
      transformWith(Transformer.from(Markdown).to(AST).io(blocker).parallel[IO].mapDocuments(f).build)
    
    def describe: IO[TransformerDescriptor] = Transformer
      .from(Markdown)
      .to(AST)
      .using(SyntaxHighlighting)
      .io(blocker)
      .parallel[IO]
      .withAlternativeParser(MarkupParser.of(ReStructuredText))
      .build
      .use (_
        .fromInput(build(inputs))
        .toOutput(StringTreeOutput)
        .describe
      )
    
    protected def transformWith (transformer: Resource[IO, TreeTransformer[IO]] = transformer): IO[RenderedTreeRoot[IO]] =
      transformer.use (_
        .fromInput(build(inputs))
        .toOutput(StringTreeOutput)
        .transform
      )

    private def transformWithBundle (bundle: ExtensionBundle): IO[RenderedTreeRoot[IO]] =
      transformWith(Transformer.from(Markdown).to(AST).using(bundle).io(blocker).parallel[IO].build)

    def transformMixedMarkup: IO[RenderedTreeRoot[IO]] =
      transformWith(Transformer.from(Markdown).to(AST).io(blocker).parallel[IO].withAlternativeParser(MarkupParser.of(ReStructuredText)).build)
    
    object Contents {
      val name = "foo"
      def forTargetFormats (formats: String*): String =
        s"""{%
          |  laika.targetFormats = [${formats.mkString(",")}]
          |%}
          |
          |foo
        """.stripMargin
      val aa = "aa"
      val style = "13"
      val link = "[link](/foo)"
      val directive = "${cursor.currentDocument.content} @:foo(bar) bb"
      val templateConfigRef = "${cursor.currentDocument.content}${value}"
      val template1 = "${cursor.currentDocument.content}"
      val template2 = "(${cursor.currentDocument.content})"
      val conf = "value: abc"
    }
    
    val simpleResult: String = """RootElement - Blocks: 1
      |. Paragraph - Spans: 1
      |. . Text - 'foo'""".stripMargin

    val mappedResult: String = """RootElement - Blocks: 1
      |. Paragraph - Spans: 1
      |. . Text - 'foo-bar'""".stripMargin

    def renderedRoot(content: Seq[RenderContent],
                     titleDocument: Option[RenderedDocument] = None,
                     coverDocument: Option[RenderedDocument] = None,
                     staticDocuments: Seq[Path] = Nil): RenderedTreeRoot[IO] = RenderedTreeRoot(
      RenderedTree(Root, None, content, titleDocument),
      TemplateRoot.fallback,
      Config.empty,
      coverDocument = coverDocument,
      staticDocuments = staticDocuments.map(ByteInput.apply(_))
    )

    def renderedTree(path: Path, content: Seq[RenderContent]): RenderedTree = RenderedTree(path, None, content)

    def renderedDoc(path: Path, expected: String): RenderedDocument = RenderedDocument(path, None, Nil, expected, Config.empty)
    
    def docs (values: (Path, String)*): Seq[RenderedDocument] = values.map { case (path, content) => renderedDoc(path, content) }

    def trees (values: (Path, Seq[RenderContent])*) = values.map { case (path, content) => renderedTree(path, content) }

  }


  "The parallel transformer" should {

    "transform an empty tree" in new TreeTransformerSetup {
      val inputs = Nil
      transformTree.assertEquals(renderedRoot(Nil, staticDocuments = TestTheme.staticASTPaths))
    }

    "transform a tree with a single document" in new TreeTransformerSetup {
      val inputs = Seq(
        Root / "name.md" -> Contents.name
      )
      transformTree.assertEquals(renderedRoot(docs((Root / "name.txt", simpleResult)), staticDocuments = TestTheme.staticASTPaths))
    }

    "transform a tree with a cover, title document and one content document" in new TreeTransformerSetup {
      val inputs = Seq(
        Root / "name.md" -> Contents.name,
        Root / "README.md" -> Contents.name,
        Root / "cover.md" -> Contents.name
      )
      transformTree.assertEquals(renderedRoot(
        docs((Root / "name.txt", simpleResult)),
        Some(renderedDoc(Root / "index.txt", simpleResult)),
        Some(renderedDoc(Root / "cover.txt", simpleResult)),
        staticDocuments = TestTheme.staticASTPaths
      ))
    }

    "transform a tree with a cover, title document and two content documents with a document mapper" in new TreeTransformerSetup {
      val inputs = Seq(
        Root / "rootDoc.md" -> Contents.name,
        Root / "sub" / "subDoc.md" -> Contents.name,
        Root / "README.md" -> Contents.name,
        Root / "cover.md" -> Contents.name
      )
      transformWithDocumentMapper(doc => doc.copy(content = doc.content.withContent(Seq(Paragraph("foo-bar")))))
        .assertEquals(renderedRoot(
          docs((Root / "rootDoc.txt", mappedResult)) ++ 
            trees((Root / "sub", docs((Root / "sub" / "subDoc.txt", mappedResult)))),
          Some(renderedDoc(Root / "index.txt", mappedResult)),
          Some(renderedDoc(Root / "cover.txt", mappedResult)),
          TestTheme.staticASTPaths
        ))
    }
    
    trait TreeProcessorSetup extends TreeTransformerSetup {
      val inputs = Seq(
        Root / "rootDoc.md" -> Contents.name,
        Root / "sub" / "subDoc.md" -> Contents.name,
        Root / "README.md" -> Contents.name,
        Root / "cover.md" -> Contents.name
      )
      def theme: ThemeProvider
      def expectedDocResult: String
      
      val mapperFunction: Document => Document = doc => doc.copy(content = doc.content.withContent(Seq(Paragraph("foo-bar"))))
      def transformWithProcessor: IO[RenderedTreeRoot[IO]] =
        transformWith(Transformer.from(Markdown).to(AST).io(blocker).parallel[IO].withTheme(theme).build)

      transformWithProcessor
        .assertEquals(renderedRoot(
          docs((Root / "rootDoc.txt", expectedDocResult)) ++
            trees((Root / "sub", docs((Root / "sub" / "subDoc.txt", expectedDocResult)))),
          Some(renderedDoc(Root / "index.txt", expectedDocResult)),
          Some(renderedDoc(Root / "cover.txt", expectedDocResult))
        ))
    }

    "transform a tree with a document mapper from a theme" in new TreeProcessorSetup {
      def expectedDocResult = mappedResult
      def theme: ThemeProvider = TestThemeBuilder.forDocumentMapper(mapperFunction)
    }

    "transform a tree with a document mapper from a theme specific to the output format" in new TreeProcessorSetup {
      def expectedDocResult = mappedResult
      def theme: ThemeProvider = TestThemeBuilder.forDocumentMapper(AST)(mapperFunction)
    }

    "transform a tree ignoring the document mapper from a theme if the format does not match" in new TreeProcessorSetup {
      def expectedDocResult = simpleResult
      def theme: ThemeProvider = TestThemeBuilder.forDocumentMapper(HTML)(mapperFunction)
    }

    "transform a tree with a template document populated by a config file in the directory" in new TreeTransformerSetup {
      val inputs = Seq(
        astDefaultTemplatePath -> Contents.templateConfigRef,
        Root / "directory.conf" -> Contents.conf,
        Root / "main.md" -> Contents.aa
      )
      val result =
        """RootElement - Blocks: 1
          |. TemplateRoot - TemplateSpans: 2
          |. . EmbeddedRoot(0) - Blocks: 1
          |. . . Paragraph - Spans: 1
          |. . . . Text - 'aa'
          |. . TemplateString - 'abc'""".stripMargin
      transformTree.assertEquals(renderedRoot(docs((Root / "main.txt", result)), staticDocuments = TestTheme.staticASTPaths))
    }

    "transform a tree with a template document populated by a root config string" in new TreeTransformerSetup {
      val inputs = Seq(
        astDefaultTemplatePath -> Contents.templateConfigRef,
        Root / "main.md" -> Contents.aa
      )
      val result =
        """RootElement - Blocks: 1
          |. TemplateRoot - TemplateSpans: 2
          |. . EmbeddedRoot(0) - Blocks: 1
          |. . . Paragraph - Spans: 1
          |. . . . Text - 'aa'
          |. . TemplateString - 'def'""".stripMargin
      transformWithConfig("value: def").assertEquals(renderedRoot(docs((Root / "main.txt", result)), staticDocuments = TestTheme.staticASTPaths))
    }

    "transform a tree with a custom template engine" in new TreeTransformerSetup {
      val inputs = Seq(
        astDefaultTemplatePath -> Contents.template1,
        Root / "main1.md" -> Contents.aa,
        Root / "main2.md" -> Contents.aa
      )
      val parser: Parser[TemplateRoot] = OperationConfig.default.templateParser.get.map(root => root.copy(root.content :+ TemplateString("cc")))
      val result =
        """RootElement - Blocks: 1
          |. TemplateRoot - TemplateSpans: 2
          |. . EmbeddedRoot(0) - Blocks: 1
          |. . . Paragraph - Spans: 1
          |. . . . Text - 'aa'
          |. . TemplateString - 'cc'""".stripMargin
      transformWithTemplates(parser).assertEquals(renderedRoot(docs(
        (Root / "main1.txt", result),
        (Root / "main2.txt", result)
      ), staticDocuments = TestTheme.staticASTPaths))
    }

    "transform a tree with a custom style sheet engine" in new TreeTransformerSetup {
      // the AST renderer does not use stylesheets, so we must use XSL-FO here
      def styleDecl (fontSize: String) =
        StyleDeclaration(StylePredicate.ElementType("Paragraph"), "font-size" -> s"${fontSize}pt")

      val parser: Parser[Set[StyleDeclaration]] = TextParsers.anyChars.map { n => Set(styleDecl(n)) }

      val inputs = Nil

      val result = RenderResult.fo.withHeliumTemplate("""<fo:block font-size="13pt">foo</fo:block>""")
      val transformer = Transformer
        .from(Markdown)
        .to(XSLFO)
        .using(BundleProvider.forStyleSheetParser(parser))
        .io(blocker)
        .parallel[IO]
        .withTheme(TestTheme.heliumTestProps.build)
        .build

      val input = InputTree[IO]
        .addString(Contents.name, Root / "doc1.md")
        .addString(Contents.style, Root / "styles.fo.css")
      
      val renderResult = transformer
        .use (_
          .fromInput(input)
          .toOutput(StringTreeOutput)
          .transform
        )
      renderResult.assertEquals(renderedRoot(docs(
        (Root / "doc1.fo", result)
      ), staticDocuments = TestTheme.staticASTPaths))
    }

    "transform a tree with a template directive" in new TreeTransformerSetup {

      import Templates.dsl._

      val directive = Templates.create("foo") {
        attribute(0).as[String] map {
          TemplateString(_)
        }
      }

      val inputs = Seq(
        astDefaultTemplatePath -> Contents.directive,
        Root / "aa.md" -> Contents.aa
      )
      val result =
        """RootElement - Blocks: 1
          |. TemplateRoot - TemplateSpans: 4
          |. . EmbeddedRoot(0) - Blocks: 1
          |. . . Paragraph - Spans: 1
          |. . . . Text - 'aa'
          |. . TemplateString - ' '
          |. . TemplateString - 'bar'
          |. . TemplateString - ' bb'""".stripMargin
      transformWithDirective(directive).assertEquals(renderedRoot(docs(
        (Root / "aa.txt", result)
      ), staticDocuments = TestTheme.staticASTPaths))
    }

    "transform a tree with a static document" in new TreeTransformerSetup {
      val inputs = Seq(
        Root / "omg.txt" -> Contents.name
      )
      transformTree.assertEquals(renderedRoot(Nil, staticDocuments = Seq(Root / "omg.txt") ++ TestTheme.staticASTPaths))
    }
    
    trait DocWithSection extends TreeTransformerSetup {
      val targetSrc: String =
        """
          |Doc Title
          |=========
          |
          |Section Title
          |-------------
        """.stripMargin
      
      def titleSlug: String
      def sectionSlug: String
      
      val title = SpanSequence("Doc Title")

      def refSrc: String =
        s"""
          |This is a [cross ref](../baz.md#$sectionSlug)
        """.stripMargin

      def targetRes: String =
        s"""RootElement - Blocks: 2
          |. Title(Id($titleSlug) + Styles(title)) - Spans: 1
          |. . Text - 'Doc Title'
          |. Section
          |. . Header(2,Id($sectionSlug) + Styles(section)) - Spans: 1
          |. . . Text - 'Section Title'
          |. . Content - Blocks: 0""".stripMargin

      def refRes: String =
        s"""RootElement - Blocks: 1
          |. Paragraph - Spans: 2
          |. . Text - 'This is a '
          |. . SpanLink(ResolvedInternalTarget(/baz.md#$sectionSlug,../baz.md#$sectionSlug,All),None) - Spans: 1
          |. . . Text - 'cross ref'""".stripMargin

      def inputs: Seq[(Path, String)] = Seq(
        Root / "baz.md" -> targetSrc,
        Root / "foo" / "bar.md" -> refSrc
      )

      def assertTree(f: IO[RenderedTreeRoot[IO]]): Unit = f.assertEquals(renderedRoot(
        docs((Root / "baz.txt", targetRes)).map(_.copy(
          title = Some(title), 
          sections = Seq(SectionInfo(sectionSlug, SpanSequence("Section Title"), Nil))
        )) ++
        trees((Root / "foo", docs((Root / "foo" / "bar.txt", refRes)))),
        staticDocuments = TestTheme.staticASTPaths))
    }
    
    "transform a tree with an internal reference using the default slug builder" in new DocWithSection {
      val titleSlug = "doc-title"
      val sectionSlug = "section-title"
      assertTree(transformTree)
    }

    "transform a tree with an internal reference using a custom slug builder" in new DocWithSection {
      val titleSlug = "doc-title-slug"
      val sectionSlug = "section-title-slug"
      assertTree(transformWithSlugBuilder(s => SlugBuilder.default(s) + "-slug"))
    }

    "transform a tree with all available file types and multiple markup formats" in new TreeTransformerSetup {
      val inputs = Seq(
        Root / "doc1.md" -> Contents.link,
        Root / "doc2.rst" -> Contents.link,
        astDefaultTemplatePath -> Contents.template1,
        Root / "dir1" / astDefaultTemplatePath.relative -> Contents.template2,
        Root / "dir1" / "doc3.md" -> Contents.name,
        Root / "dir1" / "doc4.md" -> Contents.name,
        Root / "dir2" / "omg.txt" -> Contents.name,
        Root / "dir2" / "doc5.md" -> Contents.name,
        Root / "dir2" / "doc6.md" -> Contents.name,
      )

      val withTemplate1 =
        """RootElement - Blocks: 1
          |. Paragraph - Spans: 1
          |. . Text - 'foo'""".stripMargin
      val withTemplate2 =
        """RootElement - Blocks: 1
          |. TemplateRoot - TemplateSpans: 3
          |. . TemplateString - '('
          |. . EmbeddedRoot(0) - Blocks: 1
          |. . . Paragraph - Spans: 1
          |. . . . Text - 'foo'
          |. . TemplateString - ')'""".stripMargin
      val markdown =
        """RootElement - Blocks: 1
          |. Paragraph - Spans: 1
          |. . SpanLink(ExternalTarget(/foo),None) - Spans: 1
          |. . . Text - 'link'""".stripMargin
      val rst =
        """RootElement - Blocks: 1
          |. Paragraph - Spans: 1
          |. . Text - '[link](/foo)'""".stripMargin
          
      transformMixedMarkup.assertEquals(renderedRoot(
        docs(
          (Root / "doc1.txt", markdown),
          (Root / "doc2.txt", rst)
        ) ++
        trees(
          (Root / "dir1", docs(
            (Root / "dir1" / "doc3.txt", withTemplate2),
            (Root / "dir1" / "doc4.txt", withTemplate2)
          )),
          (Root / "dir2", docs(
            (Root / "dir2" / "doc5.txt", withTemplate1),
            (Root / "dir2" / "doc6.txt", withTemplate1),
          ))
        ), 
        staticDocuments = Seq(Root / "dir2" / "omg.txt") ++ TestTheme.staticASTPaths
      ))
    }

    "transform a tree with while filtering documents based on their targetFormats setting" in new TreeTransformerSetup {
      val inputs = Seq(
        Root / "doc1.md" -> Contents.name,
        Root / "doc2.md" -> Contents.forTargetFormats(),
        Root / "dir1" / "doc3.md" -> Contents.forTargetFormats("html", "ast"),
        Root / "dir1" / "doc4.md" -> Contents.forTargetFormats("epub", "pdf"),
        Root / "dir2" / "doc5.md" -> Contents.name,
        Root / "dir2" / "doc6.md" -> Contents.name,
      )

      val result =
        """RootElement - Blocks: 1
          |. Paragraph - Spans: 1
          |. . Text - 'foo'""".stripMargin
      transformTree.assertEquals(renderedRoot(
        docs(
          (Root / "doc1.txt", result)
        ) ++
        trees(
          (Root / "dir1", docs(
            (Root / "dir1" / "doc3.txt", result)
          )),
          (Root / "dir2", docs(
            (Root / "dir2" / "doc5.txt", result),
            (Root / "dir2" / "doc6.txt", result),
          ))
        ),
        staticDocuments = TestTheme.staticASTPaths
      ))
    }

    "describe a tree with all available file types and multiple markup formats" in new TreeTransformerSetup {
      val inputs = Seq(
        Root / "doc1.md" -> Contents.link,
        Root / "doc2.rst" -> Contents.link,
        astDefaultTemplatePath -> Contents.template1,
        Root / "dir1" / astDefaultTemplatePath.relative -> Contents.template2,
        Root / "dir1" / "doc3.md" -> Contents.name,
        Root / "dir1" / "doc4.md" -> Contents.name,
        Root / "dir2" / "omg.js" -> Contents.name,
        Root / "dir2" / "doc5.md" -> Contents.name,
        Root / "dir2" / "doc6.md" -> Contents.name,
      )
      val expected = """Parser(s):
                       |  Markdown
                       |  reStructuredText
                       |Renderer:
                       |  AST
                       |Extension Bundles:
                       |  Laika's Default Extensions (supplied by library)
                       |  Laika's directive support (supplied by library)
                       |  Laika's built-in directives (supplied by library)
                       |  Default Syntax Highlighters for Code (supplied by library)
                       |  Document Type Matcher for Markdown (supplied by parser)
                       |  Default extensions for reStructuredText (supplied by parser)
                       |  Support for user-defined reStructuredText directives (supplied by parser)
                       |  Standard directives for reStructuredText (supplied by parser)
                       |  Document Type Matcher for reStructuredText (supplied by parser)
                       |  Directives for theme 'Helium' (supplied by theme)
                       |  Extensions for theme 'Helium' (supplied by theme)
                       |Settings:
                       |  Strict Mode: false
                       |  Accept Raw Content: false
                       |  Render Formatted: true
                       |Sources:
                       |  Markup File(s)
                       |    In-memory string or stream
                       |  Template(s)
                       |    In-memory string or stream
                       |  Configuration Files(s)
                       |    -
                       |  CSS for PDF
                       |    -
                       |  Copied File(s)
                       |    In-memory bytes or stream
                       |  Root Directories
                       |    -
                       |Target:
                       |  In-memory strings or streams""".stripMargin
      describe.map(_.formatted).assertEquals(expected)
    }

    trait TwoPhaseTransformer extends InputBuilder {

      val transformer: Resource[IO, BinaryTreeTransformer[IO]] = Transformer
        .from(ReStructuredText)
        .to(TestRenderResultProcessor)
        .io(blocker)
        .parallel[IO]
        .build
      
      val srcRoot: String =
        """Title
          |=====
          |
          |bbb""".stripMargin

      val srcSub: String =
        """Sub Title
          |=========
          |
          |ccc""".stripMargin

      val inputs: Seq[(Path, String)] = Seq(
        Root / "docRoot.rst" -> srcRoot,
        Root / "dir" / "docSub.rst" -> srcSub
      )

      val expectedResult: String =
        """RootElement - Blocks: 2
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
    }

    "render a tree with a RenderResultProcessor writing to an output stream" in new TwoPhaseTransformer {
      
      def transformTo(out: IO[OutputStream]): IO[Unit] = transformer.use { t =>
        t.fromInput(build(inputs)).toStream(out).transform
      }

      withByteArrayTextOutput { out =>
        transformTo(IO.pure(out))
      }.assertEquals(expectedResult)
    }

    // TODO - reactivate encoding tests
    //
    //    "transform from a java.io.InputStream to a java.io.OutputStream, specifying the encoding explicitly" in {
    //      val inStream = IO(new ByteArrayInputStream(input.getBytes("ISO-8859-1")))
    //      val codec = Codec.ISO8859
    //      withByteArrayTextOutput("ISO-8859-1") { out =>
    //        transformer.fromStream(inStream)(codec).toStream(IO.pure(out))(codec).transform.void
    //      }.assertEquals(output)
    //    }
    //
    //    "transform from a java.io.InputStream to a java.io.OutputStream, specifying the encoding implicitly" in {
    //      val inStream = IO(new ByteArrayInputStream(input.getBytes("ISO-8859-1")))
    //      implicit val codec: Codec = Codec.ISO8859
    //      withByteArrayTextOutput("ISO-8859-1") { out =>
    //        transformer.fromStream(inStream).toStream(IO.pure(out)).transform.void
    //      }.assertEquals(output)
    //    }

    "render a tree with a RenderResultProcessor writing to a file" in new TwoPhaseTransformer {
      def transformTo(f: File): IO[Unit] = transformer.use { t =>
        t.fromInput(build(inputs)).toFile(f).transform
      }

      val res = for {
        f   <- IO(File.createTempFile("output", null))
        _   <- transformTo(f)
        res <- readFile(f)
      } yield res

      res.assertEquals(expectedResult)
    }

    trait FileSystemTest {

      import cats.implicits._

      def resourcePath (path: String): String = getClass.getResource(path).getFile

      def renderedDoc (num: Int): String =
        """RootElement - Blocks: 1
          |. Paragraph - Spans: 1
          |. . Text - 'Doc""".stripMargin + num + "'"

      def readFiles (base: String): IO[List[String]] = List(
        readFile(base + "/doc-1.txt"),
        readFile(base + "/doc-2.txt"),
        readFile(base + "/tree-1/doc-3.txt"),
        readFile(base + "/tree-1/doc-4.txt"),
        readFile(base + "/tree-2/doc-5.txt"),
        readFile(base + "/tree-2/doc-6.txt"),
      ).sequence

      def readFilesFiltered (base: String): IO[List[String]] = List(
        readFile(base + "/doc-2.txt"),
        readFile(base + "/tree-2/doc-5.txt"),
        readFile(base + "/tree-2/doc-6.txt"),
      ).sequence

      def readFilesMerged (base: String): IO[List[String]] = List(
        readFile(base + "/doc-1.txt"),
        readFile(base + "/doc-2.txt"),
        readFile(base + "/tree-1/doc-3.txt"),
        readFile(base + "/tree-1/doc-4.txt"),
        readFile(base + "/tree-2/doc-5.txt"),
        readFile(base + "/tree-2/doc-6.txt"),
        readFile(base + "/tree-2/doc-7.txt"),
        readFile(base + "/tree-3/doc-8.txt"),
        readFile(base + "/doc-9.txt"),
      ).sequence
    }

    "read from and write to directories" in new FileSystemTest {
      val sourceName = resourcePath("/trees/a/")
      val expectedFileContents = (1 to 6).map(renderedDoc).toList
      val res = for {
        targetDir <- newTempDirectory
        _         <- transformer.use(_.fromDirectory(sourceName).toDirectory(targetDir).transform)
        results   <- readFiles(targetDir.getPath)
      } yield results
      res.assertEquals(expectedFileContents)
    }

    "transform a directory with a custom document type matcher" in new FileSystemTest {
      val sourceName = resourcePath("/trees/a/")
      val transformer = transformerWithBundle(BundleProvider.forDocTypeMatcher { 
        case Root / "doc1.md" => Ignored; case Root / "dir1" / _ => Ignored 
      })
      val expectedFileContents = List(2,5,6).map(renderedDoc)

      val res = for {
        targetDir  <- newTempDirectory
        _          <- transformer.use(_.fromDirectory(sourceName).toDirectory(targetDir).transform)
        contents   <- readFilesFiltered(targetDir.getPath)
        fileExists <- exists(new File(targetDir, "/doc1.txt"))
        dirExists  <- exists(new File(targetDir, "/dir1"))
      } yield (contents, fileExists, dirExists)

      res.assertEquals((expectedFileContents, false, false))
    }

    "allow to specify custom exclude filter" in new FileSystemTest {
      val sourceName = resourcePath("/trees/a/")
      val fileFilter = { f: File => f.getName == "doc1.md" || f.getName == "dir1" }
      val expectedFileContents = List(2,5,6).map(renderedDoc)
      
      val res = for {
        targetDir  <- newTempDirectory
        _          <- transformer.use(_.fromDirectory(sourceName, fileFilter).toDirectory(targetDir).transform)
        contents   <- readFilesFiltered(targetDir.getPath)
        fileExists <- exists(new File(targetDir, "/doc1.txt"))
        dirExists  <- exists(new File(targetDir, "/dir1"))
      } yield (contents, fileExists, dirExists)

      res.assertEquals((expectedFileContents, false, false))
    }

    "read from two root directories" in new FileSystemTest {
      val source1 = new File(resourcePath("/trees/a/"))
      val source2 = new File(resourcePath("/trees/b/"))
      val expectedFileContents = (1 to 9).map(renderedDoc).toList
      val res = for {
        targetDir <- newTempDirectory
        _         <- transformer.use(_.fromDirectories(Seq(source1, source2)).toDirectory(targetDir).transform)
        results   <- readFilesMerged(targetDir.getPath)
      } yield results 
      res.assertEquals(expectedFileContents)
    }

    "not copy files from the output directory if it's nested inside the input directory" in {
      new FileSystemTest {

        val result =
          """RootElement - Blocks: 1
            |. Paragraph - Spans: 1
            |. . Text - 'Hello'""".stripMargin

        val res = for {
          targetDir  <- newTempDirectory
          staticFile = new File(targetDir, "static.txt")
          inputFile  = new File(targetDir, "hello.md")
          subdir     = new File(targetDir, "sub")
          _          <- writeFile(inputFile, "Hello")
          _          <- writeFile(staticFile, "Text")
          _          <- mkDir(subdir)
          outputFile = new File(subdir, "hello.js")
          _          <- writeFile(outputFile, "Output")
          _          <- transformer.use(_.fromDirectory(targetDir).toDirectory(subdir).transform)
          hello      <- readFile(inputFile)
          static     <- readFile(new File(subdir, "static.txt"))
          result     <- readFile(new File(subdir, "hello.txt"))
          subExists  <- exists(new File(subdir, "sub"))
        } yield (hello, static, result, subExists)

        res.assertEquals(("Hello", "Text", result, false))
      }
    }
  }

}
