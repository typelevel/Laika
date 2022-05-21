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
import laika.io.model.{FileFilter, FilePath, InputTree, InputTreeBuilder, RenderContent, RenderedDocument, RenderedTree, RenderedTreeRoot, StringTreeOutput}
import laika.parse.Parser
import laika.parse.code.SyntaxHighlighting
import laika.parse.text.TextParsers
import laika.render.fo.TestTheme
import laika.rewrite.DefaultTemplatePath
import laika.rewrite.link.SlugBuilder
import laika.theme.ThemeProvider
import munit.CatsEffectSuite

import java.io.OutputStream

class TreeTransformerSpec extends CatsEffectSuite 
  with FileIO 
  with InputBuilder
  with RenderedTreeAssertions
  with IOTreeAssertions {

  
  private val transformer: Resource[IO, TreeTransformer[IO]] = Transformer.from(Markdown).to(AST).parallel[IO].build
  private def transformerWithBundle (bundle: ExtensionBundle): Resource[IO, TreeTransformer[IO]] = 
    Transformer.from(Markdown).to(AST).using(bundle).parallel[IO].build
  
  
  val astDefaultTemplatePath: Path = DefaultTemplatePath.forSuffix("txt")

  def input (in: Seq[(Path, String)], docTypeMatcher: Path => DocumentType): IO[InputTree[IO]] = build(in, docTypeMatcher)

  def transformTree (inputs: Seq[(Path, String)]): IO[RenderedTreeRoot[IO]] = transformWith(inputs)
  
  def transformWithConfig (inputs: Seq[(Path, String)], config: String): IO[RenderedTreeRoot[IO]] = 
    transformWithBundle(inputs, BundleProvider.forConfigString(config))
  def transformWithDocTypeMatcher (inputs: Seq[(Path, String)], matcher: PartialFunction[Path, DocumentType]): IO[RenderedTreeRoot[IO]] = 
    transformWithBundle(inputs, BundleProvider.forDocTypeMatcher(matcher))
  def transformWithTemplates (inputs: Seq[(Path, String)], parser: Parser[TemplateRoot]): IO[RenderedTreeRoot[IO]] = 
    transformWithBundle(inputs, BundleProvider.forTemplateParser(parser))
  def transformWithSlugBuilder (inputs: Seq[(Path, String)], f: String => String): IO[RenderedTreeRoot[IO]] = 
    transformWithBundle(inputs, BundleProvider.forSlugBuilder(f))
  def transformWithDirective (inputs: Seq[(Path, String)], directive: Templates.Directive): IO[RenderedTreeRoot[IO]] = 
    transformWithBundle(inputs, BundleProvider.forTemplateDirective(directive))
  def transformWithDocumentMapper (inputs: Seq[(Path, String)], f: Document => Document): IO[RenderedTreeRoot[IO]] = 
    transformWith(inputs, Transformer.from(Markdown).to(AST).parallel[IO].mapDocuments(f).build)

  def describe (inputs: InputTreeBuilder[IO]): IO[TransformerDescriptor] = Transformer
    .from(Markdown)
    .to(AST)
    .using(SyntaxHighlighting)
    .parallel[IO]
    .withAlternativeParser(MarkupParser.of(ReStructuredText))
    .build
    .use (_
      .fromInput(inputs)
      .toOutput(StringTreeOutput)
      .describe
    )

  def describe (inputs: Seq[(Path, String)]): IO[TransformerDescriptor] = describe(build(inputs))

  protected def transformWith (inputs: Seq[(Path, String)],
                               transformer: Resource[IO, TreeTransformer[IO]] = transformer): IO[RenderedTreeRoot[IO]] =
    transformer.use (_
      .fromInput(build(inputs))
      .toOutput(StringTreeOutput)
      .transform
    )

  private def transformWithBundle (inputs: Seq[(Path, String)], bundle: ExtensionBundle): IO[RenderedTreeRoot[IO]] =
    transformWith(inputs, Transformer.from(Markdown).to(AST).using(bundle).parallel[IO].build)

  def transformMixedMarkup (inputs: Seq[(Path, String)]): IO[RenderedTreeRoot[IO]] =
    transformWith(inputs, 
      Transformer.from(Markdown).to(AST).parallel[IO].withAlternativeParser(MarkupParser.of(ReStructuredText)).build)
  
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
    val link = "[link](http://foo.com)"
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
    staticDocuments = staticDocuments.map(ByteInput.empty(_))
  )

  def renderedTree(path: Path, content: Seq[RenderContent]): RenderedTree = RenderedTree(path, None, content)

  def renderedDoc(path: Path, expected: String): RenderedDocument = 
    RenderedDocument(path, None, Nil, expected, Config.empty)
  
  def docs (values: (Path, String)*): Seq[RenderedDocument] = 
    values.map { case (path, content) => renderedDoc(path, content) }

  def trees (values: (Path, Seq[RenderContent])*): Seq[RenderedTree] = 
    values.map { case (path, content) => renderedTree(path, content) }


  test("empty tree") {
    transformTree(Nil)
      .assertEquals(renderedRoot(Nil, staticDocuments = TestTheme.staticASTPaths))
  }

  test("tree with a single document") {
    val inputs = Seq(
      Root / "name.md" -> Contents.name
    )
    transformTree(inputs)
      .assertEquals(renderedRoot(docs((Root / "name.txt", simpleResult)), staticDocuments = TestTheme.staticASTPaths))
  }

  test("tree with a cover, title document and one content document") {
    val inputs = Seq(
      Root / "name.md" -> Contents.name,
      Root / "README.md" -> Contents.name,
      Root / "cover.md" -> Contents.name
    )
    transformTree(inputs).assertEquals(renderedRoot(
      docs((Root / "name.txt", simpleResult)),
      Some(renderedDoc(Root / "index.txt", simpleResult)),
      Some(renderedDoc(Root / "cover.txt", simpleResult)),
      staticDocuments = TestTheme.staticASTPaths
    ))
  }

  test("tree with a cover, title document and two content documents with a document mapper") {
    val inputs = Seq(
      Root / "rootDoc.md" -> Contents.name,
      Root / "sub" / "subDoc.md" -> Contents.name,
      Root / "README.md" -> Contents.name,
      Root / "cover.md" -> Contents.name
    )
    transformWithDocumentMapper(inputs, doc => doc.copy(content = doc.content.withContent(Seq(Paragraph("foo-bar")))))
      .assertEquals(renderedRoot(
        docs((Root / "rootDoc.txt", mappedResult)) ++ 
          trees((Root / "sub", docs((Root / "sub" / "subDoc.txt", mappedResult)))),
        Some(renderedDoc(Root / "index.txt", mappedResult)),
        Some(renderedDoc(Root / "cover.txt", mappedResult)),
        TestTheme.staticASTPaths
      ))
  }
  
  object TreeProcessors {
    val inputs = Seq(
      Root / "rootDoc.md" -> Contents.name,
      Root / "sub" / "subDoc.md" -> Contents.name,
      Root / "README.md" -> Contents.name,
      Root / "cover.md" -> Contents.name
    )
    val mapperFunction: Document => Document = doc => doc.copy(content = doc.content.withContent(Seq(Paragraph("foo-bar"))))
    val mapperFunctionExt: Document => Document = doc => doc.copy(content = doc.content.withContent(doc.content.content :+ Paragraph("baz")))
    def transformWithProcessor (theme: ThemeProvider): IO[RenderedTreeRoot[IO]] =
      transformWith(inputs, Transformer.from(Markdown).to(AST).parallel[IO].withTheme(theme).build)

    def run(theme: ThemeProvider, expectedDocResult: String): IO[Unit] =
      transformWithProcessor(theme)
        .assertEquals(renderedRoot(
          docs((Root / "rootDoc.txt", expectedDocResult)) ++
            trees((Root / "sub", docs((Root / "sub" / "subDoc.txt", expectedDocResult)))),
          Some(renderedDoc(Root / "index.txt", expectedDocResult)),
          Some(renderedDoc(Root / "cover.txt", expectedDocResult))
        ))
  }

  test("tree with a document mapper from a theme") {
    TreeProcessors.run(TestThemeBuilder.forDocumentMapper(TreeProcessors.mapperFunction), mappedResult)
  }

  test("tree with a document mapper from a theme and one from a theme extension") {
    val expectedResult: String = 
      """RootElement - Blocks: 2
        |. Paragraph - Spans: 1
        |. . Text - 'foo-bar'
        |. Paragraph - Spans: 1
        |. . Text - 'baz'""".stripMargin
    val theme = TestThemeBuilder.forDocumentMapper(TreeProcessors.mapperFunction)
      .extendWith(TestThemeBuilder.forDocumentMapper(TreeProcessors.mapperFunctionExt))
    TreeProcessors.run(theme, expectedResult)
  }

  test("tree with a document mapper from a theme specific to the output format") {
    TreeProcessors.run(TestThemeBuilder.forDocumentMapper(AST)(TreeProcessors.mapperFunction), mappedResult)
  }

  test("ignore the document mapper from a theme if the format does not match") {
    TreeProcessors.run(TestThemeBuilder.forDocumentMapper(HTML)(TreeProcessors.mapperFunction), simpleResult)
  }

  test("tree with a template document populated by a config file in the directory") {
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
    transformTree(inputs).assertEquals(renderedRoot(docs((Root / "main.txt", result)), staticDocuments = TestTheme.staticASTPaths))
  }

  test("tree with a template document populated by a root config string") {
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
    transformWithConfig(inputs, "value: def").assertEquals(renderedRoot(docs((Root / "main.txt", result)), staticDocuments = TestTheme.staticASTPaths))
  }

  test("tree with a custom template engine") {
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
    transformWithTemplates(inputs, parser).assertEquals(renderedRoot(docs(
      (Root / "main1.txt", result),
      (Root / "main2.txt", result)
    ), staticDocuments = TestTheme.staticASTPaths))
  }

  test("tree with a custom style sheet engine") {
    // the AST renderer does not use stylesheets, so we must use XSL-FO here
    def styleDecl (fontSize: String) =
      StyleDeclaration(StylePredicate.ElementType("Paragraph"), "font-size" -> s"${fontSize}pt")

    val parser: Parser[Set[StyleDeclaration]] = TextParsers.anyChars.map { n => Set(styleDecl(n)) }

    val result = RenderResult.fo.withHeliumTemplate("""<fo:block font-size="13pt">foo</fo:block>""")
    val transformer = Transformer
      .from(Markdown)
      .to(XSLFO)
      .using(BundleProvider.forStyleSheetParser(parser))
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

  test("tree with a template directive") {

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
    transformWithDirective(inputs, directive).assertEquals(renderedRoot(docs(
      (Root / "aa.txt", result)
    ), staticDocuments = TestTheme.staticASTPaths))
  }

  test("tree with a static document") {
    val inputs = Seq(
      Root / "omg.txt" -> Contents.name
    )
    transformTree(inputs).assertEquals(
      renderedRoot(Nil, staticDocuments = TestTheme.staticASTPaths :+ Root / "omg.txt")
    )
  }
  
  object DocWithSection {
    val targetSrc: String =
      """
        |Doc Title
        |=========
        |
        |Section Title
        |-------------
      """.stripMargin
    
    val title: SpanSequence = SpanSequence("Doc Title")

    def refSrc(sectionSlug: String): String =
      s"""
        |This is a [cross ref](../baz.md#$sectionSlug)
      """.stripMargin

    def targetRes(titleSlug: String, sectionSlug: String): String =
      s"""RootElement - Blocks: 2
        |. Title(Id($titleSlug) + Styles(title)) - Spans: 1
        |. . Text - 'Doc Title'
        |. Section
        |. . Header(2,Id($sectionSlug) + Styles(section)) - Spans: 1
        |. . . Text - 'Section Title'
        |. . Content - Blocks: 0""".stripMargin

    def refRes(sectionSlug: String): String =
      s"""RootElement - Blocks: 1
        |. Paragraph - Spans: 2
        |. . Text - 'This is a '
        |. . SpanLink(ResolvedInternalTarget(/baz.md#$sectionSlug,../baz.md#$sectionSlug,All),None) - Spans: 1
        |. . . Text - 'cross ref'""".stripMargin

    def inputs (sectionSlug: String): Seq[(Path, String)] = Seq(
      Root / "baz.md" -> targetSrc,
      Root / "foo" / "bar.md" -> refSrc(sectionSlug)
    )

    def assertTree(f: IO[RenderedTreeRoot[IO]],
                   titleSlug: String,
                   sectionSlug: String): Unit = f.assertEquals(renderedRoot(
      docs((Root / "baz.txt", targetRes(titleSlug, sectionSlug))).map(_.copy(
        title = Some(title), 
        sections = Seq(SectionInfo(sectionSlug, SpanSequence("Section Title"), Nil))
      )) ++
      trees((Root / "foo", docs((Root / "foo" / "bar.txt", refRes(sectionSlug))))),
      staticDocuments = TestTheme.staticASTPaths))
  }
  
  test("tree with an internal reference using the default slug builder") {
    DocWithSection.assertTree(transformTree(DocWithSection.inputs("section-title")), "doc-title", "section-title")
  }

  test("tree with an internal reference using a custom slug builder") {
    val sectionSlug = "section-title-slug"
    DocWithSection.assertTree(
      transformWithSlugBuilder(DocWithSection.inputs(sectionSlug), s => SlugBuilder.default(s) + "-slug"),
      "doc-title-slug",
      sectionSlug
    )
  }

  test("tree with all available file types and multiple markup formats") {
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
        |. . SpanLink(ExternalTarget(http://foo.com),None) - Spans: 1
        |. . . Text - 'link'""".stripMargin
    val rst =
      """RootElement - Blocks: 1
        |. Paragraph - Spans: 3
        |. . Text - '[link]('
        |. . SpanLink(ExternalTarget(http://foo.com),None) - Spans: 1
        |. . . Text - 'http://foo.com'
        |. . Text - ')'""".stripMargin
        
    transformMixedMarkup(inputs).assertEquals(renderedRoot(
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
      staticDocuments = TestTheme.staticASTPaths :+ Root / "dir2" / "omg.txt"
    ))
  }

  test("tree with while filtering documents based on their targetFormats setting") {
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
    transformTree(inputs).assertEquals(renderedRoot(
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

  test("describe a tree with all available file types and multiple markup formats") {
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
    describe(inputs)
      .map(_.formatted)
      .assertEquals(expected)
  }

  test("do not fail a describe operation in case of a directory input that does not exist") {
    val inputs = InputTree[IO].addDirectory("/xxx/yyy/zzz/does-not-exist/")
    describe(inputs).attempt.map(_.isRight).assert
  }

  object TwoPhaseTransformer extends InputBuilder {

    val binary: Resource[IO, BinaryTreeTransformer[IO]] = Transformer
      .from(ReStructuredText)
      .to(TestRenderResultProcessor)
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

  test("render a tree with a RenderResultProcessor writing to an output stream") {
    
    def transformTo(out: IO[OutputStream]): IO[Unit] = TwoPhaseTransformer.binary.use { t =>
      t.fromInput(build(TwoPhaseTransformer.inputs)).toStream(out).transform
    }

    withByteArrayTextOutput { out =>
      transformTo(IO.pure(out))
    }.assertEquals(TwoPhaseTransformer.expectedResult)
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

  test("render a tree with a RenderResultProcessor writing to a file") {
    def transformTo(f: FilePath): IO[Unit] = TwoPhaseTransformer.binary.use { t =>
      t.fromInput(build(TwoPhaseTransformer.inputs)).toFile(f).transform
    }

    val res = for {
      f   <- newTempFile
      _   <- transformTo(f)
      res <- readFile(f)
    } yield res

    res.assertEquals(TwoPhaseTransformer.expectedResult)
  }

  object FileSystemTest {

    import cats.implicits._

    def resourcePath (path: String): String = getClass.getResource(path).getFile

    def fileContent (num: Int): String =
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

  test("read from and write to directories") {
    import FileSystemTest._
    val sourceName = resourcePath("/trees/a/")
    val expectedFileContents = (1 to 6).map(fileContent).toList
    val res = for {
      targetDir <- newTempDirectory
      _         <- transformer.use(_.fromDirectory(sourceName).toDirectory(targetDir).transform)
      results   <- readFiles(targetDir.toString)
    } yield results
    res.assertEquals(expectedFileContents)
  }

  test("directory with a custom document type matcher") {
    import FileSystemTest._
    val sourceName = resourcePath("/trees/a/")
    val transformer = transformerWithBundle(BundleProvider.forDocTypeMatcher { 
      case Root / "doc1.md" => Ignored; case Root / "dir1" / _ => Ignored 
    })
    val expectedFileContents = List(2,5,6).map(fileContent)

    val res = for {
      targetDir  <- newTempDirectory
      _          <- transformer.use(_.fromDirectory(sourceName).toDirectory(targetDir).transform)
      contents   <- readFilesFiltered(targetDir.toString)
      fileExists <- exists(targetDir / "/doc1.txt")
      dirExists  <- exists(targetDir / "/dir1")
    } yield (contents, fileExists, dirExists)

    res.assertEquals((expectedFileContents, false, false))
  }

  test("allow to specify custom exclude filter") {
    import FileSystemTest._
    val sourceName = resourcePath("/trees/a/")
    val fileFilter = FileFilter.lift(f => f.name == "doc1.md" || f.name == "dir1")
    val expectedFileContents = List(2,5,6).map(fileContent)
    
    val res = for {
      targetDir  <- newTempDirectory
      _          <- transformer.use(_.fromDirectory(sourceName, fileFilter).toDirectory(targetDir).transform)
      contents   <- readFilesFiltered(targetDir.toString)
      fileExists <- exists(targetDir / "/doc1.txt")
      dirExists  <- exists(targetDir / "/dir1")
    } yield (contents, fileExists, dirExists)

    res.assertEquals((expectedFileContents, false, false))
  }

  test("read from two root directories") {
    import FileSystemTest._
    val source1 = FilePath.parse(resourcePath("/trees/a/"))
    val source2 = FilePath.parse(resourcePath("/trees/b/"))
    val expectedFileContents = (1 to 9).map(fileContent).toList
    val res = for {
      targetDir <- newTempDirectory
      _         <- transformer.use(_.fromDirectories(Seq(source1, source2)).toDirectory(targetDir).transform)
      results   <- readFilesMerged(targetDir.toString)
    } yield results 
    res.assertEquals(expectedFileContents)
  }

  test("do not copy files from the output directory if it's nested inside the input directory") {
    val result =
      """RootElement - Blocks: 1
        |. Paragraph - Spans: 1
        |. . Text - 'Hello'""".stripMargin

    val res = for {
      targetDir  <- newTempDirectory
      staticFile = targetDir / "static.txt"
      inputFile  = targetDir / "hello.md"
      subdir     = targetDir / "sub"
      _          <- writeFile(inputFile, "Hello")
      _          <- writeFile(staticFile, "Text")
      _          <- mkDir(subdir)
      outputFile = subdir / "hello.js"
      _          <- writeFile(outputFile, "Output")
      _          <- transformer.use(_.fromDirectory(targetDir).toDirectory(subdir).transform)
      hello      <- readFile(inputFile)
      static     <- readFile(subdir / "static.txt")
      result     <- readFile(subdir / "hello.txt")
      subExists  <- exists(subdir / "sub")
    } yield (hello, static, result, subExists)

    res.assertEquals(("Hello", "Text", result, false))
  }

}
