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

import cats.data.{ Chain, NonEmptyChain }
import cats.effect.{ Async, IO, Resource }
import cats.syntax.all._
import fs2.io.file.Files
import laika.api.Renderer
import laika.ast.Path.Root
import laika.ast._
import laika.ast.sample.{
  BuilderKey,
  ParagraphCompanionShortcuts,
  SampleConfig,
  SampleTrees,
  TestSourceBuilders
}
import laika.bundle.{ BundleOrigin, BundleProvider, ExtensionBundle }
import laika.config.{ Config, ConfigBuilder, LaikaKeys }
import laika.format._
import laika.helium.generate.FOStyles
import laika.io.api.{ BinaryTreeRenderer, TreeRenderer }
import laika.io.helper.{ InputBuilder, RenderResult, TestThemeBuilder }
import laika.io.implicits._
import laika.io.model._
import laika.io.runtime.RendererRuntime.{ DuplicatePath, RendererErrors }
import laika.io.runtime.VersionInfoGenerator
import laika.parse.GeneratedSource
import laika.parse.markup.DocumentParser.{ InvalidDocument, InvalidDocuments }
import laika.render._
import laika.render.fo.TestTheme
import laika.render.fo.TestTheme.staticHTMLPaths
import laika.rewrite.ReferenceResolver.CursorKeys
import laika.rewrite.nav.{ BasicPathTranslator, PrettyURLs, TargetFormats }
import laika.rewrite.{ DefaultTemplatePath, OutputContext, Version, VersionScannerConfig, Versions }
import munit.CatsEffectSuite

import scala.io.Codec

class TreeRendererSpec extends CatsEffectSuite
    with ParagraphCompanionShortcuts
    with FileIO
    with TestSourceBuilders
    with IOTreeAssertions { self =>

  object Inputs extends InputBuilder {

    def twoDocs(rootDoc: RootElement, subDoc: RootElement): DocumentTree = DocumentTree(
      Root,
      List(
        Document(Root / "doc", rootDoc),
        DocumentTree(Root / "tree", List(Document(Root / "tree" / "subdoc", subDoc)))
      )
    )

    def staticDoc(num: Int, path: Path = Root, formats: Option[String] = None): BinaryInput[IO] =
      ByteInput(
        "Static" + num,
        path / s"static$num.txt",
        formats.fold[TargetFormats](TargetFormats.All)(TargetFormats.Selected(_))
      )

  }

  object Results {

    val content: String = """RootElement - Blocks: 2
                            |. Paragraph - Spans: 1
                            |. . Text - 'aaö'
                            |. Paragraph - Spans: 1
                            |. . Text - 'bbb'""".stripMargin

    def titleWithId(text: String): Title = {
      val id = text
        .replaceAll("[^a-zA-Z0-9-]+", "-")
        .replaceFirst("^-", "")
        .replaceFirst("-$", "")
        .toLowerCase
      Title(text).withOptions(Id(id) + Style.title)
    }

    def doc(num: Int): String =
      """RootElement - Blocks: 1
        |. Paragraph - Spans: 1
        |. . Text - 'Text """.stripMargin + num + "'"

    def rootWithSingleDoc(
        docPath: Path,
        content: String,
        outputContext: OutputContext,
        staticPaths: Seq[Path] = Nil
    ): RenderedTreeRoot[IO] = {
      root(List(doc(docPath, content)), None, outputContext, staticDocuments = staticPaths)
    }

    def root(
        content: Seq[RenderContent],
        title: Option[SpanSequence],
        outputContext: OutputContext,
        titleDocument: Option[RenderedDocument] = None,
        coverDocument: Option[RenderedDocument] = None,
        staticDocuments: Seq[Path] = Nil
    ): RenderedTreeRoot[IO] = RenderedTreeRoot(
      RenderedTree(Root, title, content, titleDocument),
      TemplateRoot.fallback,
      Config.empty,
      outputContext,
      BasicPathTranslator(
        outputContext.fileSuffix
      ), // not part of the assertions, so not reflecting actual instance
      coverDocument = coverDocument,
      staticDocuments = staticDocuments.map(Inputs.ByteInput.empty(_))
    )

    def tree(path: Path, content: Seq[RenderContent]): RenderedTree =
      RenderedTree(path, None, content)

    def doc(path: Path): RenderedDocument                   = doc(path, content, "Title")
    def doc(path: Path, expected: String): RenderedDocument = doc(path, expected, "Title")

    def doc(path: Path, expected: String, title: String): RenderedDocument =
      RenderedDocument(path, Some(SpanSequence(title)), Nil, expected, Config.empty)

    def docNoTitle(path: Path, expected: String): RenderedDocument =
      RenderedDocument(path, None, Nil, expected, Config.empty)

    def docNoTitle(path: Path): RenderedDocument = docNoTitle(path, content)

    def betweenBrackets(span: TemplateSpan): TemplateRoot = between(span, "[", "]")

    def between(span: TemplateSpan, before: String, after: String): TemplateRoot =
      TemplateRoot(TemplateString(before), span, TemplateString(after))

  }

  trait TreeRendererSetup[FMT] {

    val fontConfigTree: DocumentTree = DocumentTree(
      Root / "laika",
      List(
        DocumentTree(
          Root / "laika" / "fonts",
          Nil,
          config = ConfigBuilder
            .empty
            .withValue(LaikaKeys.targetFormats, Seq("epub", "epub.xhtml", "pdf"))
            .build
        )
      )
    )

    def styles: StyleDeclarationSet = StyleDeclarationSet.empty

    def defaultContent: RootElement = RootElement(p("aaö"), p("bbb"))
    def defaultTree: DocumentTree   = defaultTree(defaultContent)

    def defaultTree(content: RootElement): DocumentTree =
      DocumentTree(Root, List(Document(Root / "doc", content)))

    def defaultRoot(input: DocumentTree): DocumentTreeRoot =
      DocumentTreeRoot(
        input,
        styles = Map("fo" -> styles).withDefaultValue(StyleDeclarationSet.empty)
      )

    def defaultRenderer: Resource[IO, TreeRenderer[IO]]

    def render(input: DocumentTree): IO[RenderedTreeRoot[IO]] =
      render(defaultRoot(input), defaultRenderer)

    def render(
        input: DocumentTree,
        renderer: Resource[IO, TreeRenderer[IO]]
    ): IO[RenderedTreeRoot[IO]] =
      render(defaultRoot(input), renderer)

    def render(input: DocumentTreeRoot): IO[RenderedTreeRoot[IO]] = render(input, defaultRenderer)

    def render(
        input: DocumentTreeRoot,
        renderer: Resource[IO, TreeRenderer[IO]]
    ): IO[RenderedTreeRoot[IO]] = renderer
      .use(
        _
          .from(input)
          .toOutput(StringTreeOutput)
          .render
      )

    def addPosition(tree: DocumentTree, pos: Seq[Int] = Nil): DocumentTree = {
      val nextNum = Iterator.from(1)
      tree.copy(content = tree.content.map {
        case d: Document     => d.copy(position = TreePosition(pos :+ nextNum.next()))
        case t: DocumentTree =>
          val num = pos :+ nextNum.next()
          addPosition(t.copy(position = TreePosition(num)), num)
      })
    }

  }

  object ASTRenderer extends TreeRendererSetup[TextFormatter] {
    val outputContext: OutputContext = OutputContext(AST)

    val staticPaths = Seq( // TODO - why do they differ from TreeTransformerSpec?
      Root / "laika" / "fonts" / "Lato-Regular.ttf",
      Root / "laika" / "fonts" / "Lato-Italic.ttf",
      Root / "laika" / "fonts" / "Lato-Bold.ttf",
      Root / "laika" / "fonts" / "Lato-BoldItalic.ttf",
      Root / "laika" / "fonts" / "FiraMono-Medium.otf",
      Root / "laika" / "fonts" / "icofont.ttf",
      Root / "helium" / "fonts" / "icofont.woff",
      Root / "helium" / "fonts" / "icofont.woff2"
    )

    lazy val defaultRenderer: Resource[IO, TreeRenderer[IO]] = Renderer.of(AST).parallel[IO].build
  }

  object HTMLRenderer extends TreeRendererSetup[HTMLFormatter] {
    import Results.titleWithId

    val outputContext: OutputContext = OutputContext(HTML)
    val staticPaths: Seq[Path]       = staticHTMLPaths.filterNot(_.suffix.contains("epub.css"))
    override val defaultContent: RootElement = RootElement(titleWithId("Title"), p("bbb"))
    lazy val defaultRenderer: Resource[IO, TreeRenderer[IO]] = Renderer.of(HTML).parallel[IO].build
  }

  object EPUB_XHTMLRenderer extends TreeRendererSetup[HTMLFormatter] {
    import Results.titleWithId

    val outputContext: OutputContext = OutputContext(EPUB.XHTML)

    val staticPaths: Seq[Path] = staticHTMLPaths.filterNot(path =>
      path.name == "laika-helium.css" || path.name == "icofont.min.css" || path.name == "landing.page.css" || path.suffix.contains(
        "js"
      )
    )

    override val defaultContent: RootElement = RootElement(titleWithId("Title"), p("bbb"))

    lazy val defaultRenderer: Resource[IO, TreeRenderer[IO]] =
      Renderer.of(EPUB.XHTML).parallel[IO].build

  }

  object FORenderer extends TreeRendererSetup[FOFormatter] {
    import Results.titleWithId

    val outputContext: OutputContext = OutputContext(XSLFO)

    val staticPaths = Seq(
      Root / "laika" / "fonts" / "Lato-Regular.ttf",
      Root / "laika" / "fonts" / "Lato-Italic.ttf",
      Root / "laika" / "fonts" / "Lato-Bold.ttf",
      Root / "laika" / "fonts" / "Lato-BoldItalic.ttf",
      Root / "laika" / "fonts" / "FiraMono-Medium.otf",
      Root / "laika" / "fonts" / "icofont.ttf",
      Root / "helium" / "fonts" / "icofont.woff",
      Root / "helium" / "fonts" / "icofont.woff2"
    )

    override def styles: StyleDeclarationSet = TestTheme.foStyles

    val customStyle: StyleDeclaration =
      StyleDeclaration(StylePredicate.ElementType("Paragraph"), "font-size" -> "11pt")

    val customThemeStyles: Set[StyleDeclaration] = {
      val baseStyles = TestTheme.foStyles.styles
      baseStyles + customStyle.increaseOrderBy(baseStyles.size)
    }

    override val defaultContent: RootElement = RootElement(titleWithId("Title"), p("bbb"))
    val subElem: RootElement                 = RootElement(titleWithId("Sub Title"), p("ccc"))

    val defaultParagraphStyles =
      """font-family="serif" font-size="10pt" line-height="1.5" space-after="3mm" text-align="justify""""

    val overriddenParagraphStyles =
      """font-family="serif" font-size="11pt" line-height="1.5" space-after="3mm" text-align="justify""""

    def title(id: String, text: String) =
      s"""<fo:block id="$id" color="#007c99" font-family="sans-serif" font-size="24pt" font-weight="bold" keep-with-next="always" space-after="6mm" space-before="0mm">$text</fo:block>"""

    lazy val defaultRenderer: Resource[IO, TreeRenderer[IO]] = Renderer
      .of(XSLFO)
      .parallel[IO]
      .withTheme(TestTheme.heliumTestProps.build)
      .build

  }

  test("empty tree") {
    val input = DocumentTree(Root, Nil)
    ASTRenderer.render(input)
      .assertEquals(
        Results.root(
          Nil,
          None,
          ASTRenderer.outputContext,
          staticDocuments = ASTRenderer.staticPaths
        )
      )
  }

  test("tree with a single document") {
    val expected = Results
      .root(
        List(Results.docNoTitle(Root / "doc.txt")),
        None,
        ASTRenderer.outputContext,
        staticDocuments = ASTRenderer.staticPaths
      )
    ASTRenderer
      .render(ASTRenderer.defaultTree)
      .assertEquals(expected)
  }

  test("tree with a single document to HTML using the default template") {
    val html     = RenderResult.html.withDefaultTemplate(
      "Title",
      """<h1 id="title" class="title">Title</h1>
        |<p>bbb</p>""".stripMargin
    )
    val expected = Results
      .root(
        List(Results.doc(Root / "doc.html", html)),
        None,
        HTMLRenderer.outputContext,
        staticDocuments = HTMLRenderer.staticPaths
      )
    HTMLRenderer
      .render(HTMLRenderer.defaultTree)
      .assertEquals(expected)
  }

  test("fail with duplicate paths") {
    val root  = HTMLRenderer.defaultContent
    val input = DocumentTree(
      Root,
      List(
        Document(Root / "doc1", root),
        Document(Root / "doc2", root),
        Document(Root / "doc2", root),
        Document(Root / "sub" / "doc", root),
        Document(Root / "sub" / "doc", root)
      )
    )
    HTMLRenderer.defaultRenderer
      .use(
        _
          .from(HTMLRenderer.defaultRoot(input))
          .toOutput(StringTreeOutput)
          .render
      )
      .attempt
      .assertEquals(
        Left(
          RendererErrors(Seq(DuplicatePath(Root / "doc2"), DuplicatePath(Root / "sub" / "doc")))
        )
      )
  }

  test("collect errors from multiple documents") {

    def invalidLink(key: BuilderKey): Seq[Block] =
      Seq(
        InvalidBlock(
          s"unresolved link reference: link${key.num}",
          generatedSource(s"[link${key.num}]")
        )
      )

    val input = SampleTrees.sixDocuments
      .docContent(invalidLink _)
      .build.tree

    val invalidDocuments = input.allDocuments.map { doc =>
      val msg         = s"unresolved link reference: link${doc.path.name.last}"
      val invalidSpan = InvalidBlock(msg, generatedSource(s"[link${doc.path.name.last}]"))
      InvalidDocument(doc.path, invalidSpan)
    }
    HTMLRenderer.defaultRenderer
      .use(
        _
          .from(HTMLRenderer.defaultRoot(input))
          .toOutput(StringTreeOutput)
          .render
      )
      .attempt
      .assertEquals(
        Left(
          InvalidDocuments(NonEmptyChain.fromChainUnsafe(Chain.fromSeq(invalidDocuments)))
        )
      )
  }

  test("tree with a single document to HTML using a custom template in the root directory") {
    val template = TemplateDocument(
      DefaultTemplatePath.forHTML,
      Results.betweenBrackets(
        TemplateContextReference(CursorKeys.documentContent, required = true, GeneratedSource)
      )
    )
    val input    = HTMLRenderer.defaultTree.copy(templates = Seq(template))
    val expected = """[<h1 id="title" class="title">Title</h1>
                     |<p>bbb</p>]""".stripMargin
    HTMLRenderer
      .render(input)
      .assertEquals(
        Results.rootWithSingleDoc(
          Root / "doc.html",
          expected,
          HTMLRenderer.outputContext,
          HTMLRenderer.staticPaths
        )
      )
  }

  test("tree with a single document to HTML using a render override in a theme") {
    val renderer = Renderer.of(HTML).parallel[IO]
      .withTheme(
        TestThemeBuilder.forBundle(
          BundleProvider.forOverrides(
            HTML.Overrides { case (fmt, Text(txt, _)) =>
              fmt.text(txt + "!")
            },
            origin = BundleOrigin.Theme
          )
        )
      ).build

    val expected = """<h1 id="title" class="title">Title!</h1>
                     |<p>bbb!</p>""".stripMargin
    HTMLRenderer
      .render(HTMLRenderer.defaultTree, renderer)
      .assertEquals(
        Results.rootWithSingleDoc(Root / "doc.html", expected, HTMLRenderer.outputContext)
      )
  }

  test("tree with a single document to HTML using a path translator in a theme") {
    val renderer = Renderer
      .of(HTML)
      .parallel[IO]
      .withTheme(
        TestThemeBuilder.forBundle(BundleProvider.forPathTranslator()(_.withSuffix("xhtml")))
      )
      .build

    val expected = """<h1 id="title" class="title">Title</h1>
                     |<p>bbb</p>""".stripMargin
    HTMLRenderer
      .render(HTMLRenderer.defaultTree, renderer)
      .assertEquals(
        Results.rootWithSingleDoc(Root / "doc.xhtml", expected, HTMLRenderer.outputContext)
      )
  }

  private def renderOverrideBundle(appendChar: Char, origin: BundleOrigin): ExtensionBundle =
    BundleProvider.forOverrides(
      HTML.Overrides { case (fmt, Text(txt, _)) =>
        fmt.text(txt + appendChar)
      },
      origin
    )

  test(
    "tree with a single document to HTML with a render override that shadows an override in a theme"
  ) {
    val renderer = Renderer
      .of(HTML)
      .using(renderOverrideBundle('?', BundleOrigin.User))
      .parallel[IO]
      .withTheme(TestThemeBuilder.forBundle(renderOverrideBundle('!', BundleOrigin.Theme)))
      .build

    val expected = """<h1 id="title" class="title">Title?</h1>
                     |<p>bbb?</p>""".stripMargin
    HTMLRenderer
      .render(HTMLRenderer.defaultTree, renderer)
      .assertEquals(
        Results.rootWithSingleDoc(Root / "doc.html", expected, HTMLRenderer.outputContext)
      )
  }

  test(
    "tree with a single document to HTML with a render override in a theme extension that shadows an override in a base theme"
  ) {
    val theme    = TestThemeBuilder.forBundle(renderOverrideBundle('!', BundleOrigin.Theme))
      .extendWith(TestThemeBuilder.forBundle(renderOverrideBundle('?', BundleOrigin.Theme)))
    val renderer = Renderer
      .of(HTML)
      .parallel[IO]
      .withTheme(theme)
      .build

    val expected = """<h1 id="title" class="title">Title?</h1>
                     |<p>bbb?</p>""".stripMargin
    HTMLRenderer
      .render(HTMLRenderer.defaultTree, renderer)
      .assertEquals(
        Results.rootWithSingleDoc(Root / "doc.html", expected, HTMLRenderer.outputContext)
      )
  }

  test("tree with a single document to HTML using a custom template in an extension bundle") {
    val template = Results.betweenBrackets(
      TemplateContextReference(CursorKeys.documentContent, required = true, GeneratedSource)
    )
    val inputs   = new TestThemeBuilder.Inputs {
      def build[F[_]: Async] = InputTree[F]
        .addTemplate(TemplateDocument(DefaultTemplatePath.forHTML, template))
    }
    val renderer = Renderer.of(HTML)
      .parallel[IO]
      .withTheme(TestThemeBuilder.forInputs(inputs))
      .build
    val expected = """[<h1 id="title" class="title">Title</h1>
                     |<p>bbb</p>]""".stripMargin
    HTMLRenderer
      .render(HTMLRenderer.defaultTree, renderer)
      .assertEquals(
        Results.rootWithSingleDoc(Root / "doc.html", expected, HTMLRenderer.outputContext)
      )
  }

  test("tree with a cover and title document to HTML") {
    val input    = DocumentTree(
      Root,
      List(Document(Root / "doc", HTMLRenderer.defaultContent), HTMLRenderer.fontConfigTree),
      Some(Document(Root / "README", HTMLRenderer.defaultContent))
    )
    val treeRoot = DocumentTreeRoot(
      input,
      coverDocument = Some(Document(Root / "cover", HTMLRenderer.defaultContent))
    )
    val expected = RenderResult.html.withDefaultTemplate(
      "Title",
      """<h1 id="title" class="title">Title</h1>
        |<p>bbb</p>""".stripMargin
    )
    HTMLRenderer
      .render(treeRoot)
      .assertEquals(
        Results.root(
          List(Results.doc(Root / "doc.html", expected)),
          None,
          HTMLRenderer.outputContext,
          Some(Results.doc(Root / "index.html", expected)),
          Some(Results.doc(Root / "cover.html", expected)),
          staticDocuments = Seq(
            Root / "helium" / "laika-helium.js",
            Root / "helium" / "landing.page.css",
            Root / "helium" / "icofont.min.css",
            Root / "helium" / "fonts" / "icofont.woff",
            Root / "helium" / "fonts" / "icofont.woff2",
            Root / "helium" / "laika-helium.css"
          )
        )
      )
  }

  test("tree with several documents to HTML using PrettyURLs extension") {
    val renderer = Renderer
      .of(HTML)
      .using(PrettyURLs)
      .parallel[IO]
      .build

    val contentWithLink: RootElement = RootElement(
      Results.titleWithId("Title"),
      p(SpanLink.internal("doc-2.md")("Link Text"))
    )
    val input                        = DocumentTree(
      Root,
      List(
        Document(Root / "doc-1.md", contentWithLink),
        HTMLRenderer.fontConfigTree,
        Document(Root / "doc-2.md", HTMLRenderer.defaultContent),
        HTMLRenderer.fontConfigTree
      ),
      Some(Document(Root / "README.md", HTMLRenderer.defaultContent))
    )
    val treeRoot                     = DocumentTreeRoot(input)
    val expectedDefault              = RenderResult.html.withDefaultTemplate(
      "Title",
      """<h1 id="title" class="title">Title</h1>
        |<p>bbb</p>""".stripMargin
    )
    val expectedContentWithLink      = RenderResult.html.withDefaultTemplate(
      "Title",
      """<h1 id="title" class="title">Title</h1>
        |<p><a href="../doc-2/">Link Text</a></p>""".stripMargin
    )
    HTMLRenderer
      .render(treeRoot, renderer)
      .assertEquals(
        Results.root(
          List(
            Results.doc(Root / "doc-1" / "index.html", expectedContentWithLink),
            Results.doc(Root / "doc-2" / "index.html", expectedDefault)
          ),
          None,
          HTMLRenderer.outputContext,
          Some(Results.doc(Root / "index.html", expectedDefault)),
          None,
          staticDocuments = Seq(
            Root / "helium" / "laika-helium.js",
            Root / "helium" / "landing.page.css",
            Root / "helium" / "icofont.min.css",
            Root / "helium" / "fonts" / "icofont.woff",
            Root / "helium" / "fonts" / "icofont.woff2",
            Root / "helium" / "laika-helium.css"
          )
        )
      )
  }

  test("tree with a single document to EPUB.XHTML using the default template") {
    val html = RenderResult.epub.withDefaultTemplate(
      "Title",
      """<h1 id="title" class="title">Title</h1>
        |<p>bbb</p>""".stripMargin
    )
    val path = (Root / "doc").withSuffix("epub.xhtml")
    EPUB_XHTMLRenderer
      .render(EPUB_XHTMLRenderer.defaultTree)
      .assertEquals(
        Results.rootWithSingleDoc(
          path,
          html,
          EPUB_XHTMLRenderer.outputContext,
          EPUB_XHTMLRenderer.staticPaths
        )
      )
  }

  test("tree with a single document to EPUB.XHTML using a custom template in the root directory") {
    val template = TemplateDocument(
      DefaultTemplatePath.forEPUB,
      Results.betweenBrackets(
        TemplateContextReference(CursorKeys.documentContent, required = true, GeneratedSource)
      )
    )
    val input    = EPUB_XHTMLRenderer.defaultTree.copy(templates = Seq(template))
    val expected = """[<h1 id="title" class="title">Title</h1>
                     |<p>bbb</p>]""".stripMargin
    val path     = (Root / "doc").withSuffix("epub.xhtml")
    EPUB_XHTMLRenderer
      .render(input)
      .assertEquals(
        Results.rootWithSingleDoc(
          path,
          expected,
          EPUB_XHTMLRenderer.outputContext,
          EPUB_XHTMLRenderer.staticPaths
        )
      )
  }

  test("tree with a single document to EPUB.XHTML using a custom template in a theme") {
    val template = Results.betweenBrackets(
      TemplateContextReference(CursorKeys.documentContent, required = true, GeneratedSource)
    )
    val inputs   = new TestThemeBuilder.Inputs {
      def build[F[_]: Async] = InputTree[F]
        .addTemplate(TemplateDocument(DefaultTemplatePath.forEPUB, template))
    }
    val renderer =
      Renderer
        .of(EPUB.XHTML)
        .parallel[IO]
        .withTheme(TestThemeBuilder.forInputs(inputs))
        .build
    val expected = """[<h1 id="title" class="title">Title</h1>
                     |<p>bbb</p>]""".stripMargin
    val path     = (Root / "doc").withSuffix("epub.xhtml")
    EPUB_XHTMLRenderer
      .render(EPUB_XHTMLRenderer.defaultTree, renderer)
      .assertEquals(Results.rootWithSingleDoc(path, expected, EPUB_XHTMLRenderer.outputContext))
  }

  test(
    "tree with a single document to EPUB.XHTML using a custom template in a theme extension overriding a template in the base theme"
  ) {
    val contentRef           =
      TemplateContextReference(CursorKeys.documentContent, required = true, GeneratedSource)
    val baseThemeInputs      = new TestThemeBuilder.Inputs {
      def build[F[_]: Async] = InputTree[F]
        .addTemplate(
          TemplateDocument(DefaultTemplatePath.forEPUB, Results.betweenBrackets(contentRef))
        )
    }
    val themeExtensionInputs = new TestThemeBuilder.Inputs {
      def build[F[_]: Async] = InputTree[F]
        .addTemplate(
          TemplateDocument(DefaultTemplatePath.forEPUB, Results.between(contentRef, "?", "?"))
        )
    }
    val theme                = TestThemeBuilder.forInputs(baseThemeInputs).extendWith(
      TestThemeBuilder.forInputs(themeExtensionInputs)
    )
    val renderer             =
      Renderer
        .of(EPUB.XHTML)
        .parallel[IO]
        .withTheme(theme)
        .build
    val expected             = """?<h1 id="title" class="title">Title</h1>
                     |<p>bbb</p>?""".stripMargin
    val path                 = (Root / "doc").withSuffix("epub.xhtml")
    EPUB_XHTMLRenderer
      .render(EPUB_XHTMLRenderer.defaultTree, renderer)
      .assertEquals(Results.rootWithSingleDoc(path, expected, EPUB_XHTMLRenderer.outputContext))
  }

  test("tree with a single document to XSL-FO using the default template and default CSS") {
    val expected =
      RenderResult.fo.withFallbackTemplate(s"""${FORenderer.title("_doc_title", "Title")}
                                              |<fo:block ${
                                               FORenderer.defaultParagraphStyles
                                             }>bbb</fo:block>""".stripMargin)
    val path     = Root / "doc.fo"
    FORenderer
      .render(FORenderer.defaultTree)
      .assertEquals(
        Results.rootWithSingleDoc(path, expected, FORenderer.outputContext, FORenderer.staticPaths)
      )
  }

  test("tree with a single document to XSL-FO using a custom template") {
    val template = TemplateDocument(
      DefaultTemplatePath.forFO,
      Results.betweenBrackets(
        TemplateContextReference(CursorKeys.documentContent, required = true, GeneratedSource)
      )
    )
    val input    = FORenderer.defaultTree.copy(templates = Seq(template))
    val expected = s"""[${FORenderer.title("_doc_title", "Title")}
                      |<fo:block ${FORenderer.defaultParagraphStyles}>bbb</fo:block>]""".stripMargin
    val path     = Root / "doc.fo"
    FORenderer
      .render(input)
      .assertEquals(
        Results.rootWithSingleDoc(path, expected, FORenderer.outputContext, FORenderer.staticPaths)
      )
  }

  test("tree with two documents to XSL-FO using a custom style sheet in a theme") {
    import FORenderer._

    val inputs       = new TestThemeBuilder.Inputs {
      def build[F[_]: Async] = InputTree[F]
        .addStyles(customThemeStyles, FOStyles.defaultPath)
        .addTemplate(TemplateDocument(DefaultTemplatePath.forFO, TestTheme.foTemplate))
    }
    val renderer     =
      Renderer
        .of(XSLFO)
        .parallel[IO]
        .withTheme(TestThemeBuilder.forInputs(inputs))
        .build
    val input        = Inputs.twoDocs(defaultContent, subElem)
    val expectedRoot = RenderResult.fo.withFallbackTemplate(
      s"""${title("_doc_title", "Title")}
         |<fo:block $overriddenParagraphStyles>bbb</fo:block>""".stripMargin
    )
    val expectedSub  = RenderResult.fo.withFallbackTemplate(
      s"""${title("_tree_subdoc_sub-title", "Sub Title")}
         |<fo:block $overriddenParagraphStyles>ccc</fo:block>""".stripMargin
    )
    FORenderer
      .render(input, renderer)
      .assertEquals(
        Results.root(
          List(
            Results.doc(Root / "doc.fo", expectedRoot),
            Results.tree(
              Root / "tree",
              List(
                Results.doc(Root / "tree" / "subdoc.fo", expectedSub, "Sub Title")
              )
            )
          ),
          None,
          FORenderer.outputContext
        )
      )
  }

  test("tree with two documents to XSL-FO using a custom style sheet in the tree root") {
    import FORenderer._

    val input                                      = Inputs.twoDocs(defaultContent, subElem)
    val foStyles: Map[String, StyleDeclarationSet] =
      Map("fo" -> (styles ++ StyleDeclarationSet(FOStyles.defaultPath, customStyle)))
    val expectedRoot                               = RenderResult.fo.withFallbackTemplate(
      s"""${title("_doc_title", "Title")}
         |<fo:block $overriddenParagraphStyles>bbb</fo:block>""".stripMargin
    )
    val expectedSub                                = RenderResult.fo.withFallbackTemplate(
      s"""${title("_tree_subdoc_sub-title", "Sub Title")}
         |<fo:block $overriddenParagraphStyles>ccc</fo:block>""".stripMargin
    )
    render(DocumentTreeRoot(input, styles = foStyles))
      .assertEquals(
        Results.root(
          List(
            Results.doc(Root / "doc.fo", expectedRoot),
            Results.tree(
              Root / "tree",
              List(
                Results.doc(Root / "tree" / "subdoc.fo", expectedSub, "Sub Title")
              )
            )
          ),
          None,
          FORenderer.outputContext,
          staticDocuments = staticPaths
        )
      )
  }

  test("tree with a single static document") {
    val input = DocumentTree(Root, Seq(ASTRenderer.fontConfigTree))

    val treeRoot =
      DocumentTreeRoot(input, staticDocuments = Seq(StaticDocument(Inputs.staticDoc(1).path)))

    ASTRenderer.defaultRenderer
      .use(
        _
          .from(treeRoot)
          .copying(Seq(Inputs.ByteInput("...", Root / "static1.txt")))
          .toOutput(StringTreeOutput)
          .render
      )
      .assertEquals(
        Results.root(
          Nil,
          None,
          ASTRenderer.outputContext,
          staticDocuments = Seq(Root / "static1.txt") ++ TestTheme.staticASTPaths
        )
      )
  }

  test("tree with a single static document from a theme") {
    val input = DocumentTree(Root, Nil)

    val treeRoot =
      DocumentTreeRoot(input, staticDocuments = Seq(StaticDocument(Inputs.staticDoc(1).path)))

    val inputs = new TestThemeBuilder.Inputs {
      def build[F[_]: Async] = InputTree[F].addString("...", Root / "static1.txt")
    }
    val theme  = TestThemeBuilder.forInputs(inputs)
    Renderer
      .of(AST)
      .parallel[IO]
      .withTheme(theme)
      .build
      .use(
        _
          .from(treeRoot)
          .toOutput(StringTreeOutput)
          .render
      )
      .assertEquals(
        Results.root(
          Nil,
          None,
          ASTRenderer.outputContext,
          staticDocuments = Seq(Root / "static1.txt")
        )
      )
  }

  test("tree with all available file types") {
    import ASTRenderer._

    val input = addPosition(
      SampleTrees.sixDocuments
        .doc6.config(SampleConfig.targetFormats("zzz"))
        .build
        .tree
    )

    val finalInput = input.appendContent(fontConfigTree)

    val staticDocs = Seq(
      Inputs.staticDoc(1, Root),
      Inputs.staticDoc(2, Root),
      Inputs.staticDoc(3, Root / "dir1"),
      Inputs.staticDoc(4, Root / "dir1"),
      Inputs.staticDoc(5, Root / "dir2"),
      Inputs.staticDoc(6, Root / "dir2"),
      Inputs.staticDoc(7, Root / "dir2", Some("zzz"))
    )

    val treeRoot = DocumentTreeRoot(
      finalInput,
      staticDocuments = staticDocs.map(doc => StaticDocument(doc.path, doc.formats))
    )

    val expectedStatic   = staticDocs.dropRight(1).map(_.path)
    val expectedRendered = Results.root(
      List(
        Results.docNoTitle(Root / "doc-1.txt", Results.doc(1)),
        Results.docNoTitle(Root / "doc-2.txt", Results.doc(2)),
        Results.tree(
          Root / "tree-1",
          List(
            Results.docNoTitle(Root / "tree-1" / "doc-3.txt", Results.doc(3)),
            Results.docNoTitle(Root / "tree-1" / "doc-4.txt", Results.doc(4))
          )
        ),
        Results.tree(
          Root / "tree-2",
          List(
            Results.docNoTitle(Root / "tree-2" / "doc-5.txt", Results.doc(5))
          )
        )
      ),
      None,
      outputContext,
      staticDocuments = expectedStatic ++ TestTheme.staticASTPaths
    )

    defaultRenderer
      .use(
        _
          .from(treeRoot)
          .copying(staticDocs)
          .toOutput(StringTreeOutput)
          .render
      )
      .assertEquals(expectedRendered)
  }

  test("render tree while excluding all unversioned documents, based on configuration") {
    val versions   = Versions(Version("0.4.x", "0.4"), Seq(), Seq(), renderUnversioned = false)
    val input      = SampleTrees.sixDocuments
      .root.config(_.withValue(versions))
      .tree1.config(SampleConfig.versioned(true))
      .tree2.config(SampleConfig.versioned(true))
      .build
      .tree
    val finalInput = input.appendContent(HTMLRenderer.fontConfigTree)

    val staticDocs = Seq(
      Inputs.staticDoc(1, Root),
      Inputs.staticDoc(2, Root),
      Inputs.staticDoc(3, Root / "tree-1"),
      Inputs.staticDoc(4, Root / "tree-1"),
      Inputs.staticDoc(5, Root / "tree-2"),
      Inputs.staticDoc(6, Root / "tree-2")
    )

    val treeRoot = DocumentTreeRoot(
      finalInput,
      staticDocuments = staticDocs.map(doc => StaticDocument(doc.path, doc.formats))
    )

    def docHTML(num: Int): String = s"<p>Text $num</p>"

    val expectedStatic   = staticDocs.drop(2).map(in => Root / "0.4" / in.path.relative)
    val expectedRendered = Results.root(
      List(
        Results.tree(
          Root / "0.4",
          List(
            Results.tree(
              Root / "0.4" / "tree-1",
              List(
                Results.docNoTitle(Root / "0.4" / "tree-1" / "doc-3.html", docHTML(3)),
                Results.docNoTitle(Root / "0.4" / "tree-1" / "doc-4.html", docHTML(4))
              )
            ),
            Results.tree(
              Root / "0.4" / "tree-2",
              List(
                Results.docNoTitle(Root / "0.4" / "tree-2" / "doc-5.html", docHTML(5)),
                Results.docNoTitle(Root / "0.4" / "tree-2" / "doc-6.html", docHTML(6))
              )
            )
          )
        )
      ),
      None,
      HTMLRenderer.outputContext,
      staticDocuments = expectedStatic
    )

    HTMLRenderer.defaultRenderer
      .use(
        _
          .from(treeRoot)
          .copying(staticDocs)
          .toOutput(StringTreeOutput)
          .render
      )
      .assertEquals(expectedRendered)
  }

  object TwoPhaseRenderer {
    val rootElem: RootElement = RootElement(Results.titleWithId("Title"), p("bbb"))
    val subElem: RootElement  = RootElement(Results.titleWithId("Sub Title"), p("ccc"))

    val input: DocumentTree = Inputs.twoDocs(rootElem, subElem)

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

    val renderer: Resource[IO, BinaryTreeRenderer[IO]] = Renderer
      .of(TestRenderResultProcessor)
      .parallel[IO]
      .build

  }

  test("tree with two documents using a RenderResultProcessor writing to an output stream") {
    TwoPhaseRenderer.renderer.use { r =>
      withByteArrayTextOutput { out =>
        r.from(DocumentTreeRoot(TwoPhaseRenderer.input)).toStream(IO.pure(out)).render
      }
    }.assertEquals(TwoPhaseRenderer.expectedResult)

  }

  // TODO - reactivate codec tests
  //
  // test("document to a java.io.OutputStream, specifying the encoding explicitly") {
  //      withByteArrayTextOutput("ISO-8859-1") { out =>
  //        renderer.from(rootElem).toStream(IO.pure(out))(Codec.ISO8859).render.void
  //      }.assertEquals(expected)
  //    }
  //
  // test("document to a java.io.OutputStream, specifying the encoding implicitly") {
  //      implicit val codec: Codec = Codec.ISO8859
  //      withByteArrayTextOutput("ISO-8859-1") { out =>
  //        renderer.from(rootElem).toStream(IO.pure(out)).render.void
  //      }.assertEquals(expected)
  //    }

  test("tree with two documents using a RenderResultProcessor writing to a file") {
    val res = TwoPhaseRenderer.renderer.use { r =>
      for {
        f   <- newTempFile
        _   <- r.from(DocumentTreeRoot(TwoPhaseRenderer.input)).toFile(f).render
        res <- readFile(f)
      } yield res
    }
    res.assertEquals(TwoPhaseRenderer.expectedResult)
  }

  object FileSystemTest {
    import cats.implicits._

    val input: DocumentTreeRoot = SampleTrees.sixDocuments.build

    def readFiles(base: String): IO[List[String]] = {
      List(
        readFile(base + "/doc-1.txt"),
        readFile(base + "/doc-2.txt"),
        readFile(base + "/tree-1/doc-3.txt"),
        readFile(base + "/tree-1/doc-4.txt"),
        readFile(base + "/tree-2/doc-5.txt"),
        readFile(base + "/tree-2/doc-6.txt")
      ).sequence
    }

  }

  test("render to a directory using the toDirectory method") {
    val renderer = Renderer.of(AST)
      .parallel[IO]
      .build

    val expectedFileContents: List[String] = (1 to 6).map(Results.doc).toList

    val res = for {
      f   <- newTempDirectory
      _   <- renderer.use(_.from(FileSystemTest.input).toDirectory(f).render)
      res <- FileSystemTest.readFiles(f.toString)
    } yield res

    res.assertEquals(expectedFileContents)
  }

  object VersionInfoSetup {

    val htmlRenderer: Resource[IO, TreeRenderer[IO]] = Renderer.of(HTML)
      .parallel[IO]
      .build

    def versions(scannerRoot: Option[String] = None): Versions = Versions(
      Version("0.4.x", "0.4", canonical = true),
      Seq(
        Version("0.3.x", "0.3"),
        Version("0.2.x", "0.2"),
        Version("0.1.x", "0.1", fallbackLink = "toc.html")
      ),
      Seq(Version("0.5.x", "0.5")),
      scannerConfig = scannerRoot.map(VersionScannerConfig.apply(_))
    )

    def versionedInput(scannerRoot: Option[String] = None): DocumentTreeRoot =
      SampleTrees.sixDocuments
        .root.config(_.withValue(versions(scannerRoot)))
        .tree1.config(SampleConfig.versioned(true))
        .tree2.config(SampleConfig.versioned(true))
        .build

    val expectedVersionInfo: String =
      """{
        |  "versions": [
        |    { "displayValue": "0.5.x", "pathSegment": "0.5", "fallbackLink": "/index.html", "canonical": false },
        |    { "displayValue": "0.4.x", "pathSegment": "0.4", "fallbackLink": "/index.html", "canonical": true },
        |    { "displayValue": "0.3.x", "pathSegment": "0.3", "fallbackLink": "/index.html", "canonical": false },
        |    { "displayValue": "0.2.x", "pathSegment": "0.2", "fallbackLink": "/index.html", "canonical": false },
        |    { "displayValue": "0.1.x", "pathSegment": "0.1", "fallbackLink": "/toc.html", "canonical": false }
        |  ],
        |  "linkTargets": [
        |    { "path": "/doc-1.html", "versions": ["0.1","0.3"] },
        |    { "path": "/doc-2.html", "versions": ["0.1","0.2","0.3"] },
        |    { "path": "/tree-1/doc-3.html", "versions": ["0.1","0.2","0.3","0.4"] },
        |    { "path": "/tree-1/doc-4.html", "versions": ["0.1","0.2","0.3","0.4"] },
        |    { "path": "/tree-2/doc-5.html", "versions": ["0.1","0.2","0.3","0.4"] },
        |    { "path": "/tree-2/doc-6.html", "versions": ["0.1","0.2","0.4"] }
        |  ]
        |}""".stripMargin

  }

  test("render versioned documents with an existing versionInfo JSON file") {
    import VersionInfoSetup._

    val existingVersionInfo =
      """{
        |  "versions": [
        |    { "displayValue": "0.5.x", "pathSegment": "0.5", "fallbackLink": "/index.html" },
        |    { "displayValue": "0.4.x", "pathSegment": "0.4", "fallbackLink": "/index.html" },
        |    { "displayValue": "0.3.x", "pathSegment": "0.3", "fallbackLink": "/index.html" },
        |    { "displayValue": "0.2.x", "pathSegment": "0.2", "fallbackLink": "/index.html" },
        |    { "displayValue": "0.1.x", "pathSegment": "0.1", "fallbackLink": "/toc.html" }
        |  ],
        |  "linkTargets": [
        |    { "path": "/doc-1.html", "versions": ["0.1","0.3"] },
        |    { "path": "/doc-2.html", "versions": ["0.1","0.2","0.3"] },
        |    { "path": "/tree-1/doc-3.html", "versions": ["0.1","0.2","0.3","0.4"] },
        |    { "path": "/tree-1/doc-4.html", "versions": ["0.1","0.2","0.3"] },
        |    { "path": "/tree-2/doc-5.html", "versions": ["0.1","0.2","0.3"] },
        |    { "path": "/tree-2/doc-6.html", "versions": ["0.1","0.2"] }
        |  ]
        |}""".stripMargin
    val versionInfoInput    =
      BinaryInput.fromString[IO](existingVersionInfo, VersionInfoGenerator.path)

    htmlRenderer
      .use(
        _
          .from(versionedInput())
          .copying(Seq(versionInfoInput))
          .toOutput(StringTreeOutput)
          .render
      )
      .flatMap(tree =>
        IO.fromEither(
          tree
            .staticDocuments
            .find(_.path == VersionInfoGenerator.path)
            .toRight(new RuntimeException("version info missing"))
        )
      )
      .flatMap(_.input.through(fs2.text.utf8.decode).compile.string)
      .assertEquals(expectedVersionInfo)
  }

  test("directory with existing versioned renderer output") {
    import VersionInfoSetup._

    def mkDirs(dir: FilePath): IO[Unit] =
      List(
        "0.1/tree-1",
        "0.1/tree-2",
        "0.2/tree-1",
        "0.2/tree-2",
        "0.3/tree-1",
        "0.3/tree-2"
      ).traverse { name =>
        Files[IO].createDirectories((dir / name).toFS2Path)
      }.void

    def writeExistingVersionedFiles(root: DocumentTreeRoot, dir: FilePath): IO[Unit] = {
      val paths          = root.tree.allDocuments.map(_.path.withSuffix("html").toString)
      val versionedPaths =
        paths.map("0.1" + _) ++ paths.drop(1).map("0.2" + _) ++ paths.dropRight(1).map("0.3" + _)

      versionedPaths.toList.traverse { path =>
        writeFile(dir / path, "<html></html>")
      }.void
    }

    val expectedFileContents: List[String] = (1 to 6).map(num => s"<p>Text $num</p>").toList

    def readVersionedFiles(base: String): IO[List[String]] = {
      List(
        readFile(base + "/doc-1.html"),
        readFile(base + "/doc-2.html"),
        readFile(base + "/0.4/tree-1/doc-3.html"),
        readFile(base + "/0.4/tree-1/doc-4.html"),
        readFile(base + "/0.4/tree-2/doc-5.html"),
        readFile(base + "/0.4/tree-2/doc-6.html")
      ).sequence
    }

    def readVersionInfo(base: String): IO[String] = readFile(base + "/laika/versionInfo.json")

    val res = for {
      f <- newTempDirectory
      root = versionedInput(Some(f.toString))
      _   <- mkDirs(f)
      _   <- writeExistingVersionedFiles(root, f)
      _   <- htmlRenderer.use(_.from(root).toDirectory(f).render)
      res <- readVersionedFiles(f.toString)
      vi  <- readVersionInfo(f.toString)
    } yield (res, vi)

    res.assertEquals((expectedFileContents, expectedVersionInfo))
  }

  test("directory containing a document with non-ASCII characters") {
    val expected =
      """RootElement - Blocks: 1
        |. Paragraph - Spans: 1
        |. . Text - 'Doc äöü'""".stripMargin

    val input = ASTRenderer.defaultTree(RootElement(p("Doc äöü")))

    val res = for {
      d   <- newTempDirectory
      _   <- ASTRenderer.defaultRenderer.use(
        _.from(ASTRenderer.defaultRoot(input)).toDirectory(d)(Codec.ISO8859).render
      )
      res <- readFile(d / "doc.txt")(Codec.ISO8859)
    } yield res

    res.assertEquals(expected)
  }

}
