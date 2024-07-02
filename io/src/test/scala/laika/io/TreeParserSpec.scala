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

import cats.syntax.all.*
import cats.data.{ Chain, NonEmptyChain }
import cats.effect.{ IO, Resource }
import laika.api.MarkupParser
import laika.api.builder.OperationConfig
import laika.api.bundle.{ BundleOrigin, ExtensionBundle, SpanParserBuilder }
import laika.api.config.ConfigBuilder
import laika.api.errors.{ InvalidDocument, InvalidDocuments }
import laika.ast.DocumentType.*
import laika.ast.Path.Root
import laika.ast.*
import laika.ast.sample.{ ParagraphCompanionShortcuts, SampleTrees, TestSourceBuilders }
import laika.ast.styles.{ StyleDeclaration, StyleDeclarationSet, StylePredicate }
import laika.bundle.*
import laika.config.{ LaikaKeys, MessageFilter, TargetFormats, Version, Versions }
import laika.format.{ HTML, Markdown, ReStructuredText }
import laika.io.api.TreeParser
import laika.io.helper.InputBuilder
import laika.io.internal.errors.{ ConfigException, DuplicatePath, ParserErrors }
import laika.io.syntax.*
import laika.io.model.{ InputTree, InputTreeBuilder, ParsedTree }
import laika.parse.Parser
import laika.parse.text.TextParsers
import laika.theme.Theme
import munit.CatsEffectSuite

class TreeParserSpec
    extends CatsEffectSuite
    with ParagraphCompanionShortcuts
    with IOTreeAssertions
    with TestSourceBuilders
    with InputBuilder
    with ParserSetup {

  object Contents {
    val link  = "[link](http://foo.com)"
    val name  = "foo"
    val name2 = "bar"

    val multiline: String =
      """aaa
        |
        |bbb""".stripMargin

    val directive = "aa @:foo(bar) bb"

    val template: String =
      """<div>
        |  ${cursor.currentDocument.content}
        |</div>""".stripMargin

    val template2: String =
      """<div>
        |xx${cursor.currentDocument.content}
        |</div>""".stripMargin

    val brokenTemplate: String =
      """<div>
        |${cursor.currentDocument.content} @:foo
        |</div>""".stripMargin

    val dynDoc           = "${value}"
    val conf             = "value: abc"
    val titleDocNameConf = "laika.titleDocuments.inputName = alternative-title"

    val order: String =
      """laika.navigationOrder: [
        |  lemon.md
        |  shapes
        |  cherry.md
        |  colors
        |  apple.md
        |  orange.md
        |]""".stripMargin

  }

  private val cssTargetFormats = TargetFormats.Selected("html", "epub", "xhtml")

  val defaultContent: Seq[Paragraph] = Seq(p("foo"))

  def docResult(num: Int, content: Seq[Block] = defaultContent, path: Path = Root): Document =
    Document(path / s"doc-$num.md", RootElement(content))

  def docResult(name: String): Document = Document(Root / name, RootElement(defaultContent))

  def customDocResult(name: String, content: Seq[Block], path: Path = Root): Document =
    Document(path / name, RootElement(content))

  def applyTemplates(parsed: ParsedTree[IO]): DocumentTreeRoot = {
    val rules = OperationConfig.default.rewriteRulesFor(parsed.root, RewritePhase.Render(HTML))
    parsed.root.applyTemplates(rules, OutputContext(HTML)).toOption.get
  }

  def parsedTree(
      inputs: Seq[(Path, String)],
      f: InputTreeBuilder[IO] => InputTreeBuilder[IO] = identity
  ): IO[DocumentTreeRoot] = parsedTree(defaultParser, inputs, f)

  def parsedTree(
      parser: Resource[IO, TreeParser[IO]],
      inputs: Seq[(Path, String)],
      f: InputTreeBuilder[IO] => InputTreeBuilder[IO]
  ): IO[DocumentTreeRoot] = parser
    .use(_.fromInput(f(build(inputs))).parse)
    .map(applyTemplates)

  def mixedParsedTree(inputs: Seq[(Path, String)]): IO[DocumentTreeRoot] = {
    val parser = MarkupParser
      .of(Markdown)
      .parallel[IO]
      .withTheme(Theme.empty)
      .withAlternativeParser(MarkupParser.of(ReStructuredText))
      .build
    parser.use(_.fromInput(build(inputs)).parse).map(_.root)
  }

  def parsedWith(inputs: Seq[(Path, String)], bundle: ExtensionBundle): IO[DocumentTreeRoot] =
    parserWithBundle(bundle)
      .use(_.fromInput(build(inputs)).parse)
      .map(applyTemplates)

  def parsedTemplates(
      inputs: Seq[(Path, String)],
      bundle: ExtensionBundle
  ): IO[Seq[TemplateRoot]] = {
    parserWithBundle(bundle)
      .use(_.fromInput(build(inputs)).parse)
      .flatMap { parsed =>
        val emptyDoc = Document(Root, RootElement.empty)
        val rules    = OperationConfig.default.rewriteRulesFor(emptyDoc, RewritePhase.Render(HTML))
        IO.fromEither(
          rules.map(rules =>
            parsed.root.tree.templates.map { tpl =>
              tpl.content.rewriteChildren(rules)
            }
          ).leftMap(ConfigException.apply)
        )
      }
  }

  test("an empty tree") {
    parsedTree(Nil).assertEquals(DocumentTreeRoot(DocumentTree.empty))
  }

  test("tree with a single document") {
    val inputs     = Seq(
      Root / "name.md" -> Contents.name
    )
    val treeResult = DocumentTree.builder
      .addDocument(Document(Root / "name.md", RootElement(p("foo"))))
      .buildRoot
    parsedTree(inputs).assertEquals(treeResult)
  }

  test("tree with a document containing an unvalidated link to a versioned directory") {
    val inputs      = Seq(
      Root / "doc-1.md" -> "[link](/v0.42/)"
    )
    val path        = (Root / "v0.42" / "doc").parent
    val target      = InternalTarget(path).relativeTo(Root / "doc-1.md")
    val expectedDoc =
      docResult(1, Seq(Paragraph(SpanLink.internal(path)("link").withTarget(target))))
    val treeResult  = DocumentTree.builder
      .addDocument(expectedDoc)
      .buildRoot

    val versions = Versions.forCurrentVersion(Version("0.42", "v0.42"))

    parsedTree(configuredParser(_.withConfigValue(versions)), inputs, identity).assertEquals(
      treeResult
    )
  }

  test("tree with multiple subtrees") {
    val inputs   = Seq(
      Root / "doc-1.md"            -> Contents.name,
      Root / "doc-2.md"            -> Contents.name,
      Root / "tree-1" / "doc-3.md" -> Contents.name,
      Root / "tree-1" / "doc-4.md" -> Contents.name,
      Root / "tree-2" / "doc-5.md" -> Contents.name,
      Root / "tree-2" / "doc-6.md" -> Contents.name
    )
    val expected = SampleTrees.sixDocuments
      .docContent(defaultContent)
      .suffix("md")
      .build
    parsedTree(inputs).assertEquals(expected)
  }

  test("collect errors from multiple documents") {
    val inputs           = Seq(
      Root / "doc-1.md"            -> "[link1]",
      Root / "doc-2.md"            -> "[link2]",
      Root / "tree-1" / "doc-3.md" -> "[link3]",
      Root / "tree-1" / "doc-4.md" -> "[link4]",
      Root / "tree-2" / "doc-5.md" -> "[link5]",
      Root / "tree-2" / "doc-6.md" -> "[link6]"
    )
    val invalidDocuments = inputs.map { case (path, markup) =>
      val msg         = s"unresolved link id reference: link${markup.charAt(5)}"
      val invalidSpan = InvalidSpan(msg, source(markup, markup, path))
      InvalidDocument(path, invalidSpan)
    }
    val expectedError    =
      InvalidDocuments(NonEmptyChain.fromChainUnsafe(Chain.fromSeq(invalidDocuments)))
    val expectedMessage  =
      """/doc-1.md
        |
        |  [1]: unresolved link id reference: link1
        |
        |  [link1]
        |  ^
        |
        |/doc-2.md
        |
        |  [1]: unresolved link id reference: link2
        |
        |  [link2]
        |  ^
        |
        |/tree-1/doc-3.md
        |
        |  [1]: unresolved link id reference: link3
        |
        |  [link3]
        |  ^
        |
        |/tree-1/doc-4.md
        |
        |  [1]: unresolved link id reference: link4
        |
        |  [link4]
        |  ^
        |
        |/tree-2/doc-5.md
        |
        |  [1]: unresolved link id reference: link5
        |
        |  [link5]
        |  ^
        |
        |/tree-2/doc-6.md
        |
        |  [1]: unresolved link id reference: link6
        |
        |  [link6]
        |  ^""".stripMargin
    assertEquals(InvalidDocuments.format(expectedError.documents), expectedMessage)

    parsedTree(inputs).attempt.assertEquals(Left(expectedError))
  }

  test("report errors originating in templates with additional path info") {
    val markup          = "text"
    val docPath         = Root / "doc-1.md"
    val inputs          = Seq(
      docPath                     -> markup,
      DefaultTemplatePath.forHTML -> Contents.brokenTemplate
    )
    val msg             =
      "One or more errors processing directive 'foo': No template directive registered with name: foo"
    val invalidDocument = {
      val invalidSpan2 =
        InvalidSpan(msg, source("@:foo", Contents.brokenTemplate, DefaultTemplatePath.forHTML))
      InvalidDocument(docPath, invalidSpan2)
    }
    val expectedError   = InvalidDocuments(NonEmptyChain(invalidDocument))
    val expectedMessage =
      s"""/doc-1.md
         |
         |  [/default.template.html:2]: $msg
         |
         |  $${cursor.currentDocument.content} @:foo
         |                                    ^""".stripMargin
    assertEquals(InvalidDocuments.format(expectedError.documents), expectedMessage)

    parsedTree(inputs).map(root =>
      InvalidDocuments.from(root, MessageFilter.Error).map(_.documents.head)
    ).assertEquals(Some(invalidDocument))
  }

  test("tree with a cover and a title document") {
    val inputs     = Seq(
      Root / "doc-1.md"  -> Contents.name,
      Root / "doc-2.md"  -> Contents.name,
      Root / "README.md" -> Contents.name,
      Root / "cover.md"  -> Contents.name
    )
    val treeResult =
      DocumentTree.builder
        .addDocument(docResult(1))
        .addDocument(docResult(2))
        .addDocument(docResult("README.md"))
        .addDocument(docResult("cover.md"))
        .buildRoot
    parsedTree(inputs).assertEquals(treeResult)
  }

  test("tree with a title document with a custom document name configuration") {
    val inputs     = Seq(
      Root / "directory.conf"       -> Contents.titleDocNameConf,
      Root / "doc-1.md"             -> Contents.name,
      Root / "doc-2.md"             -> Contents.name,
      Root / "alternative-title.md" -> Contents.name,
      Root / "cover.md"             -> Contents.name
    )
    val treeResult =
      DocumentTree.builder
        .addDocument(docResult(1))
        .addDocument(docResult(2))
        .addDocument(docResult("alternative-title.md"))
        .addDocument(docResult("cover.md"))
        .addConfig(
          ConfigBuilder.empty.withValue(
            LaikaKeys.titleDocuments.inputName,
            "alternative-title"
          ).build
        )
        .buildRoot
    parsedTree(inputs).assertEquals(treeResult)
  }

  test("tree with a single template".ignore) {
    val inputs     = Seq(
      Root / "main.template.html" -> Contents.name
    )
    val template   = TemplateDocument(Root / "main.template.html", TemplateRoot("foo"))
    val treeResult = DocumentTree.builder.addTemplate(template).buildRoot
    parsedTree(inputs).assertEquals(treeResult)
  }

  test("fail with duplicate paths") {
    val inputs = Seq(
      Root / "doc1.md"        -> Contents.name,
      Root / "doc2.md"        -> Contents.name,
      Root / "doc2.md"        -> Contents.name,
      Root / "sub" / "doc.md" -> Contents.name,
      Root / "sub" / "doc.md" -> Contents.name
    )
    defaultParser.use(_.fromInput(build(inputs)).parse).attempt.assertEquals(
      Left(
        ParserErrors(Set(DuplicatePath(Root / "doc2.md"), DuplicatePath(Root / "sub" / "doc.md")))
      )
    )
  }

  test("tree with a static document") {
    val inputs     = Seq(
      Root / "omg.js" -> Contents.name
    )
    val staticDoc  = StaticDocument(Root / "omg.js", cssTargetFormats)
    val treeResult = DocumentTreeRoot(DocumentTree.empty).addStaticDocuments(List(staticDoc))
    parsedTree(inputs).assertEquals(treeResult)
  }

  test("tree with static documents for different target formats") {
    val inputs     = Seq(
      Root / "forHTML" / "foo-1.jpg"            -> Contents.name,
      Root / "forHTML" / "nested" / "foo-2.jpg" -> Contents.name, // inherits config from parent dir
      Root / "forHTML" / "directory.conf"       -> "laika.targetFormats = [html]",
      Root / "forEPUB" / "foo-1.jpg"            -> Contents.name,
      Root / "forEPUB" / "nested" / "foo-2.jpg" -> Contents.name,
      Root / "forEPUB" / "directory.conf"       -> "laika.targetFormats = [epub]",
      Root / "forAll" / "foo-1.jpg"             -> Contents.name,
      Root / "forAll" / "nested" / "foo-2.jpg"  -> Contents.name
    )
    val htmlOnly   = TargetFormats.Selected("html")
    val epubOnly   = TargetFormats.Selected("epub", "xhtml")
    val staticDocs = Seq(
      StaticDocument(Root / "forHTML" / "foo-1.jpg", htmlOnly),
      StaticDocument(Root / "forHTML" / "nested" / "foo-2.jpg", htmlOnly),
      StaticDocument(Root / "forEPUB" / "foo-1.jpg", epubOnly),
      StaticDocument(Root / "forEPUB" / "nested" / "foo-2.jpg", epubOnly),
      StaticDocument(Root / "forAll" / "foo-1.jpg", TargetFormats.All),
      StaticDocument(Root / "forAll" / "nested" / "foo-2.jpg", TargetFormats.All)
    )
    parsedTree(inputs).map(_.staticDocuments).assertEquals(staticDocs)
  }

  test("tree with a provided path") {

    val providedPath = Root / "provided" / "ext.html"

    val inputs         = Seq(
      Root / "doc-1.md" -> "[link](provided/ext.html)"
    )
    val target         = InternalTarget(providedPath).relativeTo(Root / "doc-1.md")
    val expectedDoc    =
      docResult(1, Seq(Paragraph(SpanLink.internal(providedPath)("link").withTarget(target))))
    val expectedResult = DocumentTree.builder
      .addDocument(expectedDoc)
      .buildRoot
      .addStaticDocuments(List(StaticDocument(providedPath)))
    parsedTree(inputs, _.addProvidedPath(providedPath)).assertEquals(expectedResult)
  }

  test("tree with all available file types and multiple markup formats") {
    val inputs = Seq(
      Root / "doc-1.md"                       -> Contents.link,
      Root / "doc-2.rst"                      -> Contents.link,
      Root / "mainA.template.html"            -> Contents.name,
      Root / "tree-1" / "mainB.template.html" -> Contents.name,
      Root / "tree-1" / "doc-3.md"            -> Contents.name,
      Root / "tree-1" / "doc-4.md"            -> Contents.name,
      Root / "tree-2" / "doc-5.md"            -> Contents.name,
      Root / "tree-2" / "doc-6.md"            -> Contents.name,
      Root / "static-1" / "omg.js"            -> Contents.name
    )

    val linkResult = Seq(p(SpanLink.external("http://foo.com")("link")))
    val rstResult  =
      Seq(p(Text("[link]("), SpanLink.external("http://foo.com")("http://foo.com"), Text(")")))

    val expected = SampleTrees.sixDocuments
      .staticDoc(Root / "static-1" / "omg.js", "html", "epub", "xhtml")
      .docContent(defaultContent)
      .suffix("md")
      .doc1.content(linkResult)
      .doc2.content(rstResult)
      .doc2.suffix("rst")
      .root.template("mainA.template.html", TemplateString("foo"))
      .tree1.template("mainB.template.html", TemplateString("foo"))
      .build
    mixedParsedTree(inputs).assertEquals(expected)
  }

  test("custom template engine".ignore) {
    val parser: Parser[TemplateRoot] = TextParsers.anyChars.map { str => TemplateRoot("$$" + str) }
    val inputs                       = Seq(
      Root / "main1.template.html" -> Contents.name,
      Root / "main2.template.html" -> Contents.name
    )

    def template(num: Int) =
      TemplateDocument(Root / s"main$num.template.html", TemplateRoot("$$foo"))

    val treeResult = DocumentTree.builder
      .addTemplate(template(1))
      .addTemplate(template(2))
      .buildRoot
    parsedWith(inputs, BundleProvider.forTemplateParser(parser)).assertEquals(treeResult)
  }

  test("custom style sheet engine") {
    val customDocTypeMatcher: PartialFunction[Path, DocumentType] = { case path =>
      val Stylesheet = """.+\.([a,b]+).css$""".r
      path.name match {
        case Stylesheet(kind) => StyleSheet(kind)
        case _                => Ignored
      }
    }

    def styleDecl(styleName: String, order: Int = 0) =
      StyleDeclaration(StylePredicate.ElementType("Type"), styleName -> "foo").increaseOrderBy(
        order
      )

    val parser: Parser[Set[StyleDeclaration]] = TextParsers.anyChars.map { n => Set(styleDecl(n)) }
    val inputs                                = Seq(
      Root / "main1.aaa.css" -> Contents.name,
      Root / "main2.bbb.css" -> Contents.name2,
      Root / "main3.aaa.css" -> Contents.name
    )

    val treeResult = DocumentTreeRoot(DocumentTree.empty).addStyles(
      Map(
        "aaa" -> StyleDeclarationSet(
          Set(Root / "main1.aaa.css", Root / "main3.aaa.css"),
          Set(styleDecl("foo"), styleDecl("foo", 1))
        ),
        "bbb" -> styles.StyleDeclarationSet(Set(Root / "main2.bbb.css"), Set(styleDecl("bar")))
      )
    )
    parsedWith(
      inputs,
      BundleProvider.forDocTypeMatcher(customDocTypeMatcher)
        .withBase(BundleProvider.forStyleSheetParser(parser))
    ).assertEquals(treeResult)
  }

  test("template directive") {
    import laika.api.bundle.TemplateDirectives
    import TemplateDirectives.dsl._

    val directive = TemplateDirectives.create("foo") {
      attribute(0).as[String] map {
        TemplateString(_)
      }
    }
    val inputs    = Seq(
      Root / "main1.template.html" -> Contents.directive,
      Root / "main2.template.html" -> Contents.directive
    )
    val template = TemplateRoot(TemplateString("aa "), TemplateString("bar"), TemplateString(" bb"))
    val result    = Seq(template, template)
    parsedTemplates(inputs, BundleProvider.forTemplateDirective(directive)).assertEquals(result)
  }

  test("add indentation information if an embedded root is preceded by whitespace characters") {

    import laika.ast.EmbeddedRoot

    val inputs     = Seq(
      DefaultTemplatePath.forHTML -> Contents.template,
      Root / "doc.md"             -> Contents.multiline
    )
    val docResult  = Document(
      Root / "doc.md",
      RootElement(
        TemplateRoot(
          TemplateString("<div>\n  "),
          EmbeddedRoot(List(p("aaa"), p("bbb")), 2),
          TemplateString("\n</div>")
        )
      )
    )
    val treeResult = DocumentTree.builder.addDocument(docResult).buildRoot
    parsedTree(inputs).assertEquals(treeResult)
  }

  test(
    "do not add indentation information if an embedded root is preceded by non-whitespace characters"
  ) {

    import laika.ast.EmbeddedRoot

    val inputs     = Seq(
      DefaultTemplatePath.forHTML -> Contents.template2,
      Root / "doc.md"             -> Contents.multiline
    )
    val docResult  = Document(
      Root / "doc.md",
      RootElement(
        TemplateRoot(
          TemplateString("<div>\nxx"),
          EmbeddedRoot(p("aaa"), p("bbb")),
          TemplateString("\n</div>")
        )
      )
    )
    val treeResult = DocumentTree.builder.addDocument(docResult).buildRoot
    parsedTree(inputs).assertEquals(treeResult)
  }

  test("custom navigation order") {
    val inputs = Seq(
      Root / "apple.md"             -> Contents.name,
      Root / "orange.md"            -> Contents.name,
      Root / "colors" / "green.md"  -> Contents.name,
      Root / "lemon.md"             -> Contents.name,
      Root / "shapes" / "circle.md" -> Contents.name,
      Root / "cherry.md"            -> Contents.name,
      Root / "directory.conf"       -> Contents.order
    )
    defaultParser.use(_.fromInput(build(inputs)).parse).map {
      _.root.tree.content map (_.path.name)
    }.assertEquals(List("lemon.md", "shapes", "cherry.md", "colors", "apple.md", "orange.md"))
  }

  test("always move title documents to the front, even with a custom navigation order") {
    val inputs = Seq(
      Root / "apple.md"             -> Contents.name,
      Root / "orange.md"            -> Contents.name,
      Root / "colors" / "green.md"  -> Contents.name,
      Root / "lemon.md"             -> Contents.name,
      Root / "README.md"            -> Contents.name,
      Root / "shapes" / "circle.md" -> Contents.name,
      Root / "cherry.md"            -> Contents.name,
      Root / "directory.conf"       -> Contents.order
    )
    defaultParser.use(_.fromInput(build(inputs)).parse).map(_.root.tree).map { tree =>
      assertEquals(tree.titleDocument.map(_.path.basename), Some("README"))
      assertEquals(
        tree.content map (_.path.name),
        List("lemon.md", "shapes", "cherry.md", "colors", "apple.md", "orange.md")
      )
      assertEquals(
        tree.content map (_.position),
        List(
          TreePosition(Seq(1)),
          TreePosition(Seq(2)),
          TreePosition(Seq(3)),
          TreePosition(Seq(4)),
          TreePosition(Seq(5)),
          TreePosition(Seq(6))
        )
      )
    }
  }

  object CustomSpanParsers {

    import TextParsers._
    import laika.parse.syntax._

    case class DecoratedSpan(deco: Char, text: String) extends Span {
      val options: Options = Options.empty
      type Self = DecoratedSpan
      def withOptions(options: Options): DecoratedSpan = this
    }

    def spanFor(deco: Char): SpanParserBuilder = spanFor(deco, deco)

    def spanFor(deco: Char, overrideDeco: Char): SpanParserBuilder =
      SpanParserBuilder.standalone {
        (deco.toString ~> anyNot(' ')).map(DecoratedSpan(overrideDeco, _))
      }

    val input: InputTreeBuilder[IO] = InputTree[IO].addString("aaa +bbb ccc", Root / "doc.md")

    def parse(
        themeParsers: Seq[SpanParserBuilder] = Nil,
        appParsers: Seq[SpanParserBuilder] = Nil
    ): IO[RootElement] =
      parserWithThemeAndBundle(
        BundleProvider.forMarkupParser(spanParsers = themeParsers, origin = BundleOrigin.Theme),
        BundleProvider.forMarkupParser(spanParsers = appParsers)
      )
        .use(_.fromInput(input).parse)
        .map(_.root.allDocuments.head.content)

    def parseWithThemeExtension(
        themeParsers: Seq[SpanParserBuilder] = Nil,
        themeExtensionParsers: Seq[SpanParserBuilder] = Nil
    ): IO[RootElement] =
      parserWithThemeExtension(
        BundleProvider.forMarkupParser(spanParsers = themeParsers, origin = BundleOrigin.Theme),
        BundleProvider.forMarkupParser(spanParsers = themeExtensionParsers)
      )
        .use(_.fromInput(input).parse)
        .map(_.root.allDocuments.head.content)

  }

  test("use a span parser from a theme") {
    import CustomSpanParsers._

    val themeParsers = Seq(spanFor('+'))

    parse(themeParsers).assertEquals(
      RootElement(
        Paragraph(
          Text("aaa "),
          DecoratedSpan('+', "bbb"),
          Text(" ccc")
        )
      )
    )
  }

  test("let a span parser from an app extension override a span parser from a theme") {
    import CustomSpanParsers._

    val themeParsers = Seq(spanFor('+'))
    val appParsers   = Seq(spanFor('+', '!'))

    parse(themeParsers, appParsers).assertEquals(
      RootElement(
        Paragraph(
          Text("aaa "),
          DecoratedSpan('!', "bbb"),
          Text(" ccc")
        )
      )
    )
  }

  test("let a span parser from a theme extension override a span parser from a base theme") {
    import CustomSpanParsers._

    val themeParsers    = Seq(spanFor('+'))
    val themeExtParsers = Seq(spanFor('+', '!'))

    parseWithThemeExtension(themeParsers, themeExtParsers).assertEquals(
      RootElement(
        Paragraph(
          Text("aaa "),
          DecoratedSpan('!', "bbb"),
          Text(" ccc")
        )
      )
    )
  }

  // TODO - reactivate these tests for the removed sequential parser
  //
  //   "parse Markdown from an empty file" in {
  //      val filename = getClass.getResource("/emptyInput.md").getFile
  //      parser.fromFile(filename).parse.map(_.content).assertEquals(root())
  //    }
  //
  //    "parse Markdown from a java.io.InputStream instance, specifying the encoding explicitly" in {
  //      val input = """äää
  //        |ööö
  //        |üüü""".stripMargin
  //      val stream = IO(new ByteArrayInputStream(input.getBytes("ISO-8859-1")))
  //      parser.fromStream(stream)(Codec.ISO8859).parse.map(_.content).assertEquals(root(p(input)))
  //    }
  //
  //    "parse Markdown from a java.io.InputStream instance, specifying the encoding implicitly" in {
  //      val input = """äää
  //        |ööö
  //        |üüü""".stripMargin
  //      val stream = IO(new ByteArrayInputStream(input.getBytes("ISO-8859-1")))
  //      implicit val codec:Codec = Codec.ISO8859
  //      parser.fromStream(stream).parse.map(_.content).assertEquals(root(p(input)))
  //    }

}
