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

import java.io.ByteArrayInputStream

import cats.data.{Chain, NonEmptyChain}
import cats.effect.{IO, Resource, Sync}
import laika.api.MarkupParser
import laika.ast.DocumentType._
import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentTree
import laika.ast.sample.{DocumentTreeAssertions, ParagraphCompanionShortcuts, SampleSixDocuments, SampleTrees, TestSourceBuilders}
import laika.ast.{StylePredicate, _}
import laika.bundle._
import laika.config.Origin.TreeScope
import laika.config.{ConfigBuilder, Origin}
import laika.format.{Markdown, ReStructuredText}
import laika.io.api.TreeParser
import laika.io.helper.{InputBuilder, TestThemeBuilder}
import laika.io.implicits._
import laika.io.model.{InputTree, InputTreeBuilder, ParsedTree}
import laika.io.runtime.ParserRuntime.{DuplicatePath, ParserErrors}
import laika.parse.Parser
import laika.parse.markup.DocumentParser.{InvalidDocument, InvalidDocuments}
import laika.parse.text.TextParsers
import laika.rewrite.nav.TargetFormats
import laika.rewrite.{DefaultTemplatePath, TemplateContext, TemplateRewriter}
import laika.theme.Theme


class TreeParserSpec extends IOWordSpec 
  with ParagraphCompanionShortcuts
  with FileIO
  with DocumentTreeAssertions
  with TestSourceBuilders {

  trait ParserSetup {

    val defaultBuilder: TreeParser.Builder[IO] = MarkupParser
      .of(Markdown)
      .parallel[IO]
      .withTheme(Theme.empty)
    
    val defaultParser: Resource[IO, TreeParser[IO]] = defaultBuilder.build
    
    def parserWithBundle (bundle: ExtensionBundle): Resource[IO, TreeParser[IO]] = 
      MarkupParser
        .of(Markdown)
        .using(bundle)
        .parallel[IO]
        .withTheme(Theme.empty)
        .build

    def parserWithTheme (bundle: ExtensionBundle): Resource[IO, TreeParser[IO]] =
      MarkupParser
        .of(Markdown)
        .parallel[IO]
        .withTheme(TestThemeBuilder.forBundle(bundle))
        .build

    def parserWithThemeAndBundle (themeBundle: ExtensionBundle, appBundle: ExtensionBundle): Resource[IO, TreeParser[IO]] =
      MarkupParser
        .of(Markdown)
        .using(appBundle)
        .parallel[IO]
        .withTheme(TestThemeBuilder.forBundle(themeBundle))
        .build
  }
  
  trait TreeParserSetup extends InputBuilder with ParserSetup {
    
    def inputs: Seq[(Path, String)] 
    
    object Contents {
      val link = "[link](/foo)"
      val name = "foo"
      val name2 = "bar"
      val multiline = """aaa
                       |
                       |bbb""".stripMargin
      val directive = "aa @:foo(bar) bb"
      val template = """<div>
                      |  ${cursor.currentDocument.content}
                      |</div>""".stripMargin
      val template2 = """<div>
                       |xx${cursor.currentDocument.content}
                       |</div>""".stripMargin
      val dynDoc = "${value}"
      val conf = "value: abc"
      val titleDocNameConf = "laika.titleDocuments.inputName = alternative-title"
      val order = """laika.navigationOrder: [
        |  lemon.md
        |  shapes
        |  cherry.md
        |  colors
        |  apple.md
        |  orange.md
        |]""".stripMargin
    }
    
    val docTypeMatcher: Path => DocumentType = MarkupParser.of(Markdown).build.config.docTypeMatcher
    
    val defaultContent = Seq(p("foo"))
    
    def docResult (num: Int, path: Path = Root) = Document(path / s"doc-$num.md", RootElement(defaultContent))
    def docResult(name: String)                 = Document(Root / name, RootElement(defaultContent))
    def customDocResult(name: String, content: Seq[Block], path: Path = Root) = Document(path / name, RootElement(content))

    def applyTemplates (parsed: ParsedTree[IO]): DocumentTreeRoot = TemplateRewriter.applyTemplates(parsed.root, TemplateContext("html")).toOption.get
    def parsedTree: IO[DocumentTreeRoot] = defaultParser.use(_.fromInput(build(inputs)).parse).map(applyTemplates)
    
    def mixedParsedTree: IO[DocumentTreeRoot] = {
      val parser = MarkupParser
        .of(Markdown)
        .parallel[IO]
        .withTheme(Theme.empty)
        .withAlternativeParser(MarkupParser.of(ReStructuredText))
        .build
      parser.use(_.fromInput(build(inputs)).parse).map(_.root)
    }
    
    def parsedWith (bundle: ExtensionBundle): IO[DocumentTreeRoot] =
      parserWithBundle(bundle)
        .use(_.fromInput(build(inputs)).parse)
        .map(applyTemplates)

    def parsedTemplates (bundle: ExtensionBundle): IO[Seq[TemplateRoot]] = {
      parserWithBundle(bundle)
        .use(_.fromInput(build(inputs)).parse)
        .map { parsed =>
          parsed.root.tree.templates.map { tpl =>
            tpl.content.rewriteChildren(TemplateRewriter.rewriteRules(DocumentCursor(Document(Root, RootElement.empty))))
          }
        }
    }
      
  }
  

  
  "The tree parser" should {

    "parse an empty tree" in new TreeParserSetup {
      val inputs = Nil
      parsedTree.assertEquals(DocumentTreeRoot(DocumentTree(Root, Nil)))
    }

    "parse a tree with a single document" in new TreeParserSetup {
      val inputs = Seq(
        Root / "name.md" -> Contents.name
      )
      val docResult = Document(Root / "name.md", RootElement(p("foo")))
      val treeResult = DocumentTreeRoot(DocumentTree(Root, List(docResult)))
      parsedTree.assertEquals(treeResult)
    }

    "parse a tree with multiple subtrees" in new TreeParserSetup {
      val inputs = Seq(
        Root / "doc-1.md" -> Contents.name,
        Root / "doc-2.md" -> Contents.name,
        Root / "tree-1" / "doc-3.md" -> Contents.name,
        Root / "tree-1" / "doc-4.md" -> Contents.name,
        Root / "tree-2" / "doc-5.md" -> Contents.name,
        Root / "tree-2" / "doc-6.md" -> Contents.name
      )
      val expected = SampleTrees.sixDocuments
        .docContent(defaultContent)
        .suffix("md")
        .build
      parsedTree.assertEquals(expected)
    }
    
    "collect errors from multiple documents" in new TreeParserSetup {
      val inputs = Seq(
        Root / "doc-1.md" -> "[link1]",
        Root / "doc-2.md" -> "[link2]",
        Root / "tree-1" / "doc-3.md" -> "[link3]",
        Root / "tree-1" / "doc-4.md" -> "[link4]",
        Root / "tree-2" / "doc-5.md" -> "[link5]",
        Root / "tree-2" / "doc-6.md" -> "[link6]"
      )
      val invalidDocuments = inputs.map { case (path, markup) => 
        val msg = s"unresolved link id reference: link${markup.charAt(5)}"
        val invalidSpan = InvalidSpan(msg, generatedSource(markup))
        InvalidDocument(NonEmptyChain.one(invalidSpan), path)
      }
      val expectedError = InvalidDocuments(NonEmptyChain.fromChainUnsafe(Chain.fromSeq(invalidDocuments)))
      val expectedMessage =
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
      InvalidDocuments.format(expectedError.documents) shouldBe expectedMessage
      
      parsedTree.assertFailsWith(expectedError)
    }

    "parse a tree with a cover and a title document" in new TreeParserSetup {
      val inputs = Seq(
        Root / "doc-1.md" -> Contents.name,
        Root / "doc-2.md" -> Contents.name,
        Root / "README.md" -> Contents.name,
        Root / "cover.md" -> Contents.name
      )
      val treeResult = DocumentTreeRoot(
        DocumentTree(Root, List(docResult(1), docResult(2)), titleDocument = Some(docResult("README.md"))),
        coverDocument = Some(docResult("cover.md")),
      )
      parsedTree.assertEquals(treeResult)
    }

    "parse a tree with a title document with a custom document name configuration" in new TreeParserSetup {
      val inputs = Seq(
        Root / "directory.conf" -> Contents.titleDocNameConf,
        Root / "doc-1.md" -> Contents.name,
        Root / "doc-2.md" -> Contents.name,
        Root / "alternative-title.md" -> Contents.name,
        Root / "cover.md" -> Contents.name
      )
      val treeResult = DocumentTreeRoot(
        DocumentTree(Root, List(docResult(1), docResult(2)), titleDocument = Some(docResult("alternative-title.md"))),
        coverDocument = Some(docResult("cover.md")),
      )
      parsedTree.assertEquals(treeResult)
    }

    "parse a tree with a single template" ignore new TreeParserSetup {
      val inputs = Seq(
        Root / "main.template.html" -> Contents.name
      )
      val template = TemplateDocument(Root / "main.template.html", TemplateRoot("foo"))
      val treeResult = DocumentTreeRoot(DocumentTree(Root, Nil, templates = List(template)))
      parsedTree.assertEquals(treeResult)
    }

    "fail with duplicate paths" in new TreeParserSetup {
      val inputs = Seq(
        Root / "doc1.md" -> Contents.name,
        Root / "doc2.md" -> Contents.name,
        Root / "doc2.md" -> Contents.name,
        Root / "sub" / "doc.md" -> Contents.name,
        Root / "sub" / "doc.md" -> Contents.name
      )
      defaultParser.use(_.fromInput(build(inputs)).parse).attempt.assertEquals(Left(
        ParserErrors(Set(DuplicatePath(Root / "doc2.md"), DuplicatePath(Root / "sub" / "doc.md")))
      ))
    }

    "parse a tree with a static document" in new TreeParserSetup {
      val inputs = Seq(
        Root / "omg.js" -> Contents.name
      )
      val staticDoc = StaticDocument(Root / "omg.js", TargetFormats.Selected("html"))
      val treeResult = DocumentTreeRoot(DocumentTree(Root, Nil), staticDocuments = List(staticDoc))
      parsedTree.assertEquals(treeResult)
    }

    "parse a tree with all available file types and multiple markup formats" in new TreeParserSetup {
      val inputs = Seq(
        Root / "doc-1.md" -> Contents.link,
        Root / "doc-2.rst" -> Contents.link,
        Root / "mainA.template.html" -> Contents.name,
        Root / "tree-1" / "mainB.template.html" -> Contents.name,
        Root / "tree-1" / "doc-3.md" -> Contents.name,
        Root / "tree-1" / "doc-4.md" -> Contents.name,
        Root / "tree-2" / "doc-5.md" -> Contents.name,
        Root / "tree-2" / "doc-6.md" -> Contents.name,
        Root / "static-1" / "omg.js" -> Contents.name,
      )
      
      val linkResult = Seq(p(SpanLink.external("/foo")("link")))
      val rstResult = Seq(p("[link](/foo)"))
      
      val expected = SampleTrees.sixDocuments
        .staticDoc(Root / "static-1" / "omg.js", "html")
        .docContent(defaultContent)
        .suffix("md")
        .doc1.content(linkResult)
        .doc2.content(rstResult)
        .doc2.suffix("rst")
        .root.template("mainA.template.html", TemplateString("foo"))
        .tree1.template("mainB.template.html", TemplateString("foo"))
        .build
      mixedParsedTree.assertEquals(expected)
    }

    "allow to specify a custom template engine" ignore new TreeParserSetup {
      val parser: Parser[TemplateRoot] = TextParsers.anyChars.map { str => TemplateRoot("$$" + str) }
      val inputs = Seq(
        Root / "main1.template.html" -> Contents.name,
        Root / "main2.template.html" -> Contents.name
      )

      def template (num: Int) = TemplateDocument(Root / s"main$num.template.html", TemplateRoot("$$foo"))

      val treeResult = DocumentTreeRoot(DocumentTree(Root, Nil, templates = List(template(1), template(2))))
      parsedWith(BundleProvider.forTemplateParser(parser)).assertEquals(treeResult)
    }

    "allow to specify a custom style sheet engine" in new TreeParserSetup {
      override val docTypeMatcher: PartialFunction[Path, DocumentType] = {
        case path =>
          val Stylesheet = """.+\.([a,b]+).css$""".r
          path.name match {
            case Stylesheet(kind) => StyleSheet(kind)
          }
      }

      def styleDecl (styleName: String, order: Int = 0) =
        StyleDeclaration(StylePredicate.ElementType("Type"), styleName -> "foo").increaseOrderBy(order)

      val parser: Parser[Set[StyleDeclaration]] = TextParsers.anyChars.map { n => Set(styleDecl(n)) }
      val inputs = Seq(
        Root / "main1.aaa.css" -> Contents.name,
        Root / "main2.bbb.css" -> Contents.name2,
        Root / "main3.aaa.css" -> Contents.name
      )
      val treeResult = DocumentTreeRoot(DocumentTree(Root, Nil), styles = Map(
        "aaa" -> StyleDeclarationSet(Set(Root / "main1.aaa.css", Root / "main3.aaa.css"), Set(styleDecl("foo"), styleDecl("foo", 1))),
        "bbb" -> StyleDeclarationSet(Set(Root / "main2.bbb.css"), Set(styleDecl("bar")))
      ))
      parsedWith(BundleProvider.forDocTypeMatcher(docTypeMatcher)
        .withBase(BundleProvider.forStyleSheetParser(parser))).assertEquals(treeResult)
    }

    "allow to specify a template directive" in new TreeParserSetup {

      import laika.directive.Templates
      import Templates.dsl._

      val directive = Templates.create("foo") {
        attribute(0).as[String] map {
          TemplateString(_)
        }
      }
      val inputs = Seq(
        Root / "main1.template.html" -> Contents.directive,
        Root / "main2.template.html" -> Contents.directive
      )
      val template = TemplateRoot(TemplateString("aa "), TemplateString("bar"), TemplateString(" bb"))
      val result = Seq(template, template)
      parsedTemplates(BundleProvider.forTemplateDirective(directive)).assertEquals(result)
    }

    "add indentation information if an embedded root is preceded by whitespace characters" in new TreeParserSetup {

      import laika.ast.EmbeddedRoot

      val inputs = Seq(
        DefaultTemplatePath.forHTML -> Contents.template,
        Root / "doc.md" -> Contents.multiline
      )
      val docResult = Document(Root / "doc.md", RootElement(TemplateRoot(
        TemplateString("<div>\n  "),
        EmbeddedRoot(List(p("aaa"), p("bbb")), 2),
        TemplateString("\n</div>")
      )))
      val treeResult = DocumentTreeRoot(DocumentTree(Root, List(docResult)))
      parsedTree.assertEquals(treeResult)
    }

    "not add indentation information if an embedded root is preceded by non-whitespace characters" in new TreeParserSetup {

      import laika.ast.EmbeddedRoot

      val inputs = Seq(
        DefaultTemplatePath.forHTML -> Contents.template2,
        Root / "doc.md" -> Contents.multiline
      )
      val docResult = Document(Root / "doc.md", RootElement(TemplateRoot(
        TemplateString("<div>\nxx"),
        EmbeddedRoot(p("aaa"), p("bbb")),
        TemplateString("\n</div>")
      )))
      val treeResult = DocumentTreeRoot(DocumentTree(Root, List(docResult)))
      parsedTree.assertEquals(treeResult)
    }

    "allow to specify a custom navigation order" in new TreeParserSetup {
      val inputs = Seq(
        Root / "apple.md" -> Contents.name,
        Root / "orange.md" -> Contents.name,
        Root / "colors" / "green.md" -> Contents.name,
        Root / "lemon.md" -> Contents.name,
        Root / "shapes" / "circle.md" -> Contents.name,
        Root / "cherry.md" -> Contents.name,
        Root / "directory.conf" -> Contents.order,
      )
      defaultParser.use(_.fromInput(build(inputs)).parse).map {
        _.root.tree.content map (_.path.name)
      }.assertEquals(List("lemon.md", "shapes", "cherry.md", "colors", "apple.md", "orange.md"))
    }

    "always move title documents to the front, even with a custom navigation order" in new TreeParserSetup {
      val inputs = Seq(
        Root / "apple.md" -> Contents.name,
        Root / "orange.md" -> Contents.name,
        Root / "colors" / "green.md" -> Contents.name,
        Root / "lemon.md" -> Contents.name,
        Root / "README.md" -> Contents.name,
        Root / "shapes" / "circle.md" -> Contents.name,
        Root / "cherry.md" -> Contents.name,
        Root / "directory.conf" -> Contents.order,
      )
      defaultParser.use(_.fromInput(build(inputs)).parse).map(_.root.tree).asserting { tree =>
        tree.titleDocument.map(_.path.basename) shouldBe Some("README")
        tree.content map (_.path.name) should be(List("lemon.md", "shapes", "cherry.md", "colors", "apple.md", "orange.md"))
        tree.content map (_.position) should be(List(
          TreePosition(Seq(1)),
          TreePosition(Seq(2)),
          TreePosition(Seq(3)),
          TreePosition(Seq(4)),
          TreePosition(Seq(5)),
          TreePosition(Seq(6)),
        ))
      }
    }

    "read a directory from the file system using the fromDirectory method" in new ParserSetup {
      val dirname: String = getClass.getResource("/trees/a/").getFile

      val expected = SampleTrees.sixDocuments
        .docContent(key => Seq(p("Doc" + key.num)))
        .suffix("md")
        .build
      
      defaultParser.use(_.fromDirectory(dirname).parse).map(_.root).assertEquals(expected)
    }

    trait SpanParserSetup extends ParserSetup {

      import TextParsers._
      import laika.parse.implicits._
      
      def themeParsers: Seq[SpanParserBuilder] = Nil
      def appParsers: Seq[SpanParserBuilder] = Nil

      case class DecoratedSpan (deco: Char, text: String) extends Span {
        val options: Options = NoOpt
        type Self = DecoratedSpan
        def withOptions (options: Options): DecoratedSpan = this
      }

      def spanFor (deco: Char): SpanParserBuilder = spanFor(deco, deco)

      def spanFor (deco: Char, overrideDeco: Char): SpanParserBuilder =
        SpanParser.standalone {
          (deco.toString ~> anyNot(' ')).map(DecoratedSpan(overrideDeco, _))
        }
      
      val input: InputTreeBuilder[IO] = InputTree[IO].addString("aaa +bbb ccc", Root / "doc.md")
      
      def parse: IO[RootElement] = parserWithThemeAndBundle(
        BundleProvider.forMarkupParser(spanParsers = themeParsers, origin = BundleOrigin.Theme),
        BundleProvider.forMarkupParser(spanParsers = appParsers)
      ).use(_.fromInput(input).parse).map(_.root.allDocuments.head.content)
    }

    "use a span parser from a theme" in new SpanParserSetup {
      override def themeParsers: Seq[SpanParserBuilder] = Seq(spanFor('+'))

      parse.assertEquals(RootElement(Paragraph(
        Text("aaa "),
        DecoratedSpan('+', "bbb"),
        Text(" ccc")
      )))
    }

    "let a span parser from an app extension override a span parser from a theme" in new SpanParserSetup {
      override def themeParsers: Seq[SpanParserBuilder] = Seq(spanFor('+'))
      override def appParsers: Seq[SpanParserBuilder] = Seq(spanFor('+', '!'))

      parse.assertEquals(RootElement(Paragraph(
        Text("aaa "),
        DecoratedSpan('!', "bbb"),
        Text(" ccc")
      )))
    }

    trait CustomInputSetup extends ParserSetup {

      def customizeResult (sample: SampleSixDocuments): SampleSixDocuments = sample

      def customizeTree (sample: DocumentTreeRoot): DocumentTreeRoot = sample
      
      lazy val expected = customizeTree(SampleTrees.sixDocuments
        .docContent(key => Seq(p("Doc" + key.num)))
        .suffix("md")
        .apply(customizeResult)
        .build)
      
      val dirname: String = getClass.getResource("/trees/a/").getFile

      def input: InputTreeBuilder[IO]
      def parser: Resource[IO, TreeParser[IO]]
      
      def result: IO[DocumentTreeRoot] = parser.use(_.fromInput(input).parse).map(_.root)
      def run (): Unit = result.assertEquals(expected)
    }

    trait CustomInput extends CustomInputSetup {
      lazy val input: InputTreeBuilder[IO] = addDoc(InputTree[IO].addDirectory(dirname))
      lazy val parser: Resource[IO, TreeParser[IO]] = defaultBuilder.build
      def addDoc (input: InputTreeBuilder[IO]): InputTreeBuilder[IO]
    }
    
    trait CustomTheme extends CustomInputSetup {
      lazy val input: InputTreeBuilder[IO] = InputTree[IO].addDirectory(dirname)
      val themInputs = new TestThemeBuilder.Inputs {
        def build[F[_]: Sync] = addDoc(InputTree[F])
      }
      lazy val parser: Resource[IO, TreeParser[IO]] = defaultBuilder.withTheme(TestThemeBuilder.forInputs(themInputs)).build
      def addDoc[F[_]: Sync] (input: InputTreeBuilder[F]): InputTreeBuilder[F]
    }
    
    trait ExtraDocSetup extends CustomInputSetup {
      val extraPath = Root / "tree-2" / "doc-7.md"
      val extraDoc = Document(extraPath, RootElement(p("Doc7")))
      override def customizeTree (sample: DocumentTreeRoot): DocumentTreeRoot = sample.copy(
        tree = sample.tree.copy(
          content = sample.tree.content.map {
            case tree: DocumentTree if tree.path.name == "tree-2" => tree.copy(content = tree.content :+ extraDoc)
            case other => other
          }
        )
      )
    }

    trait ExtraTemplateSetup extends CustomInputSetup {
      val templatePath: Path = Root / "tree-2" / "tmpl.template.html"
      override def customizeResult(sample: SampleSixDocuments): SampleSixDocuments = 
        sample.tree2.template(templatePath.name, TemplateString("Template"))
    }

    trait ExtraConfigSetup extends CustomInputSetup {
      val configPath: Path = Root / "tree-2" / "directory.conf"
      
      override def run(): Unit = result.asserting { root =>
        root.assertEquals(expected)
        
        root.tree.selectSubtree(CurrentTree / "tree-2").flatMap(_.config.get[Int]("foo").toOption) shouldBe Some(7) 
      }
    }

    "read a directory from the file system plus one AST input" in new ExtraDocSetup with CustomInput {
      def addDoc (input: InputTreeBuilder[IO]): InputTreeBuilder[IO] = 
        input.addDocument(Document(extraPath, RootElement(Paragraph("Doc7"))))
      run()
    }

    "read a directory from the file system plus one AST input from a theme" in new ExtraDocSetup with CustomTheme {
      def addDoc[F[_]: Sync] (input: InputTreeBuilder[F]): InputTreeBuilder[F] =
        input.addDocument(Document(extraPath, RootElement(Paragraph("Doc7"))))
      run()
    }

    "read a directory from the file system plus one string input" in new ExtraDocSetup with CustomInput {
      def addDoc (input: InputTreeBuilder[IO]): InputTreeBuilder[IO] = input.addString("Doc7", extraPath)
      run()
    }

    "read a directory from the file system plus one document from an input stream" in new ExtraDocSetup with CustomInput {
      def addDoc (input: InputTreeBuilder[IO]): InputTreeBuilder[IO] = input.addStream(new ByteArrayInputStream("Doc7".getBytes), extraPath)
      run()
    }

    "read a directory from the file system plus one extra file" in new ExtraDocSetup with CustomInput {
      lazy val filename: String = getClass.getResource("/trees/d/doc-7.md").getFile
      def addDoc (input: InputTreeBuilder[IO]): InputTreeBuilder[IO] = input.addFile(filename, extraPath)
      run()
    }

    "read a directory from the file system plus one extra template from a string" in new ExtraTemplateSetup with CustomInput {
      def addDoc (input: InputTreeBuilder[IO]): InputTreeBuilder[IO] = input.addString("Template", templatePath)
      run()
    }

    "read a directory from the file system plus one extra template from a string in a theme" in new ExtraTemplateSetup with CustomTheme {
      def addDoc[F[_]: Sync] (input: InputTreeBuilder[F]): InputTreeBuilder[F] = input.addString("Template", templatePath)
      run()
    }

    "read a directory from the file system plus one extra template from an AST" in new ExtraTemplateSetup with CustomInput {
      def addDoc (input: InputTreeBuilder[IO]): InputTreeBuilder[IO] = 
        input.addTemplate(TemplateDocument(templatePath, TemplateRoot(TemplateString("Template"))))
      run()
    }

    "read a directory from the file system plus one extra config document from a string" in new ExtraConfigSetup with CustomInput {
      def addDoc (input: InputTreeBuilder[IO]): InputTreeBuilder[IO] = input.addString("foo = 7", configPath)
      run()
    }

    "read a directory from the file system plus one extra config document from a string in a theme" in new ExtraConfigSetup with CustomTheme {
      def addDoc[F[_]: Sync] (input: InputTreeBuilder[F]): InputTreeBuilder[F] = input.addString("foo = 7", configPath)
      run()
    }

    "read a directory from the file system plus one extra config document built programmatically" in new ExtraConfigSetup with CustomInput {
      def addDoc (input: InputTreeBuilder[IO]): InputTreeBuilder[IO] = 
        input.addConfig(ConfigBuilder.withOrigin(Origin(TreeScope, configPath)).withValue("foo", 7).build, configPath)
      run()
    }

    "read a directory from the file system using a custom document type matcher" in new ParserSetup {
      val dirname: String = getClass.getResource("/trees/a/").getFile

      val baseTree = SampleTrees.sixDocuments
        .docContent(key => Seq(p("Doc" + key.num)))
        .suffix("md")
        .build
      
      val expected = baseTree.copy(
        tree = baseTree.tree.copy(
          content = baseTree.tree.content.filter(_.path.name != "doc-1.md")
        )
      )
      
      val parser = parserWithBundle(BundleProvider.forDocTypeMatcher { case Root / "doc-1.md" => Ignored })
      parser
        .use(_.fromDirectory(dirname).parse)
        .map(_.root)
        .assertEquals(expected)
    }

    "allow to specify a custom exclude filter" in new ParserSetup {
      val dirname: String = getClass.getResource("/trees/a/").getFile

      val baseTree = SampleTrees.sixDocuments
        .docContent(key => Seq(p("Doc" + key.num)))
        .suffix("md")
        .build

      val expected = baseTree.copy(
        tree = baseTree.tree.copy(
          content = baseTree.tree.content.filter(c => c.path.name != "doc-1.md" && c.path.name != "tree-1")
        )
      )
      
      defaultParser
        .use(_.fromDirectory(dirname, { f: java.io.File => f.getName == "doc-1.md" || f.getName == "tree-1" }).parse)
        .map(_.root)
        .assertEquals(expected)
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
    
    trait MergedDirectorySetup extends ParserSetup {
      val dir1 = new java.io.File(getClass.getResource("/trees/a/").getFile)
      val dir2 = new java.io.File(getClass.getResource("/trees/b/").getFile)

      val baseTree = SampleTrees.sixDocuments
        .docContent(key => Seq(p("Doc" + key.num)))
        .suffix("md")
        .build
    }
    
    trait TopLevelMergeSetup extends MergedDirectorySetup {
      val doc7 = Document(Root / "tree-2" / "doc-7.md", RootElement("Doc7"))
      val doc8 = Document(Root / "tree-3" / "doc-8.md", RootElement("Doc8"))
      val doc9 = Document(Root / "doc-9.md", RootElement("Doc9"))

      val tree2 = baseTree.tree.content(3).asInstanceOf[DocumentTree]
      
      val expected = baseTree.copy(
        tree = baseTree.tree.copy(
          content =
            baseTree.tree.content.take(2) :+
              doc9 :+
              baseTree.tree.content(2) :+
              tree2.copy(content = tree2.content :+ doc7) :+
              DocumentTree(Root / "tree-3", Seq(doc8))
        )
      )
    }

    "merge two directories from the file system using the fromDirectories method" in new TopLevelMergeSetup {
      defaultParser.use(_.fromDirectories(Seq(dir1, dir2)).parse).map(_.root).assertEquals(expected)
    }

    "merge two directories from the file system using an InputTreeBuilder" in new TopLevelMergeSetup {
      val treeInput = InputTree[IO].addDirectory(dir1).addDirectory(dir2)

      defaultParser.use(_.fromInput(treeInput).parse).map(_.root).assertEquals(expected)
    }

    "merge a directory with a directory from a theme" ignore new MergedDirectorySetup {
      // TODO - remove or adjust - using markup documents as theme inputs is currently not possible
//      val treeInput = InputTree[IO].addDirectory(dir1)
//      val themeInput = InputTree[IO].addDirectory(dir2).build(MarkupParser.of(Markdown).build.config.docTypeMatcher)

//      val theme = themeInput.map{ themeInputs => new Theme[IO] {
//        def inputs = themeInputs
//        def extensions = Nil
//        def treeProcessor = PartialFunction.empty
//      }}

//      val inputs = new TestThemeBuilder.Inputs {
//        def build[F[_]: Sync: Runtime] = InputTree[F].addDirectory(dir2)//.build(MarkupParser.of(Markdown).build.config.docTypeMatcher)
//      }
//      
//      defaultBuilder
//        .withTheme(TestThemeBuilder.forInputs(inputs))
//        .build
//        .use(_.fromInput(treeInput).parse)
//        .map(_.root)
        //.assertEquals(treeResult)
    }

    "merge a directory at a specific mount-point using an InputTreeBuilder" in new MergedDirectorySetup {
      val treeInput = InputTree[IO].addDirectory(dir1).addDirectory(dir2, Root / "tree-1")

      val doc7 = Document(Root / "tree-1" / "tree-2" / "doc-7.md", RootElement("Doc7"))
      val doc8 = Document(Root / "tree-1" / "tree-3" / "doc-8.md", RootElement("Doc8"))
      val doc9 = Document(Root / "tree-1" / "doc-9.md", RootElement("Doc9"))

      val tree1 = baseTree.tree.content(2).asInstanceOf[DocumentTree]
      val tree1Merged = tree1.copy(
        content = tree1.content :+
          doc9 :+
          DocumentTree(Root / "tree-1" / "tree-2", Seq(doc7)) :+
          DocumentTree(Root / "tree-1" / "tree-3", Seq(doc8))
      )

      val expected = baseTree.copy(
        tree = baseTree.tree.copy(
          content =
            baseTree.tree.content.take(2) :+
              tree1Merged :+
              baseTree.tree.content.drop(3).head
        )
      )
      defaultParser.use(_.fromInput(treeInput).parse).map(_.root).assertEquals(expected)
    }

    "read a directory from the file system containing a file with non-ASCII characters" in new ParserSetup {
      val dirname: String = getClass.getResource("/trees/c/").getFile

      def doc (num: Int, path: Path = Root) = Document(path / s"doc-$num.md", RootElement(p(s"Doc$num äöü")))

      val treeResult = DocumentTreeRoot(DocumentTree(Root, List(doc(1))))
      defaultParser.use(_.fromDirectory(dirname).parse).map(_.root).assertEquals(treeResult)
    }
  }
}
