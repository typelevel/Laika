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
import laika.ast.helper.DocumentViewBuilder.{viewOf, _}
import laika.ast.helper.ModelBuilder
import laika.ast.{StylePredicate, _}
import laika.bundle.{BundleOrigin, BundleProvider, ExtensionBundle, SpanParser, SpanParserBuilder}
import laika.config.Origin.TreeScope
import laika.config.{ConfigBuilder, Origin}
import laika.format.{Markdown, ReStructuredText}
import laika.io.api.TreeParser
import laika.io.helper.{InputBuilder, TestThemeBuilder}
import laika.io.implicits._
import laika.io.model.{InputTree, InputTreeBuilder, ParsedTree}
import laika.io.runtime.ParserRuntime.{DuplicatePath, ParserErrors}
import laika.io.runtime.Runtime
import laika.theme.Theme
import laika.parse.Parser
import laika.parse.markup.DocumentParser.{InvalidDocument, InvalidDocuments}
import laika.parse.text.TextParsers
import laika.rewrite.{DefaultTemplatePath, TemplateContext, TemplateRewriter}
import org.scalatest.Assertion


class TreeParserSpec extends IOWordSpec 
                         with ModelBuilder
                         with FileIO {

  trait ParserSetup {

    val defaultBuilder: TreeParser.Builder[IO] = MarkupParser
      .of(Markdown)
      .io(blocker)
      .parallel[IO]
      .withTheme(Theme.empty)
    
    val defaultParser: Resource[IO, TreeParser[IO]] = defaultBuilder.build
    
    def parserWithBundle (bundle: ExtensionBundle): Resource[IO, TreeParser[IO]] = 
      MarkupParser
        .of(Markdown)
        .using(bundle)
        .io(blocker)
        .parallel[IO]
        .withTheme(Theme.empty)
        .build

    def parserWithTheme (bundle: ExtensionBundle): Resource[IO, TreeParser[IO]] =
      MarkupParser
        .of(Markdown)
        .io(blocker)
        .parallel[IO]
        .withTheme(Theme.empty)
        .withTheme(TestThemeBuilder.forBundle(bundle))
        .build

    def parserWithThemeAndBundle (themeBundle: ExtensionBundle, appBundle: ExtensionBundle): Resource[IO, TreeParser[IO]] =
      MarkupParser
        .of(Markdown)
        .using(appBundle)
        .io(blocker)
        .parallel[IO]
        .withTheme(TestThemeBuilder.forBundle(themeBundle))
        .build

    def toTreeView (parsed: ParsedTree[IO]): TreeView = viewOf(parsed.root.tree)
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
    
    def docView (num: Int, path: Path = Root) = DocumentView(path / s"doc$num.md", Content(List(p("foo"))) :: Nil)
    def docView (name: String) = DocumentView(Root / name, Content(List(p("foo"))) :: Nil)
    
    def customDocView (name: String, content: Seq[Block], path: Path = Root) = DocumentView(path / name, Content(content) :: Nil)

    def toView (parsed: ParsedTree[IO]): RootView = viewOf(parsed.root)

    def toViewWithTemplating (parsed: ParsedTree[IO]): RootView = viewOf(TemplateRewriter.applyTemplates(parsed.root, TemplateContext("html")).toOption.get)
    
    
    def parsedTree: IO[RootView] = defaultParser.use(_.fromInput(build(inputs)).parse).map(toViewWithTemplating)
    
    def mixedParsedTree: IO[RootView] = {
      val parser = MarkupParser
        .of(Markdown)
        .io(blocker)
        .parallel[IO]
        .withTheme(Theme.empty)
        .withAlternativeParser(MarkupParser.of(ReStructuredText))
        .build
      parser.use(_.fromInput(build(inputs)).parse).map(toView)
    }
    
    def parsedWith (bundle: ExtensionBundle): IO[RootView] =
      parserWithBundle(bundle)
        .use(_.fromInput(build(inputs)).parse)
        .map(toViewWithTemplating)

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
      val treeResult = RootView(Nil)
      parsedTree.assertEquals(treeResult)
    }

    "parse a tree with a single document" in new TreeParserSetup {
      val inputs = Seq(
        Root / "name.md" -> Contents.name
      )
      val docResult = DocumentView(Root / "name.md", Content(List(p("foo"))) :: Nil)
      val treeResult = TreeView(Root, List(Documents(List(docResult)))).asRoot
      parsedTree.assertEquals(treeResult)
    }

    "parse a tree with multiple subtrees" in new TreeParserSetup {
      val inputs = Seq(
        Root / "doc1.md" -> Contents.name,
        Root / "doc2.md" -> Contents.name,
        Root / "dir1" / "doc3.md" -> Contents.name,
        Root / "dir1" / "doc4.md" -> Contents.name,
        Root / "dir2" / "doc5.md" -> Contents.name,
        Root / "dir2" / "doc6.md" -> Contents.name
      )
      val subtree1 = TreeView(Root / "dir1", List(Documents(List(docView(3, Root / "dir1"), docView(4, Root / "dir1")))))
      val subtree2 = TreeView(Root / "dir2", List(Documents(List(docView(5, Root / "dir2"), docView(6, Root / "dir2")))))
      val treeResult = TreeView(Root, List(
        Documents(List(docView(1), docView(2))),
        Subtrees(List(subtree1, subtree2))
      )).asRoot
      parsedTree.assertEquals(treeResult)
    }
    
    "collect errors from multiple documents" in new TreeParserSetup {
      val inputs = Seq(
        Root / "doc1.md" -> "[link1]",
        Root / "doc2.md" -> "[link2]",
        Root / "dir1" / "doc3.md" -> "[link3]",
        Root / "dir1" / "doc4.md" -> "[link4]",
        Root / "dir2" / "doc5.md" -> "[link5]",
        Root / "dir2" / "doc6.md" -> "[link6]"
      )
      val invalidDocuments = inputs.map { case (path, markup) => 
        val msg = s"unresolved link id reference: link${markup.charAt(5)}"
        val invalidSpan = InvalidSpan(msg, generatedSource(markup))
        InvalidDocument(NonEmptyChain.one(invalidSpan), path)
      }
      val expectedError = InvalidDocuments(NonEmptyChain.fromChainUnsafe(Chain.fromSeq(invalidDocuments)))
      val expectedMessage =
        """/doc1.md
          |
          |  [1]: unresolved link id reference: link1
          |
          |  [link1]
          |  ^
          |
          |/doc2.md
          |
          |  [1]: unresolved link id reference: link2
          |
          |  [link2]
          |  ^
          |
          |/dir1/doc3.md
          |
          |  [1]: unresolved link id reference: link3
          |
          |  [link3]
          |  ^
          |
          |/dir1/doc4.md
          |
          |  [1]: unresolved link id reference: link4
          |
          |  [link4]
          |  ^
          |
          |/dir2/doc5.md
          |
          |  [1]: unresolved link id reference: link5
          |
          |  [link5]
          |  ^
          |
          |/dir2/doc6.md
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
        Root / "doc1.md" -> Contents.name,
        Root / "doc2.md" -> Contents.name,
        Root / "README.md" -> Contents.name,
        Root / "cover.md" -> Contents.name
      )
      val treeResult = RootView(Seq(
        CoverDocument(docView("cover.md")),
        TreeView(Root, List(
          TitleDocument(docView("README.md")),
          Documents(List(docView(1), docView(2)))
        ))
      ))
      parsedTree.assertEquals(treeResult)
    }

    "parse a tree with a title document with a custom document name configuration" in new TreeParserSetup {
      val inputs = Seq(
        Root / "directory.conf" -> Contents.titleDocNameConf,
        Root / "doc1.md" -> Contents.name,
        Root / "doc2.md" -> Contents.name,
        Root / "alternative-title.md" -> Contents.name,
        Root / "cover.md" -> Contents.name
      )
      val treeResult = RootView(Seq(
        CoverDocument(docView("cover.md")),
        TreeView(Root, List(
          TitleDocument(docView("alternative-title.md")),
          Documents(List(docView(1), docView(2)))
        ), ConfigBuilder
            .withOrigin(Origin(TreeScope, Root / "directory.conf"))
            .withValue("laika.titleDocuments.inputName", "alternative-title")
            .build
        )
      ))
      parsedTree.assertEquals(treeResult)
    }

    "parse a tree with a single template" ignore new TreeParserSetup {
      val inputs = Seq(
        Root / "main.template.html" -> Contents.name
      )
      val template = TemplateView(Root / "main.template.html", TemplateRoot("foo"))
      val treeResult = TreeView(Root, List(TemplateDocuments(List(template)))).asRoot
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
      val treeResult = RootView(List(StaticDocuments(List(Root / "omg.js"))))
      parsedTree.assertEquals(treeResult)
    }

    "parse a tree with all available file types and multiple markup formats" in new TreeParserSetup {
      val inputs = Seq(
        Root / "doc1.md" -> Contents.link,
        Root / "doc2.rst" -> Contents.link,
        Root / "mainA.template.html" -> Contents.name,
        Root / "dir1" / "mainB.template.html" -> Contents.name,
        Root / "dir1" / "doc3.md" -> Contents.name,
        Root / "dir1" / "doc4.md" -> Contents.name,
        Root / "dir2" / "omg.js" -> Contents.name,
        Root / "dir2" / "doc5.md" -> Contents.name,
        Root / "dir2" / "doc6.md" -> Contents.name,
      )

      def template (char: Char, path: Path) = TemplateView(path / s"main$char.template.html", TemplateRoot("foo"))

      val dyn = TemplateView(Root / "dir2" / "main.dynamic.html", TemplateRoot("foo"))
      val subtree1 = TreeView(Root / "dir1", List(
        Documents(List(docView(3, Root / "dir1"), docView(4, Root / "dir1"))),
        TemplateDocuments(List(template('B', Root / "dir1")))
      ))
      val subtree2 = TreeView(Root / "dir2", List(
        Documents(List(docView(5, Root / "dir2"), docView(6, Root / "dir2")))
      ))
      val tree = TreeView(Root, List(
        Documents(List(customDocView("doc1.md", Seq(p(SpanLink(Seq(Text("link")), ExternalTarget("/foo"))))), customDocView("doc2.rst", Seq(p("[link](/foo)"))))),
        TemplateDocuments(List(template('A', Root))),
        Subtrees(List(subtree1, subtree2))
      ))
      val expectedRoot = RootView(Seq(StaticDocuments(Seq(Root / "dir2" / "omg.js")), tree))
      mixedParsedTree.assertEquals(expectedRoot)
    }

    "allow to specify a custom template engine" ignore new TreeParserSetup {
      val parser: Parser[TemplateRoot] = TextParsers.anyChars.map { str => TemplateRoot("$$" + str) }
      val inputs = Seq(
        Root / "main1.template.html" -> Contents.name,
        Root / "main2.template.html" -> Contents.name
      )

      def template (num: Int) = TemplateView(Root / s"main$num.template.html", TemplateRoot("$$foo"))

      val treeResult = TreeView(Root, List(TemplateDocuments(List(template(1), template(2))))).asRoot
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
      val treeResult = RootView(List(StyleSheets(Map(
        "aaa" -> StyleDeclarationSet(Set(Path.parse("/main1.aaa.css"), Path.parse("/main3.aaa.css")), Set(styleDecl("foo"), styleDecl("foo", 1))),
        "bbb" -> StyleDeclarationSet(Set(Path.parse("/main2.bbb.css")), Set(styleDecl("bar")))
      ))))
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
      val template = TemplateRoot(t("aa "), t("bar"), t(" bb"))
      val result = Seq(template, template)
      parsedTemplates(BundleProvider.forTemplateDirective(directive)).assertEquals(result)
    }

    "add indentation information if an embedded root is preceded by whitespace characters" in new TreeParserSetup {

      import laika.ast.EmbeddedRoot

      val inputs = Seq(
        DefaultTemplatePath.forHTML -> Contents.template,
        Root / "doc.md" -> Contents.multiline
      )
      val docResult = DocumentView(Root / "doc.md", Content(List(TemplateRoot(
        t("<div>\n  "),
        EmbeddedRoot(List(p("aaa"), p("bbb")), 2),
        t("\n</div>")
      ))) :: Nil)
      val treeResult = TreeView(Root, List(Documents(List(docResult)))).asRoot
      parsedTree.assertEquals(treeResult)
    }

    "not add indentation information if an embedded root is preceded by non-whitespace characters" in new TreeParserSetup {

      import laika.ast.EmbeddedRoot

      val inputs = Seq(
        DefaultTemplatePath.forHTML -> Contents.template2,
        Root / "doc.md" -> Contents.multiline
      )
      val docResult = DocumentView(Root / "doc.md", Content(List(TemplateRoot(
        t("<div>\nxx"),
        EmbeddedRoot(p("aaa"), p("bbb")),
        t("\n</div>")
      ))) :: Nil)
      val treeResult = TreeView(Root, List(Documents(List(docResult)))).asRoot
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

      def docView (num: Int, path: Path = Root) = DocumentView(path / s"doc$num.md", Content(List(p("Doc" + num))) :: Nil)

      val subtree1 = TreeView(Root / "dir1", List(Documents(List(docView(3, Root / "dir1"), docView(4, Root / "dir1")))))
      val subtree2 = TreeView(Root / "dir2", List(Documents(List(docView(5, Root / "dir2"), docView(6, Root / "dir2")))))
      val treeResult = TreeView(Root, List(
        Documents(List(docView(1), docView(2))),
        Subtrees(List(subtree1, subtree2))
      ))
      defaultParser.use(_.fromDirectory(dirname).parse).map(toTreeView).assertEquals(treeResult)
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

      val bundle = BundleProvider.forMarkupParser(spanParsers = Seq(spanFor('+')))

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
      val dirname: String = getClass.getResource("/trees/a/").getFile
      def docView (num: Int, path: Path = Root) = DocumentView(path / s"doc$num.md", Content(List(p("Doc" + num))) :: Nil)
      val subtree1 = TreeView(Root / "dir1", List(Documents(List(docView(3, Root / "dir1"), docView(4, Root / "dir1")))))
      
      def subtree2: TreeView

      lazy val treeResult = TreeView(Root, List(
        Documents(List(docView(1), docView(2))),
        Subtrees(List(subtree1, subtree2))
      ))
      
      def input: InputTreeBuilder[IO]
      def parser: Resource[IO, TreeParser[IO]]
      
      def run (): Assertion = parser.use(_.fromInput(input).parse).map(toTreeView).assertEquals(treeResult)
    }

    trait CustomInput extends CustomInputSetup {
      lazy val input: InputTreeBuilder[IO] = addDoc(InputTree[IO].addDirectory(dirname))
      lazy val parser: Resource[IO, TreeParser[IO]] = defaultBuilder.build
      def addDoc (input: InputTreeBuilder[IO]): InputTreeBuilder[IO]
    }
    
    trait CustomTheme extends CustomInputSetup {
      lazy val input: InputTreeBuilder[IO] = InputTree[IO].addDirectory(dirname)
      val themInputs = new TestThemeBuilder.Inputs {
        def build[F[_]: Sync: Runtime] = addDoc(InputTree[F])
      }
      lazy val parser: Resource[IO, TreeParser[IO]] = defaultBuilder.withTheme(TestThemeBuilder.forInputs(themInputs)).build
      def addDoc[F[_]: Sync: Runtime] (input: InputTreeBuilder[F]): InputTreeBuilder[F]
    }
    
    trait ExtraDocSetup extends CustomInputSetup {
      lazy val subtree2 = TreeView(Root / "dir2", List(Documents(List(docView(5, Root / "dir2"), docView(6, Root / "dir2"), docView(7, Root / "dir2")))))
    }

    trait ExtraTemplateSetup extends CustomInputSetup {
      val templatePath: Path = Root / "dir2" / "tmpl.template.html"
      lazy val subtree2 = TreeView(Root / "dir2", List(
        Documents(List(docView(5, Root / "dir2"), docView(6, Root / "dir2"))), 
        TemplateDocuments(List(TemplateView(templatePath, TemplateRoot(TemplateString("Template")))))
      ))
    }

    trait ExtraConfigSetup extends CustomInputSetup {
      val configPath: Path = Root / "dir2" / "directory.conf"
      lazy val subtree2 = TreeView(Root / "dir2", List(
        Documents(List(docView(5, Root / "dir2"), docView(6, Root / "dir2")))
      ), ConfigBuilder.withOrigin(Origin(TreeScope, configPath)).withValue("foo", 7).build)
    }

    trait ExtraStylesSetup extends CustomInputSetup {
      val configPath: Path = Root / "dir2" / "directory.conf"
      lazy val subtree2 = TreeView(Root / "dir2", List(
        Documents(List(docView(5, Root / "dir2"), docView(6, Root / "dir2")))
      ))
    }

    "read a directory from the file system plus one AST input" in new ExtraDocSetup with CustomInput {
      def addDoc (input: InputTreeBuilder[IO]): InputTreeBuilder[IO] = 
        input.addDocument(Document(Root / "dir2" / "doc7.md", RootElement(Paragraph("Doc7"))))
      run()
    }

    "read a directory from the file system plus one AST input from a theme" in new ExtraDocSetup with CustomTheme {
      def addDoc[F[_]: Sync: Runtime] (input: InputTreeBuilder[F]): InputTreeBuilder[F] =
        input.addDocument(Document(Root / "dir2" / "doc7.md", RootElement(Paragraph("Doc7"))))
      run()
    }

    "read a directory from the file system plus one string input" in new ExtraDocSetup with CustomInput {
      def addDoc (input: InputTreeBuilder[IO]): InputTreeBuilder[IO] = input.addString("Doc7", Root / "dir2" / "doc7.md")
      run()
    }

    "read a directory from the file system plus one document from an input stream" in new ExtraDocSetup with CustomInput {
      def addDoc (input: InputTreeBuilder[IO]): InputTreeBuilder[IO] = input.addStream(new ByteArrayInputStream("Doc7".getBytes), Root / "dir2" / "doc7.md")
      run()
    }

    "read a directory from the file system plus one extra file" in new ExtraDocSetup with CustomInput {
      lazy val filename: String = getClass.getResource("/trees/d/doc7.md").getFile
      def addDoc (input: InputTreeBuilder[IO]): InputTreeBuilder[IO] = input.addFile(filename, Root / "dir2" / "doc7.md")
      run()
    }

    "read a directory from the file system plus one extra template from a string" in new ExtraTemplateSetup with CustomInput {
      def addDoc (input: InputTreeBuilder[IO]): InputTreeBuilder[IO] = input.addString("Template", templatePath)
      run()
    }

    "read a directory from the file system plus one extra template from a string in a theme" in new ExtraTemplateSetup with CustomTheme {
      def addDoc[F[_]: Sync: Runtime] (input: InputTreeBuilder[F]): InputTreeBuilder[F] = input.addString("Template", templatePath)
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
      def addDoc[F[_]: Sync: Runtime] (input: InputTreeBuilder[F]): InputTreeBuilder[F] = input.addString("foo = 7", configPath)
      run()
    }

    "read a directory from the file system plus one extra config document built programmatically" in new ExtraConfigSetup with CustomInput {
      def addDoc (input: InputTreeBuilder[IO]): InputTreeBuilder[IO] = 
        input.addConfig(ConfigBuilder.withOrigin(Origin(TreeScope, configPath)).withValue("foo", 7).build, configPath)
      run()
    }

    "read a directory from the file system using a custom document type matcher" in new ParserSetup {
      val dirname: String = getClass.getResource("/trees/a/").getFile

      def docView (num: Int, path: Path = Root) = DocumentView(path / s"doc$num.md", Content(List(p("Doc" + num))) :: Nil)

      val subtree1 = TreeView(Root / "dir1", List(Documents(List(docView(3, Root / "dir1"), docView(4, Root / "dir1")))))
      val subtree2 = TreeView(Root / "dir2", List(Documents(List(docView(5, Root / "dir2"), docView(6, Root / "dir2")))))
      val treeResult = TreeView(Root, List(
        Documents(List(docView(2))),
        Subtrees(List(subtree1, subtree2))
      ))
      val parser = parserWithBundle(BundleProvider.forDocTypeMatcher { case Root / "doc1.md" => Ignored })
      parser.use(_.fromDirectory(dirname).parse).map(toTreeView).assertEquals(treeResult)
    }

    "allow to specify a custom exclude filter" in new ParserSetup {
      val dirname: String = getClass.getResource("/trees/a/").getFile

      def docView (num: Int, path: Path = Root) = DocumentView(path / s"doc$num.md", Content(List(p("Doc" + num))) :: Nil)

      val subtree2 = TreeView(Root / "dir2", List(Documents(List(docView(5, Root / "dir2"), docView(6, Root / "dir2")))))
      val treeResult = TreeView(Root, List(
        Documents(List(docView(2))),
        Subtrees(List(subtree2))
      ))
      defaultParser
        .use(_.fromDirectory(dirname, { f: java.io.File => f.getName == "doc1.md" || f.getName == "dir1" }).parse)
        .map(toTreeView)
        .assertEquals(treeResult)
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

      def docView (num: Int, path: Path = Root) = DocumentView(path / s"doc$num.md", Content(List(p("Doc" + num))) :: Nil)

      val subtree1 = TreeView(Root / "dir1", List(Documents(List(docView(3, Root / "dir1"), docView(4, Root / "dir1"), docView(7, Root / "dir1")))))
      val subtree2 = TreeView(Root / "dir2", List(Documents(List(docView(5, Root / "dir2"), docView(6, Root / "dir2")))))
      val subtree3 = TreeView(Root / "dir3", List(Documents(List(docView(8, Root / "dir3")))))
      val treeResult = TreeView(Root, List(
        Documents(List(docView(1), docView(2), docView(9))),
        Subtrees(List(subtree1, subtree2, subtree3))
      ))
    }

    "merge two directories from the file system using the fromDirectories method" in new MergedDirectorySetup {
      defaultParser.use(_.fromDirectories(Seq(dir1, dir2)).parse).map(toTreeView).assertEquals(treeResult)
    }

    "merge two directories from the file system using an InputTreeBuilder" in new MergedDirectorySetup {
      val treeInput = InputTree[IO].addDirectory(dir1).addDirectory(dir2)

      defaultParser.use(_.fromInput(treeInput).parse).map(toTreeView).assertEquals(treeResult)
    }

    "merge a directory with a directory from a theme" ignore new MergedDirectorySetup {
      // TODO - remove or adjust - using markup documents as theme inputs is currently not possible
      val treeInput = InputTree[IO].addDirectory(dir1)
      val themeInput = InputTree[IO].addDirectory(dir2).build(MarkupParser.of(Markdown).build.config.docTypeMatcher)

//      val theme = themeInput.map{ themeInputs => new Theme[IO] {
//        def inputs = themeInputs
//        def extensions = Nil
//        def treeProcessor = PartialFunction.empty
//      }}

      val inputs = new TestThemeBuilder.Inputs {
        def build[F[_]: Sync: Runtime] = InputTree[F].addDirectory(dir2)//.build(MarkupParser.of(Markdown).build.config.docTypeMatcher)
      }
      
      defaultBuilder
        .withTheme(TestThemeBuilder.forInputs(inputs))
        .build
        .use(_.fromInput(treeInput).parse)
        .map(toTreeView)
        .assertEquals(treeResult)
    }

    "merge a directory at a specific mount-point using an InputTreeBuilder" in new MergedDirectorySetup {
      val treeInput = InputTree[IO].addDirectory(dir1).addDirectory(dir2, Root / "dir2")

      val mergedResult = {
        val subtree1 = TreeView(Root / "dir1", List(Documents(List(docView(3, Root / "dir1"), docView(4, Root / "dir1")))))
        val subtree1Nested = TreeView(Root / "dir2" / "dir1", List(Documents(List(docView(7, Root / "dir2" / "dir1")))))
        val subtree3 = TreeView(Root / "dir2" / "dir3", List(Documents(List(docView(8, Root / "dir2" / "dir3")))))
        val subtree2 = TreeView(Root / "dir2", List(
          Documents(List(docView(5, Root / "dir2"), docView(6, Root / "dir2"), docView(9, Root / "dir2"))),
          Subtrees(List(subtree1Nested, subtree3))
        ))
        TreeView(Root, List(
          Documents(List(docView(1), docView(2))),
          Subtrees(List(subtree1, subtree2))
        ))
      }
      
      defaultParser.use(_.fromInput(treeInput).parse).map(toTreeView).assertEquals(mergedResult)
    }

    "read a directory from the file system containing a file with non-ASCII characters" in new ParserSetup {
      val dirname: String = getClass.getResource("/trees/c/").getFile

      def docView (num: Int, path: Path = Root) = DocumentView(path / s"doc$num.md", Content(List(p(s"Doc$num äöü"))) :: Nil)

      val treeResult = TreeView(Root, List(
        Documents(List(docView(1)))
      ))
      defaultParser.use(_.fromDirectory(dirname).parse).map(toTreeView).assertEquals(treeResult)
    }
  }
}
