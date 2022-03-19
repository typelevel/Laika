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

import cats.data.{Chain, NonEmptyChain}
import cats.effect.IO
import laika.api.MarkupParser
import laika.ast.DocumentType._
import laika.ast.Path.Root
import laika.ast._
import laika.ast.sample.{ParagraphCompanionShortcuts, SampleTrees, TestSourceBuilders}
import laika.bundle._
import laika.config.ConfigException
import laika.format.{Markdown, ReStructuredText}
import laika.io.helper.InputBuilder
import laika.io.implicits._
import laika.io.model.{InputTree, InputTreeBuilder, ParsedTree}
import laika.io.runtime.ParserRuntime.{DuplicatePath, ParserErrors}
import laika.parse.Parser
import laika.parse.markup.DocumentParser.{InvalidDocument, InvalidDocuments}
import laika.parse.text.TextParsers
import laika.rewrite.nav.TargetFormats
import laika.rewrite.{DefaultTemplatePath, TemplateContext, TemplateRewriter}
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
    val link = "[link](/foo)"
    val name = "foo"
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
    val dynDoc = "${value}"
    val conf = "value: abc"
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
  
  val defaultContent = Seq(p("foo"))
  
  def docResult (num: Int, content: Seq[Block] = defaultContent, path: Path = Root): Document = 
    Document(path / s"doc-$num.md", RootElement(content))
  def docResult (name: String): Document = Document(Root / name, RootElement(defaultContent))
  def customDocResult(name: String, content: Seq[Block], path: Path = Root): Document = 
    Document(path / name, RootElement(content))

  def applyTemplates (parsed: ParsedTree[IO]): DocumentTreeRoot = 
    TemplateRewriter.applyTemplates(parsed.root, TemplateContext("html")).toOption.get
    
  def parsedTree (inputs: Seq[(Path, String)], f: InputTreeBuilder[IO] => InputTreeBuilder[IO] = identity): IO[DocumentTreeRoot] = defaultParser
    .use(_.fromInput(f(build(inputs))).parse)
    .map(applyTemplates)
  
  def mixedParsedTree (inputs: Seq[(Path, String)]): IO[DocumentTreeRoot] = {
    val parser = MarkupParser
      .of(Markdown)
      .parallel[IO]
      .withTheme(Theme.empty)
      .withAlternativeParser(MarkupParser.of(ReStructuredText))
      .build
    parser.use(_.fromInput(build(inputs)).parse).map(_.root)
  }
  
  def parsedWith (inputs: Seq[(Path, String)], bundle: ExtensionBundle): IO[DocumentTreeRoot] =
    parserWithBundle(bundle)
      .use(_.fromInput(build(inputs)).parse)
      .map(applyTemplates)

  def parsedTemplates (inputs: Seq[(Path, String)], bundle: ExtensionBundle): IO[Seq[TemplateRoot]] = {
    parserWithBundle(bundle)
      .use(_.fromInput(build(inputs)).parse)
      .flatMap { parsed =>
        IO.fromEither(DocumentCursor(Document(Root, RootElement.empty)).map(cursor => 
          parsed.root.tree.templates.map { tpl =>
            tpl.content.rewriteChildren(TemplateRewriter.rewriteRules(cursor))
          }
        ).left.map(ConfigException.apply))
      }
  }
  

  
  test("an empty tree") {
    parsedTree(Nil).assertEquals(DocumentTreeRoot(DocumentTree(Root, Nil)))
  }

  test("tree with a single document") {
    val inputs = Seq(
      Root / "name.md" -> Contents.name
    )
    val docResult = Document(Root / "name.md", RootElement(p("foo")))
    val treeResult = DocumentTreeRoot(DocumentTree(Root, List(docResult)))
    parsedTree(inputs).assertEquals(treeResult)
  }

  test("tree with multiple subtrees") {
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
    parsedTree(inputs).assertEquals(expected)
  }
  
  test("collect errors from multiple documents") {
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
      val invalidSpan = InvalidSpan(msg, source(markup, markup, path))
      InvalidDocument(path, invalidSpan)
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
    assertEquals(InvalidDocuments.format(expectedError.documents), expectedMessage)
    
    parsedTree(inputs).attempt.assertEquals(Left(expectedError))
  }
  
  test("report errors originating in templates with additional path info") {
    val markup = "text"
    val docPath = Root / "doc-1.md"
    val inputs = Seq(
      docPath -> markup,
      DefaultTemplatePath.forHTML -> Contents.brokenTemplate
    )
    val msg = "One or more errors processing directive 'foo': No template directive registered with name: foo"
    val invalidDocument = {
      val invalidSpan2 = InvalidSpan(msg, source("@:foo", Contents.brokenTemplate, DefaultTemplatePath.forHTML))
      InvalidDocument(docPath, invalidSpan2)
    }
    val expectedError = InvalidDocuments(NonEmptyChain(invalidDocument))
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
    parsedTree(inputs).assertEquals(treeResult)
  }

  test("tree with a title document with a custom document name configuration") {
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
    parsedTree(inputs).assertEquals(treeResult)
  }

  test("tree with a single template".ignore) {
    val inputs = Seq(
      Root / "main.template.html" -> Contents.name
    )
    val template = TemplateDocument(Root / "main.template.html", TemplateRoot("foo"))
    val treeResult = DocumentTreeRoot(DocumentTree(Root, Nil, templates = List(template)))
    parsedTree(inputs).assertEquals(treeResult)
  }

  test("fail with duplicate paths") {
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

  test("tree with a static document") {
    val inputs = Seq(
      Root / "omg.js" -> Contents.name
    )
    val staticDoc = StaticDocument(Root / "omg.js", TargetFormats.Selected("html"))
    val treeResult = DocumentTreeRoot(DocumentTree(Root, Nil), staticDocuments = List(staticDoc))
    parsedTree(inputs).assertEquals(treeResult)
  }

  test("tree with a provided path") {

    val providedPath = Root / "provided" / "ext.html"
    
    val inputs = Seq(
      Root / "doc-1.md" -> "[link](provided/ext.html)"
    )
    val target = InternalTarget(providedPath).relativeTo(Root / "doc-1.md")
    val expectedDocs = Seq(docResult(1, Seq(Paragraph(SpanLink.internal(providedPath)("link").withTarget(target)))))
    val expectedResult = DocumentTreeRoot(DocumentTree(Root, expectedDocs), staticDocuments = List(StaticDocument(providedPath)))
    parsedTree(inputs, _.addProvidedPath(providedPath)).assertEquals(expectedResult)
  }

  test("tree with all available file types and multiple markup formats") {
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
    mixedParsedTree(inputs).assertEquals(expected)
  }

  test("custom template engine".ignore) {
    val parser: Parser[TemplateRoot] = TextParsers.anyChars.map { str => TemplateRoot("$$" + str) }
    val inputs = Seq(
      Root / "main1.template.html" -> Contents.name,
      Root / "main2.template.html" -> Contents.name
    )

    def template (num: Int) = TemplateDocument(Root / s"main$num.template.html", TemplateRoot("$$foo"))

    val treeResult = DocumentTreeRoot(DocumentTree(Root, Nil, templates = List(template(1), template(2))))
    parsedWith(inputs, BundleProvider.forTemplateParser(parser)).assertEquals(treeResult)
  }

  test("custom style sheet engine") {
    val customDocTypeMatcher: PartialFunction[Path, DocumentType] = {
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
    parsedWith(inputs, BundleProvider.forDocTypeMatcher(customDocTypeMatcher)
      .withBase(BundleProvider.forStyleSheetParser(parser))).assertEquals(treeResult)
  }

  test("template directive") {

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
    parsedTemplates(inputs, BundleProvider.forTemplateDirective(directive)).assertEquals(result)
  }

  test("add indentation information if an embedded root is preceded by whitespace characters") {

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
    parsedTree(inputs).assertEquals(treeResult)
  }

  test("do not add indentation information if an embedded root is preceded by non-whitespace characters") {

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
    parsedTree(inputs).assertEquals(treeResult)
  }

  test("custom navigation order") {
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

  test("always move title documents to the front, even with a custom navigation order") {
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
    defaultParser.use(_.fromInput(build(inputs)).parse).map(_.root.tree).map { tree =>
      assertEquals(tree.titleDocument.map(_.path.basename), Some("README"))
      assertEquals(tree.content map (_.path.name), List("lemon.md", "shapes", "cherry.md", "colors", "apple.md", "orange.md"))
      assertEquals(tree.content map (_.position), List(
        TreePosition(Seq(1)),
        TreePosition(Seq(2)),
        TreePosition(Seq(3)),
        TreePosition(Seq(4)),
        TreePosition(Seq(5)),
        TreePosition(Seq(6)),
      ))
    }
  }

  

  object CustomSpanParsers {

    import TextParsers._
    import laika.parse.implicits._
    
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
    
    def parse (themeParsers: Seq[SpanParserBuilder] = Nil, appParsers: Seq[SpanParserBuilder] = Nil): IO[RootElement] = 
      parserWithThemeAndBundle(
        BundleProvider.forMarkupParser(spanParsers = themeParsers, origin = BundleOrigin.Theme),
        BundleProvider.forMarkupParser(spanParsers = appParsers)
      )
        .use(_.fromInput(input).parse)
        .map(_.root.allDocuments.head.content)

    def parseWithThemeExtension (themeParsers: Seq[SpanParserBuilder] = Nil, themeExtensionParsers: Seq[SpanParserBuilder] = Nil): IO[RootElement] =
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

    parse(themeParsers).assertEquals(RootElement(Paragraph(
      Text("aaa "),
      DecoratedSpan('+', "bbb"),
      Text(" ccc")
    )))
  }

  test("let a span parser from an app extension override a span parser from a theme") {
    import CustomSpanParsers._
    
    val themeParsers = Seq(spanFor('+'))
    val appParsers = Seq(spanFor('+', '!'))

    parse(themeParsers, appParsers).assertEquals(RootElement(Paragraph(
      Text("aaa "),
      DecoratedSpan('!', "bbb"),
      Text(" ccc")
    )))
  }

  test("let a span parser from a theme extension override a span parser from a base theme") {
    import CustomSpanParsers._

    val themeParsers = Seq(spanFor('+'))
    val themeExtParsers = Seq(spanFor('+', '!'))

    parseWithThemeExtension(themeParsers, themeExtParsers).assertEquals(RootElement(Paragraph(
      Text("aaa "),
      DecoratedSpan('!', "bbb"),
      Text(" ccc")
    )))
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
