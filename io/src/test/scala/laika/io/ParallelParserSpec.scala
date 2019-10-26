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

import cats.effect.{ContextShift, IO}
import laika.api.MarkupParser
import laika.ast.DocumentType._
import laika.ast.Path.Root
import laika.ast._
import laika.ast.helper.DocumentViewBuilder._
import laika.ast.helper.ModelBuilder
import laika.bundle.{BundleProvider, ExtensionBundle}
import laika.format.{Markdown, ReStructuredText}
import laika.io.implicits._
import laika.io.text.ParallelParser
import laika.io.helper.InputBuilder
import laika.io.model.{TreeInput}
import laika.parse.Parser
import laika.parse.text.TextParsers
import laika.rewrite.TemplateRewriter
import laika.runtime.ParserRuntime.{DuplicatePath, ParserErrors}
import laika.runtime.TestContexts._
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext


class ParallelParserSpec extends FlatSpec 
                   with Matchers
                   with ModelBuilder {

  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  trait ParserSetup {

    val defaultParser: ParallelParser[IO] = MarkupParser
      .of(Markdown)
      .io(blocker)
      .parallel[IO]
      .build
    
    def parserWithBundle (bundle: ExtensionBundle): ParallelParser[IO] = 
      MarkupParser
        .of(Markdown)
        .using(bundle)
        .io(blocker)
        .parallel[IO]
        .build
    
  }
  
  trait TreeParser extends InputBuilder with ParserSetup {
    
    def inputs: Seq[(Path, String)] 
    
    object Contents {
      val link = "[link](foo)"
      val name = "foo"
      val name2 = "bar"
      val multiline = """aaa
                       |
                       |bbb""".stripMargin
      val directive = "aa @:foo bar. bb"
      val template = """<div>
                      |  ${document.content}
                      |</div>""".stripMargin
      val template2 = """<div>
                       |xx${document.content}
                       |</div>""".stripMargin
      val dynDoc = "${config.value}"
      val conf = "value: abc"
      val order = """navigationOrder: [
        |  lemon.md
        |  shapes
        |  cherry.md
        |  colors
        |  apple.md
        |  orange.md
        |]""".stripMargin
    }
    
    val docTypeMatcher: Path => DocumentType = defaultParser.config.docTypeMatcher
    
    def build (in: Seq[(Path, String)]): IO[TreeInput[IO]] = IO.pure(build(in, docTypeMatcher))
    
    def docView (num: Int, path: Path = Root) = DocumentView(path / s"doc$num.md", Content(List(p("foo"))) :: Nil)
    def docView (name: String) = DocumentView(Root / name, Content(List(p("foo"))) :: Nil)
    
    def customDocView (name: String, content: Seq[Block], path: Path = Root) = DocumentView(path / name, Content(content) :: Nil)
  
    def withTemplatesApplied (root: DocumentTreeRoot): DocumentTreeRoot = TemplateRewriter.applyTemplates(root, "html").right.get
    
    def parsedTree: RootView = viewOf(withTemplatesApplied(defaultParser.fromInput(build(inputs)).parse.unsafeRunSync().root))
    
    def mixedParsedTree: RootView = {
      val parser = MarkupParser
        .of(Markdown)
        .io(blocker)
        .parallel[IO]
        .withAlternativeParser(MarkupParser.of(ReStructuredText))
        .build
      viewOf(parser.fromInput(IO.pure(build(inputs, parser.config.docTypeMatcher))).parse.unsafeRunSync().root)
    }
      
    
    def parsedWith (bundle: ExtensionBundle): RootView =
      viewOf(withTemplatesApplied(MarkupParser
        .of(Markdown)
        .using(bundle)
        .io(blocker)
        .parallel[IO]
        .build
        .fromInput(build(inputs)).parse.unsafeRunSync().root)
      )

    def parsedTemplates (bundle: ExtensionBundle): Seq[TemplateRoot] = {
      val root = MarkupParser
        .of(Markdown)
        .using(bundle)
        .io(blocker)
        .parallel[IO]
        .build
        .fromInput(build(inputs)).parse.unsafeRunSync().root
      root.tree.templates.map { tpl =>
        tpl.content.rewriteChildren(TemplateRewriter.rewriteRules(DocumentCursor(Document(Root, RootElement(Nil)))))
      }
    }
      
    def parsedWith (bundle: ExtensionBundle = ExtensionBundle.Empty, customMatcher: PartialFunction[Path, DocumentType] = PartialFunction.empty): RootView = {
      val input = IO.pure(build(inputs, customMatcher.orElse({case path => docTypeMatcher(path)})))
      val parser = MarkupParser.of(Markdown).using(bundle)
      viewOf(parser
        .io(blocker)
        .parallel[IO]
        .build
        .fromInput(input).parse.unsafeRunSync().root
      )
    }
  }
  

  
  "The parallel parser" should "parse an empty tree" in new TreeParser {
    val inputs = Nil
    val treeResult = RootView(Nil)
    parsedTree should be (treeResult)
  }

  it should "parse a tree with a single document" in new TreeParser {
    val inputs = Seq(
      Root / "name.md" -> Contents.name
    )
    val docResult = DocumentView(Root / "name.md", Content(List(p("foo"))) :: Nil)
    val treeResult = TreeView(Root, List(Documents(Markup, List(docResult)))).asRoot
    parsedTree should be (treeResult)
  }

  it should "parse a tree with multiple subtrees" in new TreeParser {
    val inputs = Seq(
      Root / "doc1.md"          -> Contents.name,
      Root / "doc2.md"          -> Contents.name,
      Root / "dir1" / "doc3.md" -> Contents.name,
      Root / "dir1" / "doc4.md" -> Contents.name,
      Root / "dir2" / "doc5.md" -> Contents.name,
      Root / "dir2" / "doc6.md" -> Contents.name
    )
    val subtree1 = TreeView(Root / "dir1", List(Documents(Markup, List(docView(3, Root / "dir1"),docView(4, Root / "dir1")))))
    val subtree2 = TreeView(Root / "dir2", List(Documents(Markup, List(docView(5, Root / "dir2"),docView(6, Root / "dir2")))))
    val treeResult = TreeView(Root, List(
      Documents(Markup, List(docView(1),docView(2))),
      Subtrees(List(subtree1,subtree2))
    )).asRoot
    parsedTree should be (treeResult)
  }

  it should "parse a tree with a cover and a title document" in new TreeParser {
    val inputs = Seq(
      Root / "doc1.md"  -> Contents.name,
      Root / "doc2.md"  -> Contents.name,
      Root / "title.md" -> Contents.name,
      Root / "cover.md" -> Contents.name
    )
    val treeResult = RootView(Seq(
      CoverDocument(docView("cover.md")),
      TreeView(Root, List(
        TitleDocument(docView("title.md")),
        Documents(Markup, List(docView(1),docView(2)))
      ))
    ))
    parsedTree should be (treeResult)
  }

  it should "parse a tree with a single template" ignore new TreeParser {
    val inputs = Seq(
      Root / "main.template.html" -> Contents.name
    )
    val template = TemplateView(Root / "main.template.html", TemplateRoot(List(TemplateString("foo"))))
    val treeResult = TreeView(Root, List(TemplateDocuments(List(template)))).asRoot
    parsedTree should be (treeResult)
  }

  it should "fail with duplicate paths" in new TreeParser {
    val inputs = Seq(
      Root / "doc1.md"  -> Contents.name,
      Root / "doc2.md"  -> Contents.name,
      Root / "doc2.md"  -> Contents.name,
      Root / "sub" / "doc.md"  -> Contents.name,
      Root / "sub" / "doc.md"  -> Contents.name
    )
    defaultParser.fromInput(build(inputs)).parse.attempt.unsafeRunSync() shouldBe Left(
      ParserErrors(Seq(DuplicatePath(Root / "doc2.md"), DuplicatePath(Root / "sub" / "doc.md")))
    )
  }

  it should "parse a tree with a static document" in new TreeParser {
    val inputs = Seq(
      Root / "omg.js" -> Contents.name
    )
    val treeResult = RootView(List(StaticDocuments(List(Root / "omg.js"))))
    parsedTree should be (treeResult)
  }

  it should "parse a tree with all available file types and multiple markup formats" in new TreeParser {
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
    
    def template (char: Char, path: Path) = TemplateView(path / s"main$char.template.html", TemplateRoot(List(TemplateString("foo"))))
    val dyn = TemplateView(Root / "dir2" / "main.dynamic.html", TemplateRoot(List(TemplateString("foo"))))
    val subtree1 = TreeView(Root / "dir1", List(
      Documents(Markup, List(docView(3, Root / "dir1"), docView(4, Root / "dir1"))),
      TemplateDocuments(List(template('B', Root / "dir1")))
    ))
    val subtree2 = TreeView(Root / "dir2", List(
      Documents(Markup, List(docView(5, Root / "dir2"), docView(6, Root / "dir2")))
    ))
    val tree = TreeView(Root, List(
      Documents(Markup, List(customDocView("doc1.md", Seq(p(ExternalLink(Seq(txt("link")), "foo")))),customDocView("doc2.rst", Seq(p("[link](foo)"))))),
      TemplateDocuments(List(template('A', Root))),
      Subtrees(List(subtree1,subtree2))
    ))
    val expectedRoot = RootView(Seq(StaticDocuments(Seq(Root / "dir2" / "omg.js")), tree))
    mixedParsedTree should be (expectedRoot)
  }
  
  it should "allow to specify a custom template engine" ignore new TreeParser {
    val parser: Parser[TemplateRoot] = TextParsers.any ^^ { str => TemplateRoot(List(TemplateString("$$" + str))) }
    val inputs = Seq(
      Root / "main1.template.html" -> Contents.name,
      Root / "main2.template.html" -> Contents.name
    )
    def template (num: Int) = TemplateView(Root / s"main$num.template.html", TemplateRoot(List(TemplateString("$$foo"))))
    val treeResult = TreeView(Root, List(TemplateDocuments(List(template(1),template(2))))).asRoot
    parsedWith(BundleProvider.forTemplateParser(parser)) should be (treeResult)
  }

  it should "allow to specify a custom style sheet engine" in new TreeParser {
    override val docTypeMatcher: PartialFunction[Path, DocumentType] = { case path =>
      val Stylesheet = """.+\.([a,b]+).css$""".r
      path.name match {
        case Stylesheet(kind) => StyleSheet(kind)
      }
    }
    def styleDecl(styleName: String, order: Int = 0) =
      StyleDeclaration(StylePredicate.ElementType("Type"), styleName -> "foo").increaseOrderBy(order)
    val parser: Parser[Set[StyleDeclaration]] = TextParsers.any ^^ {n => Set(styleDecl(n))}
    val inputs = Seq(
      Root / "main1.aaa.css" -> Contents.name,
      Root / "main2.bbb.css" -> Contents.name2,
      Root / "main3.aaa.css" -> Contents.name
    )
    val treeResult = RootView(List(StyleSheets(Map(
        "aaa" -> StyleDeclarationSet(Set(Path("/main1.aaa.css"), Path("/main3.aaa.css")), Set(styleDecl("foo"), styleDecl("foo", 1))),
        "bbb" -> StyleDeclarationSet(Set(Path("/main2.bbb.css")), Set(styleDecl("bar")))
    ))))
    parsedWith(BundleProvider.forDocTypeMatcher(docTypeMatcher)
      .withBase(BundleProvider.forStyleSheetParser(parser))) should be (treeResult)
  }

  it should "allow to specify a template directive" in new TreeParser {
    import laika.directive.Templates
    import Templates.dsl._

    val directive = Templates.create("foo") {
      defaultAttribute.as[String] map { TemplateString(_) }
    }
    val inputs = Seq(
      Root / "main1.template.html" -> Contents.directive,
      Root / "main2.template.html" -> Contents.directive
    )
    val template = tRoot(tt("aa "),tt("bar"),tt(" bb"))
    val result = Seq(template, template)
    parsedTemplates(BundleProvider.forTemplateDirective(directive)) should be (result)
  }

  it should "add indentation information if an embedded root is preceded by whitespace characters" in new TreeParser {
    import laika.ast.EmbeddedRoot
    val inputs = Seq(
      Root / "default.template.html" -> Contents.template,
      Root / "doc.md" -> Contents.multiline
    )
    val docResult = DocumentView(Root / "doc.md", Content(List(tRoot(
        tt("<div>\n  "),
        EmbeddedRoot(List(p("aaa"),p("bbb")), 2),
        tt("\n</div>")
    ))) :: Nil)
    val treeResult = TreeView(Root, List(Documents(Markup, List(docResult)))).asRoot
    parsedTree should be (treeResult)
  }
  
  it should "not add indentation information if an embedded root is preceded by non-whitespace characters" in new TreeParser {
    import laika.ast.EmbeddedRoot
    val inputs = Seq(
      Root / "default.template.html" -> Contents.template2,
      Root / "doc.md" -> Contents.multiline
    )
    val docResult = DocumentView(Root / "doc.md", Content(List(tRoot(
      tt("<div>\nxx"),
      EmbeddedRoot(List(p("aaa"),p("bbb"))),
      tt("\n</div>")
    ))) :: Nil)
    val treeResult = TreeView(Root, List(Documents(Markup, List(docResult)))).asRoot
    parsedTree should be (treeResult)
  }
  
  it should "allow to specify a custom navigation order" in new TreeParser {
    val inputs = Seq(
      Root / "apple.md"             -> Contents.name,
      Root / "orange.md"            -> Contents.name,
      Root / "colors" / "green.md"  -> Contents.name,
      Root / "lemon.md"             -> Contents.name,
      Root / "shapes" / "circle.md" -> Contents.name,
      Root / "cherry.md"            -> Contents.name,
      Root / "directory.conf"       -> Contents.order,
    )
    val root = defaultParser.fromInput(build(inputs)).parse.unsafeRunSync().root
    root.tree.content map (_.path.name) should be (List("lemon.md","shapes","cherry.md","colors","apple.md","orange.md"))
  }

  it should "always move title documents to the front, even with a custom navigation order" in new TreeParser {
    val inputs = Seq(
      Root / "apple.md"             -> Contents.name,
      Root / "orange.md"            -> Contents.name,
      Root / "colors" / "green.md"  -> Contents.name,
      Root / "lemon.md"             -> Contents.name,
      Root / "title.md"             -> Contents.name,
      Root / "shapes" / "circle.md" -> Contents.name,
      Root / "cherry.md"            -> Contents.name,
      Root / "directory.conf"       -> Contents.order,
    )
    val tree = defaultParser.fromInput(build(inputs)).parse.unsafeRunSync().root.tree
    
    tree.titleDocument.map(_.path.basename) shouldBe Some("title")
    
    tree.content map (_.path.name) should be (List("lemon.md","shapes","cherry.md","colors","apple.md","orange.md"))
    tree.content map (_.position) should be (List(
      TreePosition(Seq(1)),
      TreePosition(Seq(2)),
      TreePosition(Seq(3)),
      TreePosition(Seq(4)),
      TreePosition(Seq(5)),
      TreePosition(Seq(6)),
    ))
  }
  
  it should "read a directory from the file system using the fromDirectory method" in new ParserSetup {
    val dirname: String = getClass.getResource("/trees/a/").getFile
    def docView (num: Int, path: Path = Root) = DocumentView(path / s"doc$num.md", Content(List(p("Doc"+num))) :: Nil)
    val subtree1 = TreeView(Root / "dir1", List(Documents(Markup, List(docView(3, Root / "dir1"),docView(4, Root / "dir1")))))
    val subtree2 = TreeView(Root / "dir2", List(Documents(Markup, List(docView(5, Root / "dir2"),docView(6, Root / "dir2")))))
    val treeResult = TreeView(Root, List(
      Documents(Markup, List(docView(1),docView(2))),
      Subtrees(List(subtree1,subtree2))
    ))
    viewOf(defaultParser.fromDirectory(dirname).parse.unsafeRunSync().root.tree) should be (treeResult)
  }

  it should "read a directory from the file system using a custom document type matcher" in new ParserSetup {
    val dirname: String = getClass.getResource("/trees/a/").getFile
    def docView (num: Int, path: Path = Root) = DocumentView(path / s"doc$num.md", Content(List(p("Doc"+num))) :: Nil)
    val subtree1 = TreeView(Root / "dir1", List(Documents(Markup, List(docView(3, Root / "dir1"),docView(4, Root / "dir1")))))
    val subtree2 = TreeView(Root / "dir2", List(Documents(Markup, List(docView(5, Root / "dir2"),docView(6, Root / "dir2")))))
    val treeResult = TreeView(Root, List(
      Documents(Markup, List(docView(2))),
      Subtrees(List(subtree1,subtree2))
    ))
    val parser = parserWithBundle(BundleProvider.forDocTypeMatcher{ case Root / "doc1.md" => Ignored })
    viewOf(parser.fromDirectory(dirname).parse.unsafeRunSync().root.tree) should be (treeResult)
  }

  it should "allow to specify a custom exclude filter" in new ParserSetup {
    val dirname: String = getClass.getResource("/trees/a/").getFile
    def docView (num: Int, path: Path = Root) = DocumentView(path / s"doc$num.md", Content(List(p("Doc"+num))) :: Nil)
    val subtree2 = TreeView(Root / "dir2", List(Documents(Markup, List(docView(5, Root / "dir2"),docView(6, Root / "dir2")))))
    val treeResult = TreeView(Root, List(
      Documents(Markup, List(docView(2))),
      Subtrees(List(subtree2))
    ))
    viewOf(defaultParser.fromDirectory(dirname, {f:java.io.File => f.getName == "doc1.md" || f.getName == "dir1"}).parse.unsafeRunSync().root.tree) should be (treeResult)
  }
  
  it should "read a directory from the file system using the fromDirectories method" in new ParserSetup {
    val dir1 = new java.io.File(getClass.getResource("/trees/a/").getFile)
    val dir2 = new java.io.File(getClass.getResource("/trees/b/").getFile)
    def docView (num: Int, path: Path = Root) = DocumentView(path / s"doc$num.md", Content(List(p("Doc"+num))) :: Nil)
    val subtree1 = TreeView(Root / "dir1", List(Documents(Markup, List(docView(3, Root / "dir1"),docView(4, Root / "dir1"),docView(7, Root / "dir1")))))
    val subtree2 = TreeView(Root / "dir2", List(Documents(Markup, List(docView(5, Root / "dir2"),docView(6, Root / "dir2")))))
    val subtree3 = TreeView(Root / "dir3", List(Documents(Markup, List(docView(8, Root / "dir3")))))
    val treeResult = TreeView(Root, List(
      Documents(Markup, List(docView(1),docView(2),docView(9))),
      Subtrees(List(subtree1,subtree2,subtree3))
    ))
    viewOf(defaultParser.fromDirectories(Seq(dir1,dir2)).parse.unsafeRunSync().root.tree) should be (treeResult)
  }

  it should "read a directory from the file system containing a file with non-ASCII characters" in new ParserSetup {
    val dirname: String = getClass.getResource("/trees/c/").getFile
    def docView (num: Int, path: Path = Root) = DocumentView(path / s"doc$num.md", Content(List(p(s"Doc$num äöü"))) :: Nil)
    val treeResult = TreeView(Root, List(
      Documents(Markup, List(docView(1)))
    ))
    viewOf(defaultParser.fromDirectory(dirname).parse.unsafeRunSync().root.tree) should be (treeResult)
  }
  
}
