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

import java.io.ByteArrayInputStream
import java.io.StringReader
import scala.io.Codec
import scala.io.Codec.charset2codec
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers
import laika.io.DocumentType
import laika.io.DocumentType._
import laika.parse.markdown.Markdown
import laika.parse.css.ParseStyleSheet
import laika.parse.css.Styles.StyleDeclarationSet
import laika.parse.css.Styles.StyleDeclaration
import laika.parse.css.Styles.ElementType
import laika.parse.css.Styles.Selector
import laika.parse.rst.ReStructuredText
import laika.parse.rst.Elements.CustomizedTextRole
import laika.tree.Elements.ExternalLinkDefinition
import laika.tree.Elements.ExternalLink
import laika.tree.Elements.LinkReference
import laika.tree.Elements.Text
import laika.tree.Elements.Block
import laika.tree.helper.ModelBuilder
import laika.tree.helper.InputBuilder
import laika.io.InputProvider.InputConfigBuilder
import laika.tree.Documents._
import laika.tree.Paths.Path
import laika.tree.Paths.Root
import laika.tree.helper.DocumentViewBuilder._
import laika.tree.Templates.TemplateRoot
import laika.tree.Templates.TemplateString
import laika.rewrite.TemplateRewriter
import laika.template.ParseTemplate
import laika.io.Input
import laika.io.InputProvider.Directory


class ParseAPISpec extends FlatSpec 
                   with Matchers
                   with ModelBuilder {

  
  "The Parse API" should "allow parsing Markdown from a string" in {
    val input = """aaa
      |bbb
      |ccc""".stripMargin
    (Parse as Markdown fromString input).content should be (root(p(input)))
  }
  
  it should "allow parsing Markdown from a file" in {
    val input = """aaa
      |bbb
      |ccc""".stripMargin
    val filename = getClass.getResource("/testInput.md").getFile
    (Parse as Markdown fromFile filename).content should be (root(p(input))) 
  }
  
  it should "allow parsing Markdown from a java.io.Reader instance" in {
    val input = """aaa
      |bbb
      |ccc""".stripMargin
    val reader = new StringReader(input)
    (Parse as Markdown fromReader reader).content should be (root(p(input)))
  }
  
  it should "allow parsing Markdown from a java.io.InputStream instance" in {
    val input = """aaa
      |bbb
      |ccc""".stripMargin
    val stream = new ByteArrayInputStream(input.getBytes())
    (Parse as Markdown fromStream stream).content should be (root(p(input)))
  }
  
  it should "allow parsing Markdown from a java.io.InputStream instance, specifying the encoding explicitly" in {
    val input = """äää
      |ööö
      |üüü""".stripMargin
    val stream = new ByteArrayInputStream(input.getBytes("ISO-8859-1"))
    (Parse as Markdown).fromStream(stream)(Codec.ISO8859).content should be (root(p(input)))
  }
  
  it should "allow parsing Markdown from a java.io.InputStream instance, specifying the encoding implicitly" in {
    val input = """äää
      |ööö
      |üüü""".stripMargin
    val stream = new ByteArrayInputStream(input.getBytes("ISO-8859-1"))
    implicit val codec:Codec = Codec.ISO8859
    (Parse as Markdown fromStream stream).content should be (root(p(input)))
  }
  
  it should "allow parsing Markdown with all link references resolved through the default rewrite rules" in {
    val input = """[link][id]
      |
      |[id]: http://foo/""".stripMargin
    (Parse as Markdown fromString input).content should be (root(p(link(txt("link")).url("http://foo/"))))
  }
  
  it should "allow parsing Markdown into a raw document, without applying the default rewrite rules" in {
    val input = """[link][id]
      |
      |[id]: http://foo/""".stripMargin
    ((Parse as Markdown withoutRewrite) fromString input).content should be (root 
        (p (LinkReference(List(Text("link")), "id", "[link][id]")), ExternalLinkDefinition("id","http://foo/",None)))
  }
  
  trait TreeParser extends InputBuilder {
    
    def dirs: String 
    
    def contents = Map(
      "link" -> "[link](foo)",
      "name" -> "foo",
      "multiline" -> """aaa
        |
        |bbb""".stripMargin,
      "directive" -> "aa @:foo bar. bb",
      "template" -> """<div>
        |  {{document.content}}
        |</div>""".stripMargin,
      "template2" -> """<div>
        |xx{{document.content}}
        |</div>""".stripMargin,
      "dynDoc" -> "{{config.value}}",
      "conf" -> "value: abc",
      "order" -> """navigationOrder: [
        |  lemon.md
        |  shapes
        |  cherry.md
        |  colors
        |  apple.md
        |  orange.md
        |]""".stripMargin
    )
    
    def builder (source: String) = new InputConfigBuilder(parseTreeStructure(source), Codec.UTF8)
    def docView (num: Int, path: Path = Root) = DocumentView(path / (s"doc$num.md"), Content(List(p("foo"))) :: Nil)
    
    def customDocView (name: String, content: Seq[Block], path: Path = Root) = DocumentView(path / name, Content(content) :: Nil)
  
    def withTemplatesApplied (tree: DocumentTree): DocumentTree = TemplateRewriter.applyTemplates(tree, "html")
    
    def parsedTree = viewOf(withTemplatesApplied(Parse as Markdown fromTree builder(dirs)))
    
    def rawParsedTree = viewOf((Parse as Markdown withoutRewrite) fromTree builder(dirs))

    def rawMixedParsedTree = viewOf((Parse as Markdown or ReStructuredText withoutRewrite) fromTree builder(dirs))
    
    def parsedWith (f: InputConfigBuilder => InputConfigBuilder) =
      viewOf(withTemplatesApplied(Parse as Markdown fromTree f(builder(dirs))))
      
    def parsedRawWith (f: InputConfigBuilder => InputConfigBuilder) =
      viewOf((Parse as Markdown withoutRewrite) fromTree f(builder(dirs)))
  }
  

  
  it should "allow parsing an empty tree" in {
    new TreeParser {
      val dirs = ""
      val treeResult = TreeView(Root, Nil)
      parsedTree should be (treeResult)
    }
  }
  
  it should "allow parsing a tree with a single document" in {
    new TreeParser {
      val dirs = """- name.md:name"""
      val docResult = DocumentView(Root / "name.md", Content(List(p("foo"))) :: Nil)
      val treeResult = TreeView(Root, List(Documents(Markup, List(docResult))))
      parsedTree should be (treeResult)
    }
  }
  
  it should "allow parsing a tree with a single subtree" in {
    new TreeParser {
      val dirs = """+ subtree"""
      val subtree = TreeView(Root / "subtree", Nil)
      val treeResult = TreeView(Root, List(Subtrees(List(subtree))))
      parsedTree should be (treeResult)
    }
  }
  
  it should "allow parsing a tree with multiple subtrees" in {
    new TreeParser {
      val dirs = """- doc1.md:name
        |- doc2.md:name
        |+ dir1
        |  - doc3.md:name
        |  - doc4.md:name
        |+ dir2
        |  - doc5.md:name
        |  - doc6.md:name""".stripMargin
      val subtree1 = TreeView(Root / "dir1", List(Documents(Markup, List(docView(3, Root / "dir1"),docView(4, Root / "dir1")))))
      val subtree2 = TreeView(Root / "dir2", List(Documents(Markup, List(docView(5, Root / "dir2"),docView(6, Root / "dir2")))))
      val treeResult = TreeView(Root, List(
        Documents(Markup, List(docView(1),docView(2))),
        Subtrees(List(subtree1,subtree2))
      ))
      parsedTree should be (treeResult)
    }
  }
  
  it should "allow parsing a tree with a single template" in {
    new TreeParser {
      val dirs = """- main.template.html:name"""
      val template = TemplateView(Root / "main.template.html", TemplateRoot(List(TemplateString("foo"))))
      val treeResult = TreeView(Root, List(TemplateDocuments(Template, List(template))))
      rawParsedTree should be (treeResult)
    }
  }
  
  it should "allow parsing a tree with a dynamic document populated by a config file in the directory" in {
    new TreeParser {
      val dirs = """- main.dynamic.html:dynDoc
        |- directory.conf:conf""".stripMargin
      val dyn = DocumentView(Root / "main.html", List(Content(List(TemplateRoot(List(TemplateString("abc")))))))
      val treeResult = TreeView(Root, List(Documents(Dynamic, List(dyn))))
      parsedTree should be (treeResult)
    }
  }
  
  it should "allow parsing a tree with a dynamic document populated by a root config string" in {
    new TreeParser {
      val dirs = """- main.dynamic.html:dynDoc"""
      val dyn = DocumentView(Root / "main.html", List(Content(List(TemplateRoot(List(TemplateString("def")))))))
      val treeResult = TreeView(Root, List(Documents(Dynamic, List(dyn))))
      parsedWith(_.withConfigString("value: def")) should be (treeResult)
    }
  }
  
  it should "allow parsing and rewriting a tree with a dynamic document" in {
    new TreeParser {
      val dirs = """- main.dynamic.html:name"""
      val dyn = DocumentView(Root / "main.html", List(Content(List(TemplateRoot(List(TemplateString("foo")))))))
      val treeResult = TreeView(Root, List(Documents(Dynamic, List(dyn))))
      parsedTree should be (treeResult)
    }
  }
  
  it should "allow parsing a tree with a static document" in {
    new TreeParser {
      val dirs = """- omg.js:name"""
      val input = InputView("omg.js")
      val treeResult = TreeView(Root, List(Inputs(Static, List(input))))
      parsedTree should be (treeResult)
    }
  }
  
  it should "allow parsing a tree with all available file types" in {
    new TreeParser {
      val dirs = """- doc1.md:link
        |- doc2.rst:link
        |- mainA.template.html:name
        |+ dir1
        |  - mainB.template.html:name
        |  - doc3.md:name
        |  - doc4.md:name
        |+ dir2
        |  - main.dynamic.html:name
        |  - omg.js:name
        |  - doc5.md:name
        |  - doc6.md:name""".stripMargin
      def template (char: Char, path: Path) = TemplateView(path / (s"main$char.template.html"), TemplateRoot(List(TemplateString("foo"))))
      val dyn = TemplateView(Root / "dir2" / "main.dynamic.html", TemplateRoot(List(TemplateString("foo"))))
      val subtree1 = TreeView(Root / "dir1", List(
        Documents(Markup, List(docView(3, Root / "dir1"),docView(4, Root / "dir1"))),
        TemplateDocuments(Template, List(template('B', Root / "dir1")))
      ))
      val subtree2 = TreeView(Root / "dir2", List(
        Documents(Markup, List(docView(5, Root / "dir2"),docView(6, Root / "dir2"))),
        TemplateDocuments(Dynamic, List(dyn)),
        Inputs(Static, List(InputView("omg.js")))
      ))
      val treeResult = TreeView(Root, List(
        Documents(Markup, List(customDocView("doc1.md", Seq(p(ExternalLink(Seq(txt("link")), "foo")))),customDocView("doc2.rst", Seq(p("[link](foo)"))))),
        TemplateDocuments(Template, List(template('A', Root))),
        Subtrees(List(subtree1,subtree2))
      ))
      rawMixedParsedTree.rewrite{case CustomizedTextRole(_,_,_) => None} should be (treeResult)
    }
  }
  
  it should "allow to specify a custom document type matcher" in {
    new TreeParser {
      val dirs = """- name.md:name
        |- main.dynamic.html:name""".stripMargin
      val treeResult = TreeView(Root, List(Inputs(Static, List(InputView("name.md"), InputView("main.dynamic.html")))))
      parsedWith(_.withDocTypeMatcher(_ => Static)) should be (treeResult)
    }
  }
  
  it should "allow to specify a custom template engine" in {
    new TreeParser {
      val parser: Input => TemplateDocument = 
        input => TemplateDocument(input.path, TemplateRoot(List(TemplateString("$$" + input.asParserInput.input))))
      val dirs = """- main1.template.html:name
        |- main2.template.html:name""".stripMargin
      def template (num: Int) = TemplateView(Root / (s"main$num.template.html"), TemplateRoot(List(TemplateString("$$foo"))))
      val treeResult = TreeView(Root, List(TemplateDocuments(Template, List(template(1),template(2)))))
      parsedRawWith(_.withTemplateParser(ParseTemplate as parser)) should be (treeResult)
    }
  }
  
  it should "allow to specify a custom style sheet engine" in {
    new TreeParser {
      def docTypeMatcher (path: Path): DocumentType = {
        val Stylesheet = """.+\.([a,b]+).css$""".r
        path.name match {
          case Stylesheet(kind) => StyleSheet(kind)
        }
      }
      def styleDecl(styleName: String, order: Int = 0) =
        StyleDeclaration(ElementType("Type"), styleName -> "foo").increaseOrderBy(order)
      val parser: Input => StyleDeclarationSet = input =>
        StyleDeclarationSet(input.path, styleDecl(input.name.takeWhile(_ != '.')))
      val dirs = """- main1.aaa.css:name
        |- main2.bbb.css:name
        |- main3.aaa.css:name""".stripMargin
      val treeResult = TreeView(Root, List(StyleSheets(Map(
          "aaa" -> StyleDeclarationSet(Set(Path("/main1.aaa.css"), Path("/main3.aaa.css")), Set(styleDecl("main1"), styleDecl("main3", 1))),
          "bbb" -> StyleDeclarationSet(Set(Path("/main2.bbb.css")), Set(styleDecl("main2")))
      ))))
      parsedRawWith(_.withStyleSheetParser(ParseStyleSheet as parser).withDocTypeMatcher(docTypeMatcher)) should be (treeResult)
    }
  }
  
  it should "allow to specify a template directive" in {
    new TreeParser {
      import laika.directive.Directives._
      import laika.directive.Directives.Templates
      import laika.directive.Directives.Templates.Combinators._

      val directive = Templates.create("foo") {
        attribute(Default) map { TemplateString(_) }
      }
      val dirs = """- main1.template.html:directive
        |- main2.template.html:directive""".stripMargin
      def template (num: Int) = TemplateView(Root / (s"main$num.template.html"), tRoot(tt("aa "),tt("bar"),tt(" bb")))
      val treeResult = TreeView(Root, List(TemplateDocuments(Template, List(template(1),template(2)))))
      parsedRawWith(_.withTemplateDirectives(directive)) should be (treeResult)
    }
  }
  
  it should "add indentation information if an embedded root is preceded by whitespace characters" in {
    new TreeParser {
      import laika.tree.Templates.EmbeddedRoot
      val dirs = """- default.template.html:template
        |- doc.md:multiline""".stripMargin
      val docResult = DocumentView(Root / "doc.md", Content(List(tRoot(
          tt("<div>\n  "),
          EmbeddedRoot(List(p("aaa"),p("bbb")), 2),
          tt("\n</div>")
      ))) :: Nil)
      val treeResult = TreeView(Root, List(Documents(Markup, List(docResult))))
      parsedTree should be (treeResult)
    }
  }
  
  it should "not add indentation information if an embedded root is preceded by non-whitespace characters" in {
    new TreeParser {
      import laika.tree.Templates.EmbeddedRoot
      val dirs = """- default.template.html:template2
        |- doc.md:multiline""".stripMargin
      val docResult = DocumentView(Root / "doc.md", Content(List(tRoot(
        tt("<div>\nxx"),
        EmbeddedRoot(List(p("aaa"),p("bbb")), 0),
        tt("\n</div>")
      ))) :: Nil)
      val treeResult = TreeView(Root, List(Documents(Markup, List(docResult))))
      parsedTree should be (treeResult)
    }
  }
  
  it should "allow to specify a custom navigation order" in {
    new TreeParser {
      val dirs = """- apple.md:name
        |- orange.md:name
        |+ colors
        |  - green.md:name
        |- lemon.md:name
        |+ shapes
        |  - rectangle.md:name
        |- cherry.md:name
        |- directory.conf:order""".stripMargin
      val tree = Parse as Markdown fromTree builder(dirs)
      tree.content map (_.path.name) should be (List("lemon.md","shapes","cherry.md","colors","apple.md","orange.md"))
    }
  }
  
  it should "allow parallel parser execution" in {
    new TreeParser {
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
      val subtree1 = TreeView(Root / "dir1", List(Documents(Markup, List(docView(3, Root / "dir1"),docView(4, Root / "dir1"),docView(5, Root / "dir1")))))
      val subtree2 = TreeView(Root / "dir2", List(Documents(Markup, List(docView(6, Root / "dir2"),docView(7, Root / "dir2"),docView(8, Root / "dir2")))))
      val treeResult = TreeView(Root, List(
        Documents(Markup, List(docView(1),docView(2))),
        Subtrees(List(subtree1,subtree2))
      ))
      parsedWith(_.inParallel) should be (treeResult)
    }
  }
  
  it should "read a directory from the file system using the fromDirectory method" in {
    val dirname = getClass.getResource("/trees/a/").getFile
    def docView (num: Int, path: Path = Root) = DocumentView(path / (s"doc$num.md"), Content(List(p("Doc"+num))) :: Nil)
    val subtree1 = TreeView(Root / "dir1", List(Documents(Markup, List(docView(3, Root / "dir1"),docView(4, Root / "dir1")))))
    val subtree2 = TreeView(Root / "dir2", List(Documents(Markup, List(docView(5, Root / "dir2"),docView(6, Root / "dir2")))))
    val treeResult = TreeView(Root, List(
      Documents(Markup, List(docView(1),docView(2))),
      Subtrees(List(subtree1,subtree2))
    ))
    viewOf(Parse as Markdown fromDirectory(dirname)) should be (treeResult)
  }
  
  it should "read a directory from the file system using the fromDirectories method" in {
    val dir1 = new java.io.File(getClass.getResource("/trees/a/").getFile)
    val dir2 = new java.io.File(getClass.getResource("/trees/b/").getFile)
    def docView (num: Int, path: Path = Root) = DocumentView(path / (s"doc$num.md"), Content(List(p("Doc"+num))) :: Nil)
    val subtree1 = TreeView(Root / "dir1", List(Documents(Markup, List(docView(3, Root / "dir1"),docView(4, Root / "dir1"),docView(7, Root / "dir1")))))
    val subtree2 = TreeView(Root / "dir2", List(Documents(Markup, List(docView(5, Root / "dir2"),docView(6, Root / "dir2")))))
    val subtree3 = TreeView(Root / "dir3", List(Documents(Markup, List(docView(8, Root / "dir3")))))
    val treeResult = TreeView(Root, List(
      Documents(Markup, List(docView(1),docView(2),docView(9))),
      Subtrees(List(subtree1,subtree2,subtree3))
    ))
    viewOf(Parse as Markdown fromDirectories(Seq(dir1,dir2))) should be (treeResult)
  }

  it should "read a directory from the file system containing a file with non-ASCII characters" in {
    val dirname = getClass.getResource("/trees/c/").getFile
    def docView (num: Int, path: Path = Root) = DocumentView(path / (s"doc$num.md"), Content(List(p(s"Doc$num äöü"))) :: Nil)
    val treeResult = TreeView(Root, List(
      Documents(Markup, List(docView(1)))
    ))
    viewOf(Parse as Markdown fromDirectory(dirname)) should be (treeResult)
  }
  
  it should "allow to specify a custom exclude filter" in {
    val dirname = getClass.getResource("/trees/a/").getFile
    def docView (num: Int, path: Path = Root) = DocumentView(path / (s"doc$num.md"), Content(List(p("Doc"+num))) :: Nil)
    val subtree2 = TreeView(Root / "dir2", List(Documents(Markup, List(docView(5, Root / "dir2"),docView(6, Root / "dir2")))))
    val treeResult = TreeView(Root, List(
      Documents(Markup, List(docView(2))),
      Subtrees(List(subtree2))
    ))
    viewOf(Parse as Markdown fromDirectory(dirname, {f:java.io.File => f.getName == "doc1.md" || f.getName == "dir1"})) should be (treeResult)
  }
  
  it should "read a directory from the file system using the Directory object" in {
    val dirname = getClass.getResource("/trees/a/").getFile
    def docView (num: Int, path: Path = Root) = DocumentView(path / (s"doc$num.md"), Content(List(p("Doc"+num))) :: Nil)
    val subtree1 = TreeView(Root / "dir1", List(Documents(Markup, List(docView(3, Root / "dir1"),docView(4, Root / "dir1")))))
    val subtree2 = TreeView(Root / "dir2", List(Documents(Markup, List(docView(5, Root / "dir2"),docView(6, Root / "dir2")))))
    val treeResult = TreeView(Root, List(
      Documents(Markup, List(docView(1),docView(2))),
      Subtrees(List(subtree1,subtree2))
    ))
    viewOf(Parse as Markdown fromTree(Directory(dirname))) should be (treeResult)
  }

}
