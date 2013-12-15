/*
 * Copyright 2013 the original author or authors.
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
import org.scalatest.matchers.ShouldMatchers
import laika.parse.markdown.Markdown
import laika.tree.Elements.ExternalLinkDefinition
import laika.tree.Elements.LinkReference
import laika.tree.Elements.Text
import laika.tree.helper.ModelBuilder
import laika.tree.helper.InputBuilder
import laika.io.InputProvider.InputConfigBuilder
import laika.tree.Documents._
import laika.tree.DocumentTreeHelper._
import laika.tree.Templates.TemplateRoot
import laika.tree.Templates.TemplateString
import laika.tree.Templates.TemplateDocument
import laika.template.ParseTemplate
import laika.io.Input
import laika.io.InputProvider.Directory

class ParseAPISpec extends FlatSpec 
                   with ShouldMatchers
                   with ModelBuilder 
                   with InputBuilder {

  
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
    ((Parse as Markdown asRawDocument) fromString input).content should be (root 
        (p (LinkReference(List(Text("link")), "id", "[link][id]")), ExternalLinkDefinition("id","http://foo/",None)))
  }
  
  def contents = Map(
    "name" -> "foo",
    "directive" -> "aa @:foo bar. bb",
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
  def docView (num: Int, path: Path = Root) = DocumentView(path / ("doc"+num+".md"), Content(List(p("foo"))) :: Nil)
  
  it should "allow parsing an empty tree" in {
    val dirs = ""
    val treeResult = TreeView(Root, Nil)
    viewOf(Parse as Markdown fromTree builder(dirs)) should be (treeResult)
  }
  
  it should "allow parsing a tree with a single document" in {
    val dirs = """- name.md:name"""
    val docResult = DocumentView(Root / "name.md", Content(List(p("foo"))) :: Nil)
    val treeResult = TreeView(Root, List(Documents(Markup, List(docResult))))
    viewOf(Parse as Markdown fromTree builder(dirs)) should be (treeResult)
  }
  
  it should "allow parsing a tree with a single subtree" in {
    val dirs = """+ subtree"""
    val subtree = TreeView(Root / "subtree", Nil)
    val treeResult = TreeView(Root, List(Subtrees(List(subtree))))
    viewOf(Parse as Markdown fromTree builder(dirs)) should be (treeResult)
  }
  
  it should "allow parsing a tree with multiple subtrees" in {
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
    viewOf(Parse as Markdown fromTree builder(dirs)) should be (treeResult)
  }
  
  it should "allow parsing a tree with a single template" in {
    val dirs = """- main.template.html:name"""
    val template = TemplateView(Root / "main.template.html", TemplateRoot(List(TemplateString("foo"))))
    val treeResult = TreeView(Root, List(TemplateDocuments(Template, List(template))))
    viewOf((Parse as Markdown asRawDocument) fromTree builder(dirs)) should be (treeResult)
  }
  
  it should "allow parsing a tree with a dynamic document populated by a config file in the directory" in {
    val dirs = """- main.dynamic.html:dynDoc
      |- directory.conf:conf""".stripMargin
    val dyn = DocumentView(Root / "main.html", List(Content(List(TemplateRoot(List(TemplateString("abc")))))))
    val treeResult = TreeView(Root, List(Documents(Dynamic, List(dyn))))
    viewOf(Parse as Markdown fromTree builder(dirs)) should be (treeResult)
  }
  
  it should "allow parsing a tree with a dynamic document populated by a root config string" in {
    val dirs = """- main.dynamic.html:dynDoc"""
    val dyn = DocumentView(Root / "main.html", List(Content(List(TemplateRoot(List(TemplateString("def")))))))
    val treeResult = TreeView(Root, List(Documents(Dynamic, List(dyn))))
    viewOf(Parse as Markdown fromTree builder(dirs).withConfigString("value: def")) should be (treeResult)
  }
  
  it should "allow parsing and rewriting a tree with a dynamic document" in {
    val dirs = """- main.dynamic.html:name"""
    val dyn = DocumentView(Root / "main.html", List(Content(List(TemplateRoot(List(TemplateString("foo")))))))
    val treeResult = TreeView(Root, List(Documents(Dynamic, List(dyn))))
    viewOf(Parse as Markdown fromTree builder(dirs)) should be (treeResult)
  }
  
  it should "allow parsing a tree with a static document" in {
    val dirs = """- omg.js:name"""
    val input = InputView("omg.js")
    val treeResult = TreeView(Root, List(Inputs(Static, List(input))))
    viewOf(Parse as Markdown fromTree builder(dirs)) should be (treeResult)
  }
  
  it should "ignore directories that match the default exclude pattern" in {
    val dirs = """- name.md:name
      |+ .git
      |  - file1:name
      |  - file2:name""".stripMargin
    val docResult = DocumentView(Root / "name.md", Content(List(p("foo"))) :: Nil)
    val treeResult = TreeView(Root, List(Documents(Markup, List(docResult))))
    viewOf(Parse as Markdown fromTree builder(dirs)) should be (treeResult)
  }
  
  it should "ignore files that match the default exclude pattern" in {
    val dirs = """- name.md:name
      |- foo.git:name""".stripMargin
    val docResult = DocumentView(Root / "name.md", Content(List(p("foo"))) :: Nil)
    val treeResult = TreeView(Root, List(Documents(Markup, List(docResult))))
    viewOf(Parse as Markdown fromTree builder(dirs)) should be (treeResult)
  }
  
  it should "allow parsing a tree with all available file types" in {
    val dirs = """- doc1.md:name
      |- doc2.md:name
      |- mainA.template.html:name
      |+ dir1
      |  - ignore.svn:name
      |  - mainB.template.html:name
      |  - doc3.md:name
      |  - doc4.md:name
      |+ dir2
      |  - main.dynamic.html:name
      |  - omg.js:name
      |  - doc5.md:name
      |  - doc6.md:name
      |+ .git
      |  - file1:name
      |  - file2:name""".stripMargin
    def template (char: Char, path: Path) = TemplateView(path / ("main"+char+".template.html"), TemplateRoot(List(TemplateString("foo"))))
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
      Documents(Markup, List(docView(1),docView(2))),
      TemplateDocuments(Template, List(template('A', Root))),
      Subtrees(List(subtree1,subtree2))
    ))
    viewOf((Parse as Markdown asRawDocument) fromTree builder(dirs)) should be (treeResult)
  }
  
  it should "allow to specify a custom document type matcher" in {
    val dirs = """- name.md:name
      |- main.dynamic.html:name""".stripMargin
    val treeResult = TreeView(Root, List(Inputs(Static, List(InputView("name.md"), InputView("main.dynamic.html")))))
    viewOf(Parse as Markdown fromTree builder(dirs).withDocTypeMatcher(_ => Static)) should be (treeResult)
  }
  
  it should "allow to specify a custom template engine" in {
    val parser: Input => TemplateDocument = 
      input => TemplateDocument(input.path, TemplateRoot(List(TemplateString("$$" + input.asParserInput.source))), null)
    val dirs = """- main1.template.html:name
      |- main2.template.html:name""".stripMargin
    def template (num: Int) = TemplateView(Root / ("main"+num+".template.html"), TemplateRoot(List(TemplateString("$$foo"))))
    val treeResult = TreeView(Root, List(TemplateDocuments(Template, List(template(1),template(2)))))
    viewOf((Parse as Markdown asRawDocument) fromTree builder(dirs).withTemplates(ParseTemplate as parser)) should be (treeResult)
  }
  
  it should "allow to specify a template directive" in {
    import laika.directive.Directives._
    import laika.directive.Directives.Templates
    import laika.directive.Directives.Templates.Combinators._
    
    val directive = Templates.create("foo") {
      attribute(Default) map { TemplateString(_) }
    }
    val dirs = """- main1.template.html:directive
      |- main2.template.html:directive""".stripMargin
    def template (num: Int) = TemplateView(Root / ("main"+num+".template.html"), tRoot(tt("aa "),tt("bar"),tt(" bb")))
    val treeResult = TreeView(Root, List(TemplateDocuments(Template, List(template(1),template(2)))))
    viewOf((Parse as Markdown asRawDocument) fromTree builder(dirs).withTemplateDirectives(directive)) should be (treeResult)
  }
  
  it should "allow to specify a custom navigation order" in {
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
    tree.navigatables map (_.path.name) should be (List("lemon.md","shapes","cherry.md","colors","apple.md","orange.md"))
  }
  
  
  it should "allow parallel parser execution" in {
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
    viewOf(Parse as Markdown fromTree builder(dirs).inParallel) should be (treeResult)
  }
  
  it should "read a directory from the file system using the fromDirectory method" in {
    val dirname = getClass.getResource("/trees/a/").getFile
    def docView (num: Int, path: Path = Root) = DocumentView(path / ("doc"+num+".md"), Content(List(p("Doc"+num))) :: Nil)
    val subtree1 = TreeView(Root / "dir1", List(Documents(Markup, List(docView(3, Root / "dir1"),docView(4, Root / "dir1")))))
    val subtree2 = TreeView(Root / "dir2", List(Documents(Markup, List(docView(5, Root / "dir2"),docView(6, Root / "dir2")))))
    val treeResult = TreeView(Root, List(
      Documents(Markup, List(docView(1),docView(2))),
      Subtrees(List(subtree1,subtree2))
    ))
    viewOf(Parse as Markdown fromDirectory(dirname)) should be (treeResult)
  }
  
  it should "read a directory from the file system using the Directory object" in {
    val dirname = getClass.getResource("/trees/a/").getFile
    def docView (num: Int, path: Path = Root) = DocumentView(path / ("doc"+num+".md"), Content(List(p("Doc"+num))) :: Nil)
    val subtree1 = TreeView(Root / "dir1", List(Documents(Markup, List(docView(3, Root / "dir1"),docView(4, Root / "dir1")))))
    val subtree2 = TreeView(Root / "dir2", List(Documents(Markup, List(docView(5, Root / "dir2"),docView(6, Root / "dir2")))))
    val treeResult = TreeView(Root, List(
      Documents(Markup, List(docView(1),docView(2))),
      Subtrees(List(subtree1,subtree2))
    ))
    viewOf(Parse as Markdown fromTree(Directory(dirname))) should be (treeResult)
  }

}