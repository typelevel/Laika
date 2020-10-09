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

package laika.directive

import cats.data.NonEmptySet
import cats.implicits._
import laika.api.MarkupParser
import laika.api.builder.OperationConfig
import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentTree
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.config._
import laika.format.Markdown
import laika.parse.SourceCursor
import laika.rewrite.nav.{ChoiceConfig, SelectionConfig, Selections, TargetFormats}
import laika.rewrite.{DefaultTemplatePath, TemplateContext, TemplateRewriter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class StandardDirectiveSpec extends AnyFlatSpec
  with Matchers
  with ModelBuilder {


  lazy val templateParser = StandardDirectives.processExtension(DirectiveSupport).parsers.templateParser.get
  lazy val markupParser = MarkupParser.of(Markdown).failOnMessages(MessageFilter.None).build

  def parse (input: String, path: Path = Root / "doc"): Document = markupParser.parse(input, path).toOption.get
  def parseUnresolved (input: String, path: Path = Root / "doc"): Document = markupParser.parseUnresolved(input, path).toOption.get.document

  def parseWithFragments (input: String, path: Path = Root / "doc"): (Map[String,Element], RootElement) = {
    val doc = parse(input, path)
    (doc.fragments, doc.content)
  }

  def parseTemplate (input: String): TemplateRoot = templateParser.parse(SourceCursor(input)).toOption.get

  def parseTemplateWithConfig (input: String, config: String): RootElement = {
    val tRoot = parseTemplate(input)
    val template = TemplateDocument(Path.Root / "theme" / "test.template.html", tRoot)
    val cursor = DocumentCursor(Document(Path.Root / "docs" / "doc1.md", root(), config = ConfigParser.parse(config).resolve().toOption.get))
    TemplateRewriter.applyTemplate(cursor, template).toOption.get.content
  }


  "The fragment directive" should "parse a fragment with a single paragraph" in {
    val input = """aa
                  |
                  |@:fragment(foo)
                  |
                  |Fragment Text
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    parseWithFragments(input) should be ((Map("foo" -> Paragraph(List(Text("Fragment Text")),Styles("foo"))),root(p("aa"),p("bb"))))
  }

  it should "parse a fragment with a two paragraphs" in {
    val input = """aa
                  |
                  |@:fragment(foo)
                  |
                  |Line 1
                  |
                  |Line 2
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    parseWithFragments(input) should be ((Map("foo" -> BlockSequence(List(p("Line 1"), p("Line 2")),Styles("foo"))),root(p("aa"),p("bb"))))
  }


  "The pageBreak directive" should "parse an empty directive" in {
    val input = """aa
                  |
                  |@:pageBreak
                  |
                  |bb""".stripMargin
    parse(input).content should be (root(p("aa"),PageBreak(),p("bb")))
  }


  "The todo directive" should "parse a block directive" in {
    val input = """aa
                  |
                  |@:todo(FIXME LATER)
                  |
                  |bb""".stripMargin
    parse(input).content should be (root(p("aa"),BlockSequence(Nil),p("bb")))
  }

  it should "parse a span directive" in {
    val input = """aa @:todo(FIXME LATER) bb"""
    parse(input).content should be (root(p(Text("aa "),SpanSequence(Nil),Text(" bb"))))
  }


  "The relativePath directive" should "translate a relative path" in {
    val input = """aa @:relativePath(theme.css) bb"""
    parseTemplateWithConfig(input, "") should be (root(TemplateRoot(
      t("aa "),
      TemplateString("../theme/theme.css"),
      t(" bb")
    )))
  }

  it should "translate an absolute path" in {
    val input = """aa @:relativePath(/theme/theme.css) bb"""
    parseTemplateWithConfig(input, "") should be (root(TemplateRoot(
      t("aa "),
      TemplateString("../theme/theme.css"),
      t(" bb")
    )))
  }


  "The style directive" should "parse a body with a single block" in {
    val input = """aa
                  |
                  |@:style(foo)
                  |
                  |11
                  |22
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    parse(input).content should be (root(p("aa"), Paragraph(List(Text("11\n22")),Styles("foo")), p("bb")))
  }

  it should "parse a body with two blocks" in {
    val input = """aa
                  |
                  |@:style(foo)
                  |
                  |11
                  |22
                  |
                  |33
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    parse(input).content should be (root(p("aa"), BlockSequence(List(p("11\n22"),p("33")),Styles("foo")), p("bb")))
  }

  "The callout directive" should "parse a body with a single block" in {
    val input = """aa
                  |
                  |@:callout(info)
                  |
                  |11
                  |22
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    parse(input).content should be (root(p("aa"), BlockSequence("11\n22").withStyles("callout", "info"), p("bb")))
  }

  it should "parse a body with two blocks" in {
    val input = """aa
                  |
                  |@:callout(info)
                  |
                  |11
                  |22
                  |
                  |33
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    parse(input).content should be (root(p("aa"), BlockSequence(List(p("11\n22"),p("33")),Styles("callout", "info")), p("bb")))
  }

  it should "parse a single nested span" in {
    val input = """aa @:style(foo) 11 @:@ bb"""
    parse(input).content should be (root(p(Text("aa "), Text(" 11 ", Styles("foo")), Text(" bb"))))
  }

  it should "parse two nested spans" in {
    val input = """aa @:style(foo) 11 *22* 33 @:@ bb"""
    parse(input).content should be (root(p(Text("aa "), SpanSequence(List(Text(" 11 "),Emphasized("22"),Text(" 33 ")),Styles("foo")), Text(" bb"))))
  }

  trait ChoiceSetup {
    val parser = MarkupParser
      .of(Markdown)
      .failOnMessages(MessageFilter.None)
      .withConfigValue(Selections(
        SelectionConfig("config",
          ChoiceConfig("a", "label-a"),
          ChoiceConfig("b", "label-b")
        )
      ))
      .build

    def parse (input: String): RootElement = parser.parse(input).toOption.get.content
  }

  "The select directive" should "parse a body with a two alternatives" in new ChoiceSetup {
    val input = """aa
                  |
                  |@:select(config)
                  |
                  |@:choice(a)
                  |11
                  |22
                  |
                  |@:choice(b)
                  |33
                  |44
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    val group = Selection("config", Seq(
      Choice("a","label-a", List(p("11\n22"))),
      Choice("b","label-b", List(p("33\n44")))
    ))
    parse(input) should be (root(p("aa"), group, p("bb")))
  }

  it should "parse a body with a two alternatives and a common body" in new ChoiceSetup {
    val input = """aa
                  |
                  |@:select(config)
                  |
                  |common
                  |
                  |@:choice(a)
                  |11
                  |22
                  |
                  |@:choice(b)
                  |33
                  |44
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    val group = Selection("config", Seq(
      Choice("a","label-a", List(p("common"), p("11\n22"))),
      Choice("b","label-b", List(p("common"), p("33\n44")))
    ))
    parse(input) should be (root(p("aa"), group, p("bb")))
  }

  it should "fail with less than two alternatives in the body" in new ChoiceSetup {
    val directive =
      """@:select(config)
        |
        |@:choice(a)
        |11
        |22
        |
        |@:@""".stripMargin
    val input = s"""aa
                   |
                  |$directive
                   |
                  |bb""".stripMargin
    val message = "One or more errors processing directive 'select': too few occurrences of separator directive 'choice': expected min: 2, actual: 1"
    val invalid = InvalidBlock(message, source(directive, input))
    parse(input) should be (root(p("aa"), invalid, p("bb")))
  }

  it should "fail when a label is missing in the configuration" in new ChoiceSetup {
    val directive =
      """@:select(config)
        |
        |@:choice(a)
        |11
        |22
        |
        |@:choice(c)
        |33
        |44
        |
        |@:@""".stripMargin
    val input =
      s"""aa
         |
         |$directive
         |
         |bb""".stripMargin
    val message = "One or more errors processing directive 'select': No label defined for choice 'c' in selection 'config'"
    val invalid = InvalidBlock(message, source(directive, input))
    parse(input) should be (root(p("aa"),
      invalid, p("bb")))
  }

  it should "unwrap a selected choice in the template rewrite rules" in {
    val group = Selection("config", Seq(
      Choice("a","label-a", List(p("common"), p("11\n22"))),
      Choice("b","label-b", List(p("common"), p("33\n44")))
    ))
    val config = Selections(
      SelectionConfig("config", ChoiceConfig("a", "label-a"), ChoiceConfig("b", "label-b", selected = true))
    )
    val doc = Document(Root / "doc", root(group))
    val tree = DocumentTreeRoot(DocumentTree(Root, Seq(doc), config = ConfigBuilder.empty.withValue(config).build))
    val cursor = DocumentCursor(doc, TreeCursor(RootCursor(tree)), tree.config, TreePosition(Nil))
    val rewritten = TemplateRewriter.applyTemplate(cursor, TemplateDocument(Root, TemplateRoot.fallback))
    rewritten.map(_.content) shouldBe Right(root(BlockSequence(List(p("common"), p("33\n44")))))
  }


  "The format directive" should "parse a body with a single paragraph" in {
    val input = """aa
                  |
                  |@:format(foo)
                  |
                  |11
                  |22
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    parse(input).content should be (root(p("aa"), TargetFormat(NonEmptySet.one("foo"), p("11\n22")), p("bb")))
  }

  it should "parse a body with two paragraphs" in {
    val input = """aa
                  |
                  |@:format(foo)
                  |
                  |11
                  |22
                  |
                  |33
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    parse(input).content should be (root(p("aa"), TargetFormat(NonEmptySet.one("foo"), BlockSequence(p("11\n22"),p("33"))), p("bb")))
  }

  it should "parse a directive with multiple formats" in {
    val input = """aa
                  |
                  |@:format(foo, bar, baz)
                  |
                  |11
                  |22
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    parse(input).content should be (root(p("aa"), TargetFormat(NonEmptySet.of("foo", "bar", "baz"), p("11\n22")), p("bb")))
  }


  "The for directive" should "process the default body once if the referenced object is a map" in {
    val input = """aaa @:for(person) ${_.name} ${_.age} @:@ bbb"""
    val config = "person: { name: Mary, age: 35 }"
    parseTemplateWithConfig(input, config) should be (root(TemplateRoot(
      t("aaa "),
      TemplateSpanSequence(
        t(" "),t("Mary"),t(" "),t("35"),t(" ")
      ),
      t(" bbb")
    )))
  }

  it should "process the default body multiple times if the referenced object is a list" in {
    val input = """aaa @:for(persons) ${_.name} ${_.age} @:@ bbb"""
    val config = "persons: [{ name: Mary, age: 35 },{ name: Lucy, age: 32 },{ name: Anna, age: 42 }]"
    parseTemplateWithConfig(input, config) should be (root(TemplateRoot(
      t("aaa "),
      TemplateSpanSequence(
        TemplateSpanSequence(
          t(" "),t("Mary"),t(" "),t("35"),t(" ")
        ),
        TemplateSpanSequence(
          t(" "),t("Lucy"),t(" "),t("32"),t(" ")
        ),
        TemplateSpanSequence(
          t(" "),t("Anna"),t(" "),t("42"),t(" ")
        )
      ),
      t(" bbb")
    )))
  }

  it should "not process the default body if the referenced object is an empty collection" in {
    val input = """aaa @:for(persons) ${_.name} ${_.age} @:@ bbb"""
    val config = "persons: []"
    parseTemplateWithConfig(input, config) should be (root(TemplateRoot(
      t("aaa "),
      TemplateSpanSequence.empty,
      t(" bbb")
    )))
  }

  it should "process the @:empty body part if the referenced object is an empty collection" in {
    val input = """aaa @:for(persons) ${_.name} ${_.age} @:empty none @:@ bbb"""
    val config = "persons: []"
    parseTemplateWithConfig(input, config) should be (root(TemplateRoot(
      t("aaa "),
      TemplateSpanSequence(" none "),
      t(" bbb")
    )))
  }

  it should "process the default body once if the referenced object is a scalar value" in {
    val input = """aaa @:for(person) ${_} @:@ bbb"""
    val config = "person: Mary"
    parseTemplateWithConfig(input, config) should be (root(TemplateRoot(
      t("aaa "),
      TemplateSpanSequence(t(" "),t("Mary"),t(" ")),
      t(" bbb")
    )))
  }

  "The if directive" should "process the default body once if the referenced object is the string 'true'" in {
    val input = """aaa @:if(monday) text @:@ bbb"""
    val config = "monday: true"
    parseTemplateWithConfig(input, config) should be (root(TemplateRoot(
      t("aaa "),
      TemplateSpanSequence(" text "),
      t(" bbb")
    )))
  }

  it should "process the default body once if the referenced object is the string 'on'" in {
    val input = """aaa @:if(monday) text @:@ bbb"""
    val config = "monday: on"
    parseTemplateWithConfig(input, config) should be (root(TemplateRoot(
      t("aaa "),
      TemplateSpanSequence(" text "),
      t(" bbb")
    )))
  }

  it should "not process the default body if the referenced object does not exist" in {
    val input = """aaa @:if(monday) text @:@ bbb"""
    val config = "tuesday: on"
    parseTemplateWithConfig(input, config) should be (root(TemplateRoot(
      t("aaa "),
      TemplateSpanSequence.empty,
      t(" bbb")
    )))
  }

  it should "process the @:else body if the referenced object does not exist" in {
    val input = """aaa @:if(monday) text @:else none @:@ bbb"""
    val config = "tuesday: on"
    parseTemplateWithConfig(input, config) should be (root(TemplateRoot(
      t("aaa "),
      TemplateSpanSequence(" none "),
      t(" bbb")
    )))
  }

  it should "process the first @:elseIf body if it is defined" in {
    val input = """111 @:if(aaa) aaa @:elseIf(bbb) bbb @:elseIf(ccc) ccc @:else none @:@ 222"""
    val config = "bbb: on"
    parseTemplateWithConfig(input, config) should be (root(TemplateRoot(
      t("111 "),
      TemplateSpanSequence(" bbb "),
      t(" 222")
    )))
  }

  it should "process the second @:elseIf body if it is defined" in {
    val input = """111 @:if(aaa) aaa @:elseIf(bbb) bbb @:elseIf(ccc) ccc @:else none @:@ 222"""
    val config = "ccc: on"
    parseTemplateWithConfig(input, config) should be (root(TemplateRoot(
      t("111 "),
      TemplateSpanSequence(" ccc "),
      t(" 222")
    )))
  }

  it should "not process the default body if the referenced object is not a string recognized as true" in {
    val input = """aaa @:if(monday) text @:@ bbb"""
    val config = "monday: off"
    parseTemplateWithConfig(input, config) should be (root(TemplateRoot(
      t("aaa "),
      TemplateSpanSequence.empty,
      t(" bbb")
    )))
  }

  trait ApiDirectiveSetup {
    def input (typeName: String): String = blockInput(s"aa @:api($typeName) bb")

    def blockInput (block: String): String =
      s"""{%
         |  laika.links.api = [
         |    { baseUri = "https://default.api/" },
         |    { baseUri = "https://foo.api/", packagePrefix = foo },
         |    { baseUri = "https://bar.api/", packagePrefix = foo.bar }
         |    { baseUri = "local/path/", packagePrefix = internal }
         |  ]
         |%}
         |
        |$block
      """.stripMargin
  }

  "The api directive" should "create a span link based on the default base URI" in new ApiDirectiveSetup {
    parse(input("def.bar.Baz")).content should be (root(p(
      Text("aa "),
      SpanLink(Seq(Text("Baz")), ExternalTarget("https://default.api/def/bar/Baz.html")),
      Text(" bb")
    )))
  }

  it should "create a span link based on the longest prefix match" in new ApiDirectiveSetup {
    parse(input("foo.bar.Baz")).content should be (root(p(
      Text("aa "),
      SpanLink(Seq(Text("Baz")), ExternalTarget("https://bar.api/foo/bar/Baz.html")),
      Text(" bb")
    )))
  }

  it should "create a span link based on the shorter prefix match" in new ApiDirectiveSetup {
    parse(input("foo.baz.Baz")).content should be (root(p(
      Text("aa "),
      SpanLink(Seq(Text("Baz")), ExternalTarget("https://foo.api/foo/baz/Baz.html")),
      Text(" bb")
    )))
  }

  it should "create a span link to a method" in new ApiDirectiveSetup {
    parse(input("foo.baz.Baz#canEqual(that:Any\\):Boolean")).content should be (root(p(
      Text("aa "),
      SpanLink(Seq(Text("Baz.canEqual")), ExternalTarget("https://foo.api/foo/baz/Baz.html#canEqual(that:Any):Boolean")),
      Text(" bb")
    )))
  }

  it should "create a span link for a package" in new ApiDirectiveSetup {
    parse(input("foo.bar.package")).content should be (root(p(
      Text("aa "),
      SpanLink(Seq(Text("foo.bar")), ExternalTarget("https://bar.api/foo/bar/index.html")),
      Text(" bb")
    )))
  }
  
  it should "fail for an internal link to a missing target" in new ApiDirectiveSetup {
    val directive = "@:api(internal.foo.Foo)"
    val input = blockInput(s"aa $directive bb")
    val path = Path.parse("/local/path/internal/foo/Foo.html")
    val msg = "One or more errors processing directive 'api': unresolved internal reference: local/path/internal/foo/Foo.html"
    parse(input).content should be (root(p(
      Text("aa "),
      InvalidSpan(msg, source(directive, input)),
      Text(" bb")
    )))
  }

  it should "fail when there is no matching base URI defined" in new ApiDirectiveSetup {
    val directive = "@:api(foo.bar.Baz)"
    val input = s"aa $directive bb"
    val msg = "One or more errors processing directive 'api': No base URI defined for 'foo.bar.Baz' and no default URI available."
    parse(input).content should be (root(p(
      Text("aa "),
      InvalidSpan(msg, source(directive, input)),
      Text(" bb")
    )))
  }

  it should "parse an api directive as the only element of a block" ignore new ApiDirectiveSetup {
    // TODO - this fails right now, might need auto-promotion of span directives without body to block directives
    val input = """aa
                  |
                  |@:api(foo.bar.Baz)
                  |
                  |bb""".stripMargin
    parse(blockInput(input)).content should be (root(
      p("aa"),
      p(SpanLink(Seq(Text("Baz")), ExternalTarget(s"https://bar.api/foo/bar/Baz.html"))),
      p("bb")
    ))
  }

  it should "parse an api directive as the first element of a block" in new ApiDirectiveSetup {
    val input = """aa
                  |
                  |@:api(foo.bar.Baz) bb
                  |
                  |cc""".stripMargin
    parse(blockInput(input)).content should be (root(
      p("aa"),
      p(SpanLink(Seq(Text("Baz")), ExternalTarget(s"https://bar.api/foo/bar/Baz.html")), Text(" bb")),
      p("cc")
    ))
  }

  it should "parse an api directive as the first line of a block" ignore new ApiDirectiveSetup {
    val input = """aa
                  |
                  |@:api(foo.bar.Baz)
                  |bb
                  |
                  |cc""".stripMargin
    parse(blockInput(input)).content should be (root(
      p("aa"),
      p(SpanLink(Seq(Text("Baz")), ExternalTarget(s"https://bar.api/foo/bar/Baz.html")), Text(" bb")),
      p("cc")
    ))
  }

  trait SourceDirectiveSetup {
    def input (typeName: String) =
      s"""{%
         |  laika.links.source = [
         |    { baseUri = "https://default.source/", suffix = scala },
         |    { baseUri = "https://foo.source/", suffix = scala, packagePrefix = foo },
         |    { baseUri = "https://bar.source/", suffix = java, packagePrefix = foo.bar }
         |  ]
         |%}
         |
        |aa @:source($typeName) bb
      """.stripMargin
  }

  "The source directive" should "create a span link based on the default base URI" in new SourceDirectiveSetup {
    parse(input("def.bar.Baz")).content should be (root(p(
      Text("aa "),
      SpanLink(Seq(Text("Baz")), ExternalTarget(s"https://default.source/def/bar/Baz.scala")),
      Text(" bb")
    )))
  }

  it should "create a span link based on the longest prefix match" in new SourceDirectiveSetup {
    parse(input("foo.bar.Baz")).content should be (root(p(
      Text("aa "),
      SpanLink(Seq(Text("Baz")), ExternalTarget(s"https://bar.source/foo/bar/Baz.java")),
      Text(" bb")
    )))
  }

  it should "create a span link based on the shorter prefix match" in new SourceDirectiveSetup {
    parse(input("foo.baz.Baz")).content should be (root(p(
      Text("aa "),
      SpanLink(Seq(Text("Baz")), ExternalTarget(s"https://foo.source/foo/baz/Baz.scala")),
      Text(" bb")
    )))
  }

  it should "fail when there is no matching base URI defined" in new SourceDirectiveSetup {
    val directive = "@:api(foo.bar.Baz)"
    val input = s"aa $directive bb"
    val msg = "One or more errors processing directive 'api': No base URI defined for 'foo.bar.Baz' and no default URI available."
    parse(input).content should be (root(p(
      Text("aa "),
      InvalidSpan(msg, source(directive, input)),
      Text(" bb")
    )))
  }


  trait TreeModel {

    import Path.Root

    def hasTitleDocs: Boolean = false

    def header (level: Int, title: Int, style: String = "section") =
      Header(level,List(Text("Section "+title)), Styles(style))

    val sectionsWithoutTitle = RootElement(
      Header(1, List(Text("Title")), Style.title) ::
        header(1,1) ::
        header(2,2) ::
        header(1,3) ::
        header(2,4) ::
        Nil
    )

    def includeTargetFormatConfig: Boolean = false

    def targetFormatsFor (name: String): Option[Seq[String]] =
      if (!includeTargetFormatConfig) None else name match {
        case "doc2" => Some(Nil)
        case "doc3" => Some(Seq("html", "txt"))
        case "doc4" => Some(Seq("epub", "pdf"))
        case _ => None
      }

    def config(path: Path, title: String, scope: Origin.Scope): Config = ConfigBuilder
      .withOrigin(Origin(scope, path))
      .withValue(LaikaKeys.title, title)
      .withValue(LaikaKeys.targetFormats, targetFormatsFor(path.name))
      .build

    def titleDoc (path: Path): Option[Document] =
      if (!hasTitleDocs || path == Root) None
      else Some(Document(path / "title", sectionsWithoutTitle, config = config(path / "title", "TitleDoc", Origin.DocumentScope)))

    def buildTree (templates: List[TemplateDocument] = Nil, docUnderTest: Option[Document] = None): DocumentTree = {
      def docs (parent: Path, nums: Int*): Seq[Document] = nums map { n =>
        val docConfig = config(parent / ("doc"+n), "Doc "+n, Origin.DocumentScope)
        (n, docUnderTest) match {
          case (6, Some(doc)) => doc.copy(config = docConfig)
          case _ => Document(parent / ("doc"+n), sectionsWithoutTitle, config = docConfig)
        }
      }
      DocumentTree(Root, docs(Root, 1,2) ++ List(
        DocumentTree(Root / "sub1", docs(Root / "sub1",3,4), titleDoc(Root / "sub1"), config = config(Root / "sub1", "Tree 1", Origin.TreeScope)),
        DocumentTree(Root / "sub2", docs(Root / "sub2",5,6), titleDoc(Root / "sub2"), config = config(Root / "sub2", "Tree 2", Origin.TreeScope))
      ), templates = templates)
    }

    def parseTemplateAndRewrite (template: String): RootElement = {
      val templateDoc = TemplateDocument(DefaultTemplatePath.forHTML, parseTemplate(template))
      val inputTree = buildTree(List(templateDoc))
      val tree = inputTree.rewrite(OperationConfig.default.rewriteRulesFor(DocumentTreeRoot(inputTree)))
      TemplateRewriter.applyTemplates(DocumentTreeRoot(tree), TemplateContext("html")).toOption.get.tree.selectDocument(CurrentTree / "sub2" / "doc6").get.content
    }

    def parseDocumentAndRewrite (markup: String): RootElement = {
      val markupDoc = parseUnresolved(markup, Root / "sub2" / "doc6")
      val inputTree = buildTree(Nil, Some(markupDoc))
      val tree = inputTree.rewrite(OperationConfig.default.rewriteRulesFor(DocumentTreeRoot(inputTree)))
      TemplateRewriter.applyTemplates(DocumentTreeRoot(tree), TemplateContext("html")).toOption.get.tree.selectDocument(CurrentTree / "sub2" / "doc6").get.content
    }

    def markup = """# Title
                   |
                   |# Headline 1
                   |
                   |# Headline 2""".stripMargin
  }

  trait NavModel {

    def hasTitleDocs: Boolean

    def maxLevels: Int = Int.MaxValue
    def excludeSections: Boolean = false

    def itemStyles: Options = NoOpt

    val refPath: Path = Root / "sub2" / "doc6"

    def styles (level: Int): Options = Style.level(level) + itemStyles

    def sectionList (path: Path, section: Int, level: Int): NavigationItem = NavigationItem(
      SpanSequence(s"Section $section"),
      if (section % 2 == 0 || level == maxLevels) Nil else Seq(
        sectionList(path, section + 1, level+1)
      ),
      Some(NavigationLink(InternalTarget(path.withFragment(s"section-$section")).relativeTo(refPath))),
      options = styles(level)
    )

    def docList (path: Path, doc: Int, level: Int, title: Option[String] = None): NavigationItem = NavigationItem(
      SpanSequence(title.getOrElse(s"Doc $doc")),
      if (level == maxLevels || excludeSections) Nil else Seq(
        sectionList(path, 1, level+1),
        sectionList(path, 3, level+1)
      ),
      Some(NavigationLink(InternalTarget(path).relativeTo(refPath), path == refPath)),
      TargetFormats.All,
      styles(level)
    )

    def treeList (tree: Int, docStartNum: Int, level: Int, excludeSelf: Boolean = false): NavigationItem = {
      val children = if (level == maxLevels) Nil else List(
        docList(Root / s"sub$tree" / s"doc$docStartNum", docStartNum, level + 1),
        docList(Root / s"sub$tree" / s"doc${docStartNum + 1}", docStartNum + 1, level + 1),
      )
      if (hasTitleDocs) NavigationItem(
        SpanSequence("TitleDoc"),
        children,
        Some(NavigationLink(InternalTarget(Root / s"sub$tree" / "title").relativeTo(refPath))),
        options = styles(level)
      )
      else NavigationItem(
        SpanSequence(s"Tree $tree"),
        if (excludeSelf) children.take(1) else children,
        options = styles(level)
      )
    }

    val rootEntry: NavigationItem = NavigationItem(SpanSequence("/"), Nil, None, TargetFormats.All, styles(1))

    def rootList: NavigationItem =
      NavigationItem(SpanSequence("/"), List(
        docList(Root / "doc1", 1, 2),
        docList(Root / "doc2", 2, 2),
        treeList(1, 3, 2),
        treeList(2, 5, 2)
      ), options = styles(1))

    def templateResult (items: NavigationItem*): RootElement = buildResult(NavigationList(items, itemStyles))

    def error (msg: String, fragment: String, input: String): RootElement = {
      buildResult(InvalidSpan(msg, source(fragment, input)))
    }

    private val sections = Seq(
      Title(List(Text("Title")), Id("title") + Style.title),
      Section(Header(1, List(Text("Section 1")), Id("section-1") + Style.section), Seq(
        Section(Header(2, List(Text("Section 2")), Id("section-2") + Style.section), Nil)
      )),
      Section(Header(1, List(Text("Section 3")), Id("section-3") + Style.section), Seq(
        Section(Header(2, List(Text("Section 4")), Id("section-4") + Style.section), Nil)
      ))
    )

    private def buildResult (element: Element): RootElement = {
      root(TemplateRoot(
        t("aaa "),
        TemplateElement(element),
        t(" bbb "),
        EmbeddedRoot(sections)
      ))
    }

    def blockResult (items: NavigationItem*): RootElement = root(
      p("aaa"),
      NavigationList(items, itemStyles),
      p("bbb")
    )

    def extLink (num: Int): NavigationItem = NavigationItem(
      SpanSequence(s"Link $num"), 
      Nil,
      Some(NavigationLink(ExternalTarget(s"http://domain-$num.com/")))
    )
  }

  "The template nav directive" should "produce two manual entries" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { title = Link 1, target = "http://domain-1.com/"}
        |    { title = Link 2, target = "http://domain-2.com/"} 
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(extLink(1), extLink(2)))
  }

  it should "produce a manual entry and a generated entry" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { title = Link 1, target = "http://domain-1.com/"}
        |    { target = "#" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(extLink(1), docList(Root / "sub2" / "doc6", 6, 1)))
  }

  it should "produce an entry generated from the root of the tree" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "/" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(rootList))
  }

  it should "produce an entry generated from the root of the tree with documents filtered by target format" in new TreeModel with NavModel {

    override def includeTargetFormatConfig = true

    val filteredRootList: NavigationItem =
      NavigationItem(SpanSequence("/"), List(
        docList(Root / "doc1", 1, 2),
        NavigationItem(SpanSequence("Tree 1"),
          List(
            docList(Root / s"sub1" / s"doc3", 3, 3).copy(targetFormats = TargetFormats.Selected(NonEmptySet.of("html", "txt")))
          ), options = styles(2)),
        treeList(2, 5, 2)
      ), options = styles(1))
    
    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "/" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(filteredRootList))
  }

  it should "produce an entry generated from the root of the tree with title documents" in new TreeModel with NavModel {

    override val hasTitleDocs: Boolean = true

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "/" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(rootList))
  }

  it should "produce an entry generated from the current tree" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "." }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(treeList(2, 5, 1)))
  }

  it should "produce an entry generated from the current tree with a maximum depth" in new TreeModel with NavModel {

    override def maxLevels: Int = 2

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = ".", depth = 2 }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(treeList(2, 5, 1)))
  }

  it should "produce an entry generated from the current tree with the root excluded" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = ".", excludeRoot = true }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(treeList(2, 5, 0).content: _*))
  }

  it should "produce an entry generated from the current tree with the self link excluded" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  excludeSelf = true
        |  entries = [
        |    { target = "." }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(treeList(2, 5, 1, excludeSelf = true)))
  }

  it should "produce an entry generated from the current tree with sections excluded" in new TreeModel with NavModel {

    override def excludeSections: Boolean = true

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = ".", excludeSections = true }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(treeList(2, 5, 1)))
  }

  it should "produce an entry generated from the current document" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "#" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(docList(Root / "sub2" / "doc6", 6, 1)))
  }

  it should "produce an entry generated from the current document with a custom title" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "#", title = Custom }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(docList(Root / "sub2" / "doc6", 6, 1, title = Some("Custom"))))
  }

  it should "produce an entry generated from a document referred to with an absolute path" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "/sub1/doc3" }
        |  ] 
        |} bbb ${cursor.currentDocument.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(docList(Root / "sub1" / "doc3", 3, 1)))
  }

  it should "fail when referring to a path that does not exist" in new TreeModel with NavModel {

    val directive = """@:navigationTree {
                      |  entries = [
                      |   { target = "/sub2/doc99" }
                      |  ]
                      |}""".stripMargin

    val template =
      s"""aaa $directive bbb $${cursor.currentDocument.content}""".stripMargin

    val msg = "One or more errors processing directive 'navigationTree': One or more errors generating navigation: Unable to resolve document or tree with path: /sub2/doc99"
    parseTemplateAndRewrite(template) should be (error(msg, directive, template))
  }

  it should "fail with an invalid depth attribute" in new TreeModel with NavModel {

    val directive = """@:navigationTree {
                      |  entries = [
                      |   { target = "/", depth = foo }
                      |  ]
                      |}""".stripMargin

    val template =
      s"""aaa $directive bbb $${cursor.currentDocument.content}""".stripMargin

    val msg = "One or more errors processing directive 'navigationTree': One or more errors decoding array elements: not an integer: foo"
    parseTemplateAndRewrite(template) should be (error(msg, directive, template))
  }

  it should "fail with a manual node without title" in new TreeModel with NavModel {

    val directive = """@:navigationTree {
                      |  entries = [
                      |   { target = "http://foo.bar" }
                      |  ]
                      |}""".stripMargin

    val template =
      s"""aaa $directive bbb $${cursor.currentDocument.content}""".stripMargin

    val msg = "One or more errors processing directive 'navigationTree': One or more errors decoding array elements: Not found: 'title'"
    parseTemplateAndRewrite(template) should be (error(msg, directive, template))
  }

  "The block nav directive" should "produce two manual entries" in new TreeModel with NavModel {

    val input =
      """aaa
        |
        |@:navigationTree { 
        |  entries = [
        |    { title = Link 1, target = "http://domain-1.com/"}
        |    { title = Link 2, target = "http://domain-2.com/"} 
        |  ] 
        |}
        |
        |bbb""".stripMargin

    parseDocumentAndRewrite(input) should be (blockResult(extLink(1), extLink(2)))
  }

  "The template breadcrumb directive" should "produce three entries" in new TreeModel with NavModel {

    override val maxLevels: Int = 1
    override def itemStyles: Options = Style.breadcrumb

    val input = "aaa @:breadcrumb bbb ${cursor.currentDocument.content}"

    parseTemplateAndRewrite(input) should be (templateResult(
      rootEntry,
      treeList(2, 0, 1),
      docList(Root / "sub2" / "doc6", 6, 1)
    ))
  }

  "The block breadcrumb directive" should "produce three entries" in new TreeModel with NavModel {

    override val maxLevels: Int = 1
    override def itemStyles: Options = Style.breadcrumb

    val input =
      """aaa
        |
        |@:breadcrumb
        |
        |bbb""".stripMargin

    parseDocumentAndRewrite(input) should be (blockResult(
      rootEntry,
      treeList(2, 0, 1),
      docList(Root / "sub2" / "doc6", 6, 1)
    ))
  }

  it should "produce three entries with title documents" in new TreeModel with NavModel {

    override val maxLevels: Int = 1
    override def itemStyles: Options = Style.breadcrumb
    override def hasTitleDocs: Boolean = true

    val input =
      """aaa
        |
        |@:breadcrumb
        |
        |bbb""".stripMargin

    parseDocumentAndRewrite(input) should be (blockResult(
      rootEntry,
      treeList(2, 0, 1),
      docList(Root / "sub2" / "doc6", 6, 1)
    ))
  }

  it should "produce an entry generated from a document referred to with a relative path" in new TreeModel with NavModel {

    val input =
      """aaa
        |
        |@:navigationTree { 
        |  entries = [
        |    { target = "../sub1/doc3" }
        |  ] 
        |}
        |
        |bbb""".stripMargin

    parseDocumentAndRewrite(input) should be (blockResult(docList(Root / "sub1" / "doc3", 3, 1)))
  }

  trait LinkModel extends TreeModel {

    def staticDocs = Seq(
      StaticDocument(Root / "doc-1.css", TargetFormats.Selected("html")),
      StaticDocument(Root / "doc-2.epub.css", TargetFormats.Selected("epub", "epub.xhtml")),
      StaticDocument(Root / "doc-3.shared.css", TargetFormats.Selected("epub", "epub.xhtml", "html")),
      StaticDocument(Root / "sub2" / "doc-4.css", TargetFormats.Selected("html")),
    )

    def templatePath: Path

    override def parseTemplateAndRewrite (template: String): RootElement = {
      val templateDoc = TemplateDocument(templatePath, parseTemplate(template))
      val inputTree = buildTree(List(templateDoc))
      val tree = inputTree.rewrite(OperationConfig.default.rewriteRulesFor(DocumentTreeRoot(inputTree)))
      val root = DocumentTreeRoot(tree, staticDocuments = staticDocs)
      val templateSuffix = templatePath.suffix.get.stripPrefix("template.")
      val finalFormat = if (templateSuffix == "html") "html" else "epub"
      val res = TemplateRewriter.applyTemplates(root, TemplateContext(templateSuffix, finalFormat)).toOption.get
      res.tree.selectDocument(CurrentTree / "sub2" / "doc6").get.content
    }

    def buildResult (content: String): RootElement = {
      root(TemplateRoot(
        t("aaa\n\n"),
        TemplateElement(RawContent(NonEmptySet.of("html","xhtml","epub"), content)),
        t("\n\nbbb")
      ))
    }
  }

  "The linkCSS directive" should "pick all CSS documents apart from those for EPUB when used without attributes" in new LinkModel {
    def templatePath = DefaultTemplatePath.forHTML
    val input =
      """aaa
        |
        |@:linkCSS
        |
        |bbb""".stripMargin

    val res = parseTemplateAndRewrite(input)
    val generatedHTML = """<link rel="stylesheet" type="text/css" href="../doc-1.css" />
                          |    <link rel="stylesheet" type="text/css" href="../doc-3.shared.css" />
                          |    <link rel="stylesheet" type="text/css" href="doc-4.css" />""".stripMargin
    parseTemplateAndRewrite(input) should be (buildResult(generatedHTML))
  }

  it should "pick matching CSS documents apart from those for EPUB when used with an paths filter" in new LinkModel {
    def templatePath = DefaultTemplatePath.forHTML
    val input =
      """aaa
        |
        |@:linkCSS { paths = [ /sub2 ] }
        |
        |bbb""".stripMargin

    val res = parseTemplateAndRewrite(input)
    val generatedHTML = """<link rel="stylesheet" type="text/css" href="doc-4.css" />""".stripMargin
    parseTemplateAndRewrite(input) should be (buildResult(generatedHTML))
  }

  it should "pick matching CSS documents apart from those for EPUB and respect the order of the specified filters" in new LinkModel {
    def templatePath = DefaultTemplatePath.forHTML
    val input =
      """aaa
        |
        |@:linkCSS { paths = [ /sub2, / ] }
        |
        |bbb""".stripMargin

    val res = parseTemplateAndRewrite(input)
    val generatedHTML = """<link rel="stylesheet" type="text/css" href="doc-4.css" />
                          |    <link rel="stylesheet" type="text/css" href="../doc-1.css" />
                          |    <link rel="stylesheet" type="text/css" href="../doc-3.shared.css" />""".stripMargin
    parseTemplateAndRewrite(input) should be (buildResult(generatedHTML))
  }

  it should "pick all CSS documents for EPUB when used without attributes" in new LinkModel {
    def templatePath = DefaultTemplatePath.forEPUB
    val input =
      """aaa
        |
        |@:linkCSS
        |
        |bbb""".stripMargin

    val res = parseTemplateAndRewrite(input)
    val generatedHTML = """<link rel="stylesheet" type="text/css" href="../doc-2.epub.css" />
                          |    <link rel="stylesheet" type="text/css" href="../doc-3.shared.css" />""".stripMargin
    parseTemplateAndRewrite(input) should be (buildResult(generatedHTML))
  }

  "The linkJS directive" should "pick all JavaScript documents when used without attributes" in new LinkModel {
    override def staticDocs = Seq(
      StaticDocument(Root / "doc-1.js", TargetFormats.Selected("html")),
      StaticDocument(Root / "doc-2.foo"),
      StaticDocument(Root / "doc-3.bar"),
      StaticDocument(Root / "sub2" / "doc-4.js", TargetFormats.Selected("html")),
    )
    def templatePath = DefaultTemplatePath.forHTML
    val input =
      """aaa
        |
        |@:linkJS
        |
        |bbb""".stripMargin

    val res = parseTemplateAndRewrite(input)
    val generatedHTML = """<script src="../doc-1.js"></script>
                          |    <script src="doc-4.js"></script>""".stripMargin
    parseTemplateAndRewrite(input) should be (buildResult(generatedHTML))
  }

  trait ImageSetup {

    val imgTarget = InternalTarget(CurrentTree / "picture.jpg")
    val resolvedImageTarget = InternalTarget(CurrentTree / "picture.jpg").relativeTo(Root / "doc")

    val defaultBlockStyle = Styles("default-image-block")
    val defaultSpanStyle = Styles("default-image-span")

    def blocks(directive: String): String =
      s"""aaa
         |
      |$directive
         |
      |bbb""".stripMargin
  }

  "The image block directive" should "succeed without any HOCON attributes" in new ImageSetup {
    val result = root(p("aaa"), BlockSequence(Seq(SpanSequence(Image(resolvedImageTarget))), defaultBlockStyle), p("bbb"))
    parse(blocks("@:image(picture.jpg)")).content should be (result)
  }

  it should "support the alt and title attributes" in new ImageSetup {
    val input = """@:image(picture.jpg) {
                  |  alt = alt
                  |  title = desc
                  |}""".stripMargin
    val result = root(p("aaa"), BlockSequence(Seq(SpanSequence(Image(resolvedImageTarget, alt = Some("alt"), title = Some("desc")))), defaultBlockStyle), p("bbb"))
    parse(blocks(input)).content should be (result)
  }

  it should "support the intrinsicWidth and intrinsicHeight attributes" in new ImageSetup {
    val input = """@:image(picture.jpg) {
                  |  intrinsicWidth = 320
                  |  intrinsicHeight = 240
                  |}""".stripMargin
    val result = root(p("aaa"), BlockSequence(Seq(SpanSequence(Image(resolvedImageTarget, width = Some(LengthUnit.px(320)), height = Some(LengthUnit.px(240))))), defaultBlockStyle), p("bbb"))
    parse(blocks(input)).content should be (result)
  }

  it should "support the style attribute" in new ImageSetup {
    val input = """@:image(picture.jpg) {
                  |  style = small-image
                  |}""".stripMargin
    val result = root(p("aaa"), BlockSequence(Seq(SpanSequence(Image(resolvedImageTarget))), Styles("small-image")), p("bbb"))
    parse(blocks(input)).content should be (result)
  }

  "The image span directive" should "succeed without any HOCON attributes" in new ImageSetup {
    val result = root(p(Text("aaa "), Image(resolvedImageTarget, options = defaultSpanStyle), Text(" bbb")))
    parse("aaa @:image(picture.jpg) bbb").content should be (result)
  }

  it should "support the alt and title attributes" in new ImageSetup {
    val input = """aaa @:image(picture.jpg) {
                  |  alt = alt
                  |  title = desc
                  |} bbb""".stripMargin
    val result = root(p(Text("aaa "), Image(resolvedImageTarget, alt = Some("alt"), title = Some("desc"), options = defaultSpanStyle), Text(" bbb")))
    parse(input).content should be (result)
  }

  it should "support the intrinsicWidth and intrinsicHeight attributes" in new ImageSetup {
    val input = """aaa @:image(picture.jpg) {
                  |  intrinsicWidth = 320
                  |  intrinsicHeight = 240
                  |} bbb""".stripMargin
    val result = root(p(Text("aaa "), Image(resolvedImageTarget, width = Some(LengthUnit.px(320)), height = Some(LengthUnit.px(240)), options = defaultSpanStyle), Text(" bbb")))
    parse(input).content should be (result)
  }

  it should "support the style attribute" in new ImageSetup {
    val input = """aaa @:image(picture.jpg) {
                  |  style = small-image
                  |} bbb""".stripMargin
    val result = root(p(Text("aaa "), Image(resolvedImageTarget, options = Styles("small-image")), Text(" bbb")))
    parse(input).content should be (result)
  }

}
