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

import laika.api.MarkupParser
import laika.api.builder.OperationConfig
import laika.ast.Path.Root
import laika.ast.RelativePath.{CurrentDocument, CurrentTree}
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.config.{Config, ConfigBuilder, ConfigParser, Key, Origin}
import laika.format.Markdown
import laika.parse.ParserContext
import laika.rewrite.TemplateRewriter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class StandardDirectiveSpec extends AnyFlatSpec
  with Matchers
  with ModelBuilder {


  lazy val templateParser = StandardDirectives.processExtension(DirectiveSupport).parsers.templateParser.get
  lazy val markupParser = MarkupParser.of(Markdown).build

  def parse (input: String, path: Path = Root / "doc"): Document = markupParser.parse(input, path).toOption.get
  def parseUnresolved (input: String, path: Path = Root / "doc"): Document = markupParser.parseUnresolved(input, path).toOption.get.document

  def parseWithFragments (input: String, path: Path = Root / "doc"): (Map[String,Element], RootElement) = {
    val doc = parse(input, path)
    (doc.fragments, doc.content)
  }

  def parseTemplate (input: String): TemplateRoot = templateParser.parse(ParserContext(input)).toOption.get

  def parseTemplateWithConfig (input: String, config: String): RootElement = {
    val tRoot = parseTemplate(input)
    val template = TemplateDocument(Path.Root, tRoot)
    val cursor = DocumentCursor(Document(Path.Root, root(), config = ConfigParser.parse(config).resolve().toOption.get))
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

  it should "parse a single nested span" in {
    val input = """aa @:style(foo) 11 @:@ bb"""
    parse(input).content should be (root(p(Text("aa "), Text(" 11 ", Styles("foo")), Text(" bb"))))
  }

  it should "parse two nested spans" in {
    val input = """aa @:style(foo) 11 *22* 33 @:@ bb"""
    parse(input).content should be (root(p(Text("aa "), SpanSequence(List(Text(" 11 "),Emphasized("22"),Text(" 33 ")),Styles("foo")), Text(" bb"))))
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
    parse(input).content should be (root(p("aa"), TargetFormat("foo", p("11\n22")), p("bb")))
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
    parse(input).content should be (root(p("aa"), TargetFormat("foo", BlockSequence(p("11\n22"),p("33"))), p("bb")))
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


  "The ref directive" should "create a generic link" in {
    val input = """aa @:ref(Some Id) bb"""
    parse(input).content should be (root(p(Text("aa "), GenericReference(Seq(Text("Some Id")), "Some Id", "@:ref(Some Id)"), Text(" bb"))))
  }


  trait ApiDirectiveSetup {
    def input (typeName: String) =
      s"""{%
         |  links.api = [
         |    { baseUri = "https://default.api/" },
         |    { baseUri = "https://foo.api/", packagePrefix = foo },
         |    { baseUri = "https://bar.api/", packagePrefix = foo.bar }
         |  ]
         |%}
         |
        |aa @:api($typeName) bb
      """.stripMargin
  }


  "The api directive" should "create a span link based on the default base URI" in new ApiDirectiveSetup {
    parse(input("def.bar.Baz")).content should be (root(p(
      Text("aa "),
      SpanLink(Seq(Text("Baz")), ExternalTarget(s"https://default.api/def/bar/Baz.html")),
      Text(" bb")
    )))
  }

  it should "create a span link based on the longest prefix match" in new ApiDirectiveSetup {
    parse(input("foo.bar.Baz")).content should be (root(p(
      Text("aa "),
      SpanLink(Seq(Text("Baz")), ExternalTarget(s"https://bar.api/foo/bar/Baz.html")),
      Text(" bb")
    )))
  }

  it should "create a span link based on the shorter prefix match" in new ApiDirectiveSetup {
    parse(input("foo.baz.Baz")).content should be (root(p(
      Text("aa "),
      SpanLink(Seq(Text("Baz")), ExternalTarget(s"https://foo.api/foo/baz/Baz.html")),
      Text(" bb")
    )))
  }

  it should "create a span link to a method" in new ApiDirectiveSetup {
    parse(input("foo.baz.Baz#canEqual(that:Any\\):Boolean")).content should be (root(p(
      Text("aa "),
      SpanLink(Seq(Text("Baz.canEqual")), ExternalTarget(s"https://foo.api/foo/baz/Baz.html#canEqual(that:Any):Boolean")),
      Text(" bb")
    )))
  }

  it should "create a span link for a package" in new ApiDirectiveSetup {
    parse(input("foo.bar.package")).content should be (root(p(
      Text("aa "),
      SpanLink(Seq(Text("foo.bar")), ExternalTarget(s"https://bar.api/foo/bar/index.html")),
      Text(" bb")
    )))
  }

  it should "fail when there is no matching base URI defined" in new ApiDirectiveSetup {
    val res = parse("aa @:api(foo.bar.Baz) bb")
    parse("aa @:api(foo.bar.Baz) bb").content should be (root(p(
      Text("aa "),
      InvalidElement("One or more errors processing directive 'api': No base URI defined for 'foo.bar.Baz' and no default URI available.", "@:api(foo.bar.Baz)").asSpan,
      Text(" bb")
    )))
  }


  trait TreeModel {

    import Path.Root

    val pathUnderTest = Root / "sub2" / "doc7" // TODO - 0.16 - remove

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

    def config(path: Path, title: String, scope: Origin.Scope): Config = ConfigBuilder
      .withOrigin(Origin(scope, path))
      .withValue("title", title)
      .build

    def titleDoc (path: Path): Option[Document] =
      if (!hasTitleDocs || path == Root) None
      else Some(Document(path / "title", sectionsWithoutTitle, config = config(path / "title", "TitleDoc", Origin.DocumentScope)))

    def buildTree (templates: List[TemplateDocument] = Nil, docUnderTest: Option[Document] = None, legacyAdditionalMarkup: List[Document] = Nil): DocumentTree = {
      def docs (parent: Path, nums: Int*): Seq[Document] = nums map { n =>
        val docConfig = config(parent / ("doc"+n), "Doc "+n, Origin.DocumentScope)
        (n, docUnderTest) match {
          case (6, Some(doc)) => doc.copy(config = docConfig)
          case _ => Document(parent / ("doc"+n), sectionsWithoutTitle, config = docConfig)
        }
      }
      DocumentTree(Root, docs(Root, 1,2) ++ List(
        DocumentTree(Root / "sub1", docs(Root / "sub1",3,4), titleDoc(Root / "sub1"), config = config(Root / "sub1", "Tree 1", Origin.TreeScope)),
        DocumentTree(Root / "sub2", docs(Root / "sub2",5,6) ++ legacyAdditionalMarkup, titleDoc(Root / "sub2"), config = config(Root / "sub2", "Tree 2", Origin.TreeScope))
      ), templates = templates)
    }

    def parseAndRewrite (template: String, legacyAdditionalMarkup: String): RootElement = { // TODO - 0.16 - remove
      val templateDoc = TemplateDocument(Root / "test.html", parseTemplate(template))
      val doc = Document(pathUnderTest, parse(legacyAdditionalMarkup, pathUnderTest).content, config =
        config(pathUnderTest, "Doc 7", Origin.DocumentScope).withValue("template","/test.html").build)
      val inputTree = buildTree(List(templateDoc), legacyAdditionalMarkup = List(doc))
      val tree = inputTree.rewrite(OperationConfig.default.rewriteRulesFor(DocumentTreeRoot(inputTree)))
      TemplateRewriter.applyTemplates(DocumentTreeRoot(tree), "html").toOption.get.tree.selectDocument(CurrentTree / "sub2" / "doc7").get.content
    }

    def parseTemplateAndRewrite (template: String): RootElement = {
      val templateDoc = TemplateDocument(Root / "default.template.html", parseTemplate(template))
      val inputTree = buildTree(List(templateDoc))
      val tree = inputTree.rewrite(OperationConfig.default.rewriteRulesFor(DocumentTreeRoot(inputTree)))
      TemplateRewriter.applyTemplates(DocumentTreeRoot(tree), "html").toOption.get.tree.selectDocument(CurrentTree / "sub2" / "doc6").get.content
    }

    def parseDocumentAndRewrite (markup: String): RootElement = {
      val markupDoc = parseUnresolved(markup, Root / "sub2" / "doc6")
      val inputTree = buildTree(Nil, Some(markupDoc))
      val tree = inputTree.rewrite(OperationConfig.default.rewriteRulesFor(DocumentTreeRoot(inputTree)))
      TemplateRewriter.applyTemplates(DocumentTreeRoot(tree), "html").toOption.get.tree.selectDocument(CurrentTree / "sub2" / "doc6").get.content
    }

    def markup = """# Title
                   |
                   |# Headline 1
                   |
                   |# Headline 2""".stripMargin
  }

  // TODO - 0.16 - remove
  trait TocModel extends TreeModel {
    import Path._
    import laika.ast.TitledBlock

    val treeUnderTest = Root / "sub2"

    def title: Option[String] = None

    def hasTitleDocLinks: Boolean = false

    def targetFor (path: Path): InternalTarget = InternalTarget.fromPath(path, pathUnderTest)

    def sectionCrossLink (path: Path, section: Int, level: Int) =
      Paragraph(Seq(SpanLink(List(Text("Section "+section)), targetFor(path.withFragment("section-"+section)))), Style.legacyToc + Style.level(level))

    def leafLink (path: Path, section: Int, level: Int) =
      BulletListItem(List(sectionCrossLink(path, section, level)), StringBullet("*"))

    def sectionNode (path: Path, section: Int, level: Int) =
      BulletListItem(List(sectionCrossLink(path, section, level), BulletList(List(leafLink(path, section+1, level+1)), StringBullet("*"))), StringBullet("*"))

    def docCrossLink (path: Path, doc: Int, level: Int) =
      Paragraph(Seq(SpanLink(List(Text("Doc "+doc)), targetFor(path))), Style.legacyToc + Style.level(level))

    def docList (path: Path, doc: Int, level: Int) =
      BulletListItem(List(docCrossLink(path, doc, level), BulletList(List(
        sectionNode(path, 1, level+1),
        sectionNode(path, 3, level+1)
      ), StringBullet("*"))), StringBullet("*"))

    def docListFirstLevel (path: Path, doc: Int, level: Int) =
      BulletListItem(List(docCrossLink(path, doc, level), BulletList(List(
        leafLink(path, 1, level+1),
        leafLink(path, 3, level+1)
      ), StringBullet("*"))), StringBullet("*"))

    def internalLink (section: Int, level: Int) =
      BulletListItem(List(
        Paragraph(Seq(SpanLink(List(Text("Headline "+section)), InternalTarget(pathUnderTest.withFragment("headline-"+section), CurrentDocument("headline-"+section)))), Style.legacyToc + Style.level(level))
      ), StringBullet("*"))

    def extraDoc (treeNum: Int, level: Int): List[BulletListItem] =
      if (treeNum == 1) Nil
      else List(BulletListItem(List(
        Paragraph(List(Text("Doc 7")), Style.legacyToc + Style.level(level) + Style.active),
        BulletList(List(
          internalLink(1, level+1),
          internalLink(2, level+1)
        ), StringBullet("*"))), StringBullet("*")))

    def treeTitle (treeNum: Int): Paragraph =
      if (!hasTitleDocLinks) Paragraph(List(Text("Tree "+treeNum)), Style.legacyToc + Style.level(1))
      else Paragraph(Seq(SpanLink(List(Text("TitleDoc")), targetFor(Root / ("sub"+treeNum) / "title"))), Style.legacyToc + Style.level(1))

    def treeList (treeNum: Int, docStart: Int): BulletListItem =
      BulletListItem(List(
        treeTitle(treeNum),
        BulletList(List(
          docList(Root / ("sub"+treeNum) / ("doc"+docStart),     docStart,   2),
          docList(Root / ("sub"+treeNum) / ("doc"+(docStart+1)), docStart+1, 2)
        ) ++ extraDoc(treeNum,2), StringBullet("*"))
      ), StringBullet("*"))

    def rootList =
      BulletList(List(
        docList(Root / "doc1", 1, 1),
        docList(Root / "doc2", 2, 1),
        treeList(1, 3),
        treeList(2, 5)
      ), StringBullet("*"))

    def currentList =
      BulletList(List(
        docList(Root / "sub2" / "doc5", 5, 1),
        docList(Root / "sub2" / "doc6", 6, 1)
      ) ++ extraDoc(2,1), StringBullet("*"))

    def firstTree =
      BulletList(List(
        docList(Root / "sub1" / "doc3", 3, 1),
        docList(Root / "sub1" / "doc4", 4, 1)
      ), StringBullet("*"))

    def firstTreeFirstLevel =
      BulletList(List(
        docListFirstLevel(Root / "sub1" / "doc3", 3, 1),
        docListFirstLevel(Root / "sub1" / "doc4", 4, 1)
      ), StringBullet("*"))

    def currentDoc =
      BulletList(List(
        internalLink(1, 1),
        internalLink(2, 1)
      ), StringBullet("*"))

    def result (list: BulletList) = {
      val toc = title match {
        case Some(text) => TitledBlock(List(Text(text)), List(list), options=Style.legacyToc)
        case None       => BlockSequence(List(list), Style.legacyToc)
      }
      root(TemplateRoot(
        t("aaa "),
        TemplateElement(toc),
        t(" bbb "),
        EmbeddedRoot(
          Title(List(Text("Title")), Id("title") + Style.title),
          Section(Header(1, List(Text("Headline 1")), Id("headline-1") + Style.section), Nil),
          Section(Header(1, List(Text("Headline 2")), Id("headline-2") + Style.section), Nil)
        )
      ))
    }

    def markupTocResult = root(
      BlockSequence(List(currentDoc), Style.legacyToc),
      Title(List(Text("Title")), Id("title") + Style.title),
      Section(Header(1, List(Text("Headline 1")), Id("headline-1") + Style.section), Nil),
      Section(Header(1, List(Text("Headline 2")), Id("headline-2") + Style.section), Nil)
    )
  }

  trait NavModel {

    def hasTitleDocs: Boolean

    def maxLevels: Int = Int.MaxValue
    def excludeSections: Boolean = false

    def itemStyles: Options = NoOpt

    val refPath: Path = Root / "sub2" / "doc6"

    def styles (level: Int): Options = Style.level(level) + itemStyles

    def sectionList (path: Path, section: Int, level: Int): NavigationLink = NavigationLink(
      SpanSequence(s"Section $section"),
      InternalTarget.fromPath(path.withFragment(s"section-$section"), refPath),
      if (section % 2 == 0 || level == maxLevels) Nil else Seq(
        sectionList(path, section + 1, level+1)
      ),
      options = styles(level)
    )

    def docList (path: Path, doc: Int, level: Int, title: Option[String] = None): NavigationLink = NavigationLink(
      SpanSequence(title.getOrElse(s"Doc $doc")),
      InternalTarget.fromPath(path, refPath),
      if (level == maxLevels || excludeSections) Nil else Seq(
        sectionList(path, 1, level+1),
        sectionList(path, 3, level+1)
      ),
      path == refPath,
      styles(level)
    )

    def treeList (tree: Int, docStartNum: Int, level: Int): NavigationItem = {
      val children = if (level == maxLevels) Nil else List(
        docList(Root / s"sub$tree" / s"doc$docStartNum", docStartNum, level + 1),
        docList(Root / s"sub$tree" / s"doc${docStartNum + 1}", docStartNum + 1, level + 1),
      )
      if (hasTitleDocs) NavigationLink(SpanSequence("TitleDoc"), InternalTarget.fromPath(Root / s"sub$tree" / "title", refPath), children, options = styles(level))
      else NavigationHeader(SpanSequence(s"Tree $tree"), children, options = styles(level))
    }

    val rootEntry: NavigationHeader = NavigationHeader(SpanSequence("/"), Nil, styles(1))

    def rootList: NavigationHeader =
      NavigationHeader(SpanSequence("/"), List(
        docList(Root / "doc1", 1, 2),
        docList(Root / "doc2", 2, 2),
        treeList(1, 3, 2),
        treeList(2, 5, 2)
      ), options = styles(1))

    def templateResult (items: NavigationItem*): RootElement = buildResult(NavigationList(items, itemStyles))

    def error (msg: String, src: String): RootElement = {
      buildResult(InvalidElement(msg, src).asSpan)
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

    def extLink (num: Int): NavigationLink = NavigationLink(SpanSequence(s"Link $num"), ExternalTarget(s"http://domain-$num.com/"), Nil)
  }

  "The template nav directive" should "produce two manual entries" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { title = Link 1, target = "http://domain-1.com/"}
        |    { title = Link 2, target = "http://domain-2.com/"} 
        |  ] 
        |} bbb ${document.content}""".stripMargin

    val res = parseTemplateAndRewrite(template)

    parseTemplateAndRewrite(template) should be (templateResult(extLink(1), extLink(2)))
  }

  it should "produce a manual entry and a generated entry" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { title = Link 1, target = "http://domain-1.com/"}
        |    { target = "#" }
        |  ] 
        |} bbb ${document.content}""".stripMargin

    val res = parseTemplateAndRewrite(template)

    parseTemplateAndRewrite(template) should be (templateResult(extLink(1), docList(Root / "sub2" / "doc6", 6, 1)))
  }

  it should "produce an entry generated from the root of the tree" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "/" }
        |  ] 
        |} bbb ${document.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(rootList))
  }

  it should "produce an entry generated from the root of the tree with title documents" in new TreeModel with NavModel {

    override val hasTitleDocs: Boolean = true

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "/" }
        |  ] 
        |} bbb ${document.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(rootList))
  }

  it should "produce an entry generated from the current tree" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "." }
        |  ] 
        |} bbb ${document.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(treeList(2, 5, 1)))
  }

  it should "produce an entry generated from the current tree with a maximum depth" in new TreeModel with NavModel {

    override def maxLevels: Int = 2

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = ".", depth = 2 }
        |  ] 
        |} bbb ${document.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(treeList(2, 5, 1)))
  }

  it should "produce an entry generated from the current tree with the root excluded" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = ".", excludeRoot = true }
        |  ] 
        |} bbb ${document.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(treeList(2, 5, 0).content: _*))
  }

  it should "produce an entry generated from the current tree with sections excluded" in new TreeModel with NavModel {

    override def excludeSections: Boolean = true

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = ".", excludeSections = true }
        |  ] 
        |} bbb ${document.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(treeList(2, 5, 1)))
  }

  it should "produce an entry generated from the current document" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "#" }
        |  ] 
        |} bbb ${document.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(docList(Root / "sub2" / "doc6", 6, 1)))
  }

  it should "produce an entry generated from the current document with a custom title" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "#", title = Custom }
        |  ] 
        |} bbb ${document.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(docList(Root / "sub2" / "doc6", 6, 1, title = Some("Custom"))))
  }

  it should "produce an entry generated from a document referred to with an absolute path" in new TreeModel with NavModel {

    val template =
      """aaa @:navigationTree { 
        |  entries = [
        |    { target = "/sub1/doc3" }
        |  ] 
        |} bbb ${document.content}""".stripMargin

    parseTemplateAndRewrite(template) should be (templateResult(docList(Root / "sub1" / "doc3", 3, 1)))
  }

  it should "fail when referring to a path that does not exist" in new TreeModel with NavModel {

    val directive = """@:navigationTree {
                      |  entries = [
                      |   { target = "/sub2/doc99" }
                      |  ]
                      |}""".stripMargin

    val template =
      s"""aaa $directive bbb $${document.content}""".stripMargin

    val msg = "One or more errors processing directive 'navigationTree': One or more errors generating navigation: Unable to resolve document or tree with path: /sub2/doc99"
    parseTemplateAndRewrite(template) should be (error(msg, directive))
  }

  it should "fail with an invalid depth attribute" in new TreeModel with NavModel {

    val directive = """@:navigationTree {
                      |  entries = [
                      |   { target = "/", depth = foo }
                      |  ]
                      |}""".stripMargin

    val template =
      s"""aaa $directive bbb $${document.content}""".stripMargin

    val msg = "One or more errors processing directive 'navigationTree': One or more errors decoding array elements: not an integer: foo"
    parseTemplateAndRewrite(template) should be (error(msg, directive))
  }

  it should "fail with a manual node without title" in new TreeModel with NavModel {

    val directive = """@:navigationTree {
                      |  entries = [
                      |   { target = "http://foo.bar" }
                      |  ]
                      |}""".stripMargin

    val template =
      s"""aaa $directive bbb $${document.content}""".stripMargin

    val msg = "One or more errors processing directive 'navigationTree': One or more errors decoding array elements: Not found: 'title'"
    parseTemplateAndRewrite(template) should be (error(msg, directive))
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

    val input = "aaa @:breadcrumb bbb ${document.content}"

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


  "The template toc directive" should "produce a table of content starting from the root tree" in {
    new TreeModel with TocModel {

      val template = """aaa @:toc bbb ${document.content}"""

      parseAndRewrite(template, markup) should be (result(rootList))
    }
  }

  it should "produce a table of content starting from the root tree containing title documents" in {
    new TreeModel with TocModel {

      override val hasTitleDocs = true
      override val hasTitleDocLinks = true

      val template = """aaa @:toc bbb ${document.content}"""

      parseAndRewrite(template, markup) should be (result(rootList))
    }
  }

  it should "produce a table of content starting from the root tree with a title" in {
    new TreeModel with TocModel {

      override val title = Some("Some Title")

      val template = """aaa @:toc { title="Some Title" } bbb ${document.content}"""

      parseAndRewrite(template, markup) should be (result(rootList))
    }
  }

  it should "produce a table of content starting from the current tree" in {
    new TreeModel with TocModel {

      val template = """aaa @:toc { root=<currentTree> } bbb ${document.content}"""

      parseAndRewrite(template, markup) should be (result(currentList))
    }
  }

  it should "produce a table of content starting from the root of the current document" in {
    new TreeModel with TocModel {

      val template = """aaa @:toc { root=<currentDocument> } bbb ${document.content}"""

      parseAndRewrite(template, markup) should be (result(currentDoc))
    }
  }

  it should "produce a table of content starting from an explicit absolute path" in {
    new TreeModel with TocModel {

      val template = """aaa @:toc { root=/sub1 } bbb ${document.content}"""

      parseAndRewrite(template, markup) should be (result(firstTree))
    }
  }

  it should "produce a table of content starting from an explicit relative path" in {
    new TreeModel with TocModel {

      val template = """aaa @:toc { root="../sub1" } bbb ${document.content}"""

      parseAndRewrite(template, markup) should be (result(firstTree))
    }
  }

  it should "produce a table of content starting from an explicit relative path with depth 2" in {
    new TreeModel with TocModel {

      val template = """aaa @:toc { root="../sub1", depth=2 } bbb ${document.content}"""

      parseAndRewrite(template, markup) should be (result(firstTreeFirstLevel))
    }
  }

  "The document toc directive" should "produce a table of content starting from the root of the current document" in {
    new TreeModel with TocModel {

      override val markup = """@:toc
                              |
                              |# Title
                              |
                              |# Headline 1
                              |
                              |# Headline 2""".stripMargin

      val template = """${document.content}"""

      parseAndRewrite(template, markup) should be (markupTocResult)
    }
  }


}
