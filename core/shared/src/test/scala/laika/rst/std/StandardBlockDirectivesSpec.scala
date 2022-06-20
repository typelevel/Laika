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

package laika.rst.std

import cats.data.NonEmptySet
import laika.api.{MarkupParser, RenderPhaseRewrite}
import laika.api.builder.OperationConfig
import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentTree
import laika.ast._
import laika.ast.sample.{ParagraphCompanionShortcuts, SampleTrees}
import laika.config.{ConfigValue, Field, LaikaKeys, ObjectValue, StringValue}
import laika.format.{AST, HTML, ReStructuredText}
import laika.parse.GeneratedSource
import laika.rewrite.ReferenceResolver.CursorKeys
import laika.rewrite.{DefaultTemplatePath, OutputContext, TemplateRewriter}
import laika.rewrite.link.LinkConfig
import laika.rst.ast.{Contents, Include, RstStyle}
import munit.FunSuite

/**
 * @author Jens Halm
 */
class StandardBlockDirectivesSpec extends FunSuite with ParagraphCompanionShortcuts with RenderPhaseRewrite {

  val simplePars: List[Paragraph] = List(p("1st Para"), p("2nd Para"))

  private val parser = MarkupParser
    .of(ReStructuredText)
    .build
  
  private val docParser = MarkupParser
    .of(ReStructuredText)
    .withConfigValue(LinkConfig(excludeFromValidation = Seq(Root)))
    .build
  
  def run (input: String, expected: Block*)(implicit loc: munit.Location): Unit = {
    val res = docParser
      .parse(input)
      .flatMap(rewrite(docParser, AST))
      .map(_.content)
    assertEquals(res, Right(RootElement(expected)))
  }

  def runRaw (input: String, expected: Block*): Unit = {
    val res = parser
      .parseUnresolved(input)
      .map(_
        .document
        .content
        .rewriteBlocks({ case _: Hidden with Block => Remove }) // removing the noise of rst TextRoles which are not the focus of this spec
      )
    
    assertEquals(res, Right(RootElement(expected)))
  }

  def runFragments (input: String, expectedFragments: Map[String, Element], expectedContent: RootElement): Unit = {
    val res = docParser.parse(input).map { doc =>
      (doc.fragments, doc.content)
    }
    assertEquals(res, Right((expectedFragments, expectedContent)))
  }
  
  test("compound - sequence of two paragraphs") {
    val input = """.. compound::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, BlockSequence(simplePars, RstStyle.compound))
  }
  
  test("compound - set an id for the sequence") {
    val input = """.. compound::
      | :name: foo
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, BlockSequence(simplePars, RstStyle.compound + Id("foo")))
  }
  
  test("compound - set a style for the sequence") {
    val input = """.. compound::
      | :class: foo
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, BlockSequence(simplePars, Styles("foo", "compound")))
  }
  
  
  test("container - sequence of two paragraphs") {
    val input = """.. container::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, BlockSequence(simplePars))
  }
  
  test("container - sequence of two paragraphs with two custom styles") {
    val input = """.. container:: foo bar
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, BlockSequence(simplePars, Styles("foo", "bar")))
  }
  
  
  test("admonition - generic") {
    val input = """.. admonition:: TITLE
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, TitledBlock(List(Text("TITLE")), simplePars, RstStyle.admonition))
  }
  
  test("admonition - set id and style") {
    val input = """.. admonition:: TITLE
      | :name: foo
      | :class: bar
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, TitledBlock(List(Text("TITLE")), simplePars, Id("foo") + Styles("bar","admonition")))
  }
  
  test("attention admonition") {
    val input = """.. attention::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, TitledBlock(List(Text("Attention!")), simplePars, Styles("attention")))
  }
  
  test("attention admonition with id and style") {
    val input = """.. attention::
      | :name: foo
      | :class: bar
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, TitledBlock(List(Text("Attention!")), simplePars, Id("foo") + Styles("bar","attention")))
  }
  
  test("attention admonition with nested blocks and inline markup") {
    val input = """.. attention::
      |
      | 1st *Para*
      |
      |  2nd Para""".stripMargin
    run(input, TitledBlock(List(Text("Attention!")), List(p(Text("1st "),Emphasized("Para")), QuotedBlock("2nd Para")), Styles("attention")))
  }
  
  test("caution admonition") {
    val input = """.. caution::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, TitledBlock(List(Text("Caution!")), simplePars, Styles("caution")))
  }
  
  test("danger admonition") {
    val input = """.. danger::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, TitledBlock(List(Text("!DANGER!")), simplePars, Styles("danger")))
  }
  
  test("the error admonition") {
    val input = """.. error::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, TitledBlock(List(Text("Error")), simplePars, Styles("error")))
  }
  
  test("hint admonition") {
    val input = """.. hint::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, TitledBlock(List(Text("Hint")), simplePars, Styles("hint")))
  }
  
  test("important admonition") {
    val input = """.. important::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, TitledBlock(List(Text("Important")), simplePars, Styles("important")))
  }
  
  test("note admonition") {
    val input = """.. note::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, TitledBlock(List(Text("Note")), simplePars, Styles("note")))
  }
  
  test("tip admonition") {
    val input = """.. tip::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, TitledBlock(List(Text("Tip")), simplePars, Styles("tip")))
  }
  
  test("warning admonition") {
    val input = """.. warning::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, TitledBlock(List(Text("Warning")), simplePars, Styles("warning")))
  }
  
  test("warning admonition - match the name of the directive case-insensitively") {
    val input = """.. Warning::
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, TitledBlock(List(Text("Warning")), simplePars, Styles("warning")))
  }
  
  
  test("topic - sequence of two paragraphs") {
    val input = """.. topic:: TITLE
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, TitledBlock(List(Text("TITLE")), simplePars, RstStyle.topic))
  }
  
  test("topic - set id and style") {
    val input = """.. topic:: TITLE
      | :name: foo
      | :class: bar
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, TitledBlock(List(Text("TITLE")), simplePars, Id("foo") + Styles("bar","topic")))
  }
  
  
  test("sidebar - sequence of two paragraphs") {
    val input = """.. sidebar:: TITLE
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, TitledBlock(List(Text("TITLE")), simplePars, RstStyle.sidebar))
  }
  
  test("sidebar - set id and style") {
    val input = """.. sidebar:: TITLE
      | :name: foo
      | :class: bar
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, TitledBlock(List(Text("TITLE")), simplePars, Id("foo") + Styles("bar","sidebar")))
  }
  
  test("sidebar - set id and subtitle") {
    val input = """.. sidebar:: TITLE
      | :name: foo
      | :subtitle: some *text*
      |
      | 1st Para
      |
      | 2nd Para""".stripMargin
    run(input, TitledBlock(List(Text("TITLE")), Paragraph(List(Text("some "),Emphasized("text")), RstStyle.subtitle) :: simplePars, 
        Id("foo") + RstStyle.sidebar))
  }
  
  
  test("rubric - parse spans without other options") {
    val input = """.. rubric:: some *text*"""
    run(input, Paragraph(List(Text("some "),Emphasized("text")), RstStyle.rubric))
  }
  
  test("rubric - set id and styles") {
    val input = """.. rubric:: some *text*
      | :name: foo
      | :class: bar""".stripMargin
    run(input, Paragraph(List(Text("some "),Emphasized("text")), Id("foo") + Styles("bar","rubric")))
  }
  
  
  test("epigraph - single line") {
    val input = """.. epigraph:: some *text*"""
    run(input, QuotedBlock(List(p(Text("some "),Emphasized("text"))), Nil, Styles("epigraph")))
  }
  
  test("epigraph - multiple lines") {
    val input = """.. epigraph:: 
      |
      | some *text*
      | some more""".stripMargin
    run(input, QuotedBlock(List(p(Text("some "),Emphasized("text"),Text("\nsome more"))), Nil, Styles("epigraph")))
  }
  
  test("epigraph - with attributions") {
    val input = """.. epigraph:: 
      |
      | some *text*
      | some more
      | 
      | -- attr""".stripMargin
    run(input, QuotedBlock(List(p(Text("some "),Emphasized("text"),Text("\nsome more"))), List(Text("attr", Style.attribution)), Styles("epigraph")))
  }
  
  test("highlights - single line") {
    val input = """.. highlights:: some *text*"""
    run(input, QuotedBlock(List(p(Text("some "),Emphasized("text"))), Nil, Styles("highlights")))
  }
  
  test("pull-quote - single line") {
    val input = """.. pull-quote:: some *text*"""
    run(input, QuotedBlock(List(p(Text("some "),Emphasized("text"))), Nil, Styles("pull-quote")))
  }
  
  
  test("parsed-literal - multiple lines") {
    val input = """.. parsed-literal::
      | 
      | some *text*
      | some more""".stripMargin
    run(input, ParsedLiteralBlock(List(Text("some "),Emphasized("text"),Text("\nsome more"))))
  }
  
  test("parsed-literal - set id and style") {
    val input = """.. parsed-literal::
      | :name: foo
      | :class: bar
      |
      | some *text*
      | some more""".stripMargin
    run(input, ParsedLiteralBlock(List(Text("some "),Emphasized("text"),Text("\nsome more")), Id("foo") + Styles("bar")))
  }
  
  test("parsed-literal - multiple lines separated by spaces, preserving indentation") {
    val input = """.. parsed-literal::
      | 
      | some *text*
      | some more
      |
      |  indented""".stripMargin
    run(input, ParsedLiteralBlock(List(Text("some "),Emphasized("text"),Text("\nsome more\n\n indented"))))
  }
  
  
  test("code - multiple lines") {
    val input = """.. code:: banana-script
      | 
      | some banana
      | some more""".stripMargin
    run(input, CodeBlock("banana-script", List(Text("some banana\nsome more"))))
  }
  
  test("code - set id and style") {
    val input = """.. code:: banana-script
      | :name: foo
      | :class: bar
      |
      | some banana
      | some more""".stripMargin
    run(input, CodeBlock("banana-script", List(Text("some banana\nsome more")), Nil, Id("foo") + Styles("bar")))
  }
  
  test("code - multiple lines separated by spaces, preserving indentation") {
    val input = """.. code:: banana-script
      | 
      | some banana
      | some more
      |
      |  indented""".stripMargin
    run(input, CodeBlock("banana-script", List(Text("some banana\nsome more\n\n indented"))))
  }
  
  
  test("grid table with caption") {
    val input = """.. table:: *caption*
      | 
      | +---+---+
      | | a | b |
      | +---+---+
      | | c | d |
      | +---+---+""".stripMargin
    run(input, Table(Row(BodyCell("a"),BodyCell("b")), Row(BodyCell("c"),BodyCell("d"))).copy(caption = Caption(List(Emphasized("caption")))))
  }
  
  test("simple table with caption") {
    val input = """.. table:: *caption*
      | 
      | ===  ===
      |  a    b
      |  c    d
      | ===  ===""".stripMargin
    run(input, Table(Row(BodyCell("a"),BodyCell("b")), Row(BodyCell("c"),BodyCell("d"))).copy(caption = Caption(List(Emphasized("caption")))))
  }
  
  test("simple table without caption") {
    val input = """.. table::
      | 
      | ===  ===
      |  a    b
      |  c    d
      | ===  ===""".stripMargin
    run(input, Table(Row(BodyCell("a"),BodyCell("b")), Row(BodyCell("c"),BodyCell("d"))))
  }
  
  
  private val resolvedImageTarget = InternalTarget(CurrentTree / "picture.jpg")
  private val imgLink = SpanLink.external("http://foo.com/")(Image(resolvedImageTarget))
  
  test("image without options") {
    val input = """.. image:: picture.jpg"""
    run(input, p(Image(resolvedImageTarget)))
  }
  
  test("image with alt option") {
    val input = """.. image:: picture.jpg
      | :alt: alt""".stripMargin
    run(input, p(Image(resolvedImageTarget, alt = Some("alt"))))
  }
  
  test("image with target option with a simple reference") {
    val input = """.. image:: picture.jpg
      | :target: ref_
      |
      |.. _ref: http://foo.com/""".stripMargin
    run(input, p(imgLink))
  }
  
  test("image with target option with a phrase reference") {
    val input = """.. image:: picture.jpg
      | :target: `some ref`_
      |
      |.. _`some ref`: http://foo.com/""".stripMargin
    run(input, p(imgLink))
  }
  
  test("image with target option with a uri") {
    val input = """.. image:: picture.jpg
      | :target: http://foo.com/""".stripMargin
    run(input, p(imgLink))
  }
  
  test("image with class option") {
    val input = """.. image:: picture.jpg
      | :class: foo""".stripMargin
    run(input, p(Image(resolvedImageTarget, options=Styles("foo"))))
  }
  
  
  test("figure without options") {
    val input = """.. figure:: picture.jpg"""
    run(input, Figure(Image(resolvedImageTarget),Nil,Nil))
  }
  
  test("figure with a caption") {
    val input = """.. figure:: picture.jpg
      |
      | This is the *caption*""".stripMargin
    run(input, Figure(Image(resolvedImageTarget), List(Text("This is the "),Emphasized("caption")), Nil))
  }
  
  test("figure with a caption and a legend") {
    val input = """.. figure:: picture.jpg
      |
      | This is the *caption*
      |
      | And this is the legend""".stripMargin
    run(input, Figure(Image(resolvedImageTarget), List(Text("This is the "),Emphasized("caption")), List(p("And this is the legend"))))
  }
  
  test("support the class option for the figure and the image") {
    val input = """.. figure:: picture.jpg
      | :class: img
      | :figclass: fig""".stripMargin
    run(input, Figure(Image(resolvedImageTarget, options=Styles("img")), Nil, Nil, Styles("fig")))
  }
  
  test("support the target option with a simple reference and a caption") {
    val input = """.. figure:: picture.jpg
      | :target: ref_
      |
      | This is the *caption*
      |
      |.. _ref: http://foo.com/""".stripMargin
    run(input, Figure(imgLink, List(Text("This is the "),Emphasized("caption")), Nil))
  }
  
  
  test("header - creates a fragment in the document") {
    val input = """.. header:: 
      | This is
      | a header
      |
      |This isn't""".stripMargin
    val fragments = Map("header" -> BlockSequence("This is\na header"))
    val rootElem = RootElement(p("This isn't"))
    runFragments(input, fragments, rootElem)
  }
  
  test("footer - creates a fragment in the document") {
    val input = """.. footer:: 
      | This is
      | a footer
      |
      |This isn't""".stripMargin
    val fragments = Map("footer" -> BlockSequence("This is\na footer"))
    val rootElem = RootElement(p("This isn't"))
    runFragments(input, fragments, rootElem)
  }
  
  test("include - creates a placeholder in the document") {
    val input = """.. include:: other.rst"""
    runRaw(input, Include("other.rst", GeneratedSource))
  }
  
  test("include rewriter replaces the node with the corresponding document") {
    val ref = TemplateContextReference(CursorKeys.documentContent, required = true, GeneratedSource)
    val tree = SampleTrees.twoDocuments
      .doc1.content(Include("doc-2", GeneratedSource))
      .doc2.content(p("text"))
      .root.template(DefaultTemplatePath.forHTML.name, ref)
      .build
      .tree

    val result = for {
      rewrittenTree    <- tree.rewrite(OperationConfig.default.withBundlesFor(ReStructuredText).rewriteRulesFor(DocumentTreeRoot(tree), RewritePhase.Resolve))
      root              = DocumentTreeRoot(rewrittenTree)
      rules             = OperationConfig.default.rewriteRulesFor(root, RewritePhase.Render(HTML))
      templatesApplied <- TemplateRewriter.applyTemplates(root, rules, OutputContext(HTML))
    } yield templatesApplied.tree.content.collect { case doc: Document => doc }.head.content

    assertEquals(result, Right(RootElement(BlockSequence("text"))))
  }
  
  test("title - sets the title in the document instance") {
    val input = """.. title:: Title"""
    assertEquals(docParser.parse(input).map(_.title), Right(Some(SpanSequence("Title"))))
  }
  
  test("meta - creates config entries in the document instance") {
    val input = """.. meta::
      | :key1: val1
      | :key2: val2""".stripMargin
    val expected = ObjectValue(Seq(Field("key1", StringValue("val1")), Field("key2", StringValue("val2"))))
    val res = docParser.parse(input).flatMap(_.config.get[ConfigValue]("meta"))
    assertEquals(res, Right(expected))
  }
  
  test("sectnum - creates config entries in the document instance") {
    val input = """.. sectnum::
      | :depth: 3
      | :start: 1
      | :prefix: (
      | :suffix: )""".stripMargin
    val expected = ObjectValue(Seq(
      Field("depth", StringValue("3")), 
      Field("start", StringValue("1")), 
      Field("prefix", StringValue("(")), 
      Field("suffix", StringValue(")"))
    ))
    val res = docParser.parse(input).flatMap(_.config.get[ConfigValue](LaikaKeys.autonumbering))
    assertEquals(res, Right(expected))
  }
  
  test("contents - creates a placeholder in the document") {
    val input = """.. contents:: This is the title
      | :depth: 3
      | :local: true""".stripMargin
    val elem = Contents("This is the title", GeneratedSource, depth = 3, local = true)
    runRaw(input, elem)
  }
  
  test("contents rewriter replaces the node with the corresponding list element") {
    def header (level: Int, title: Int, style: String = "section") =
      Header(level,List(Text("Title "+title)),Id("title-"+title) + Styles(style))
      
    def title (title: Int) =
      Title(List(Text("Title "+title)),Id("title-"+title) + Style.title)
      
    def link (level: Int, titleNum: Int, children: Seq[NavigationItem] = Nil): NavigationItem = {
      val target = Some(NavigationLink(InternalTarget(Root / s"doc#title-$titleNum").relativeTo(Root / "doc")))
      val title = SpanSequence("Title "+titleNum)
      NavigationItem(title, children, target, options = Style.level(level))
    }

    val sectionsWithTitle = RootElement(
      header(1,1,"title") ::
      Contents("This is the title", GeneratedSource, 3) ::
      header(2,2) ::
      header(3,3) ::
      header(2,4) ::
      header(3,5) ::
      Nil
    )
    
    val navList = NavigationList(Seq(
      link(1, 2, Seq(link(2, 3))),
      link(1, 4, Seq(link(2, 5)))
    ))
    
    val expected = RootElement(
      title(1),
      TitledBlock(List(Text("This is the title")), List(navList), Style.nav),
      Section(header(2,2), List(Section(header(3,3), Nil))),
      Section(header(2,4), List(Section(header(3,5), Nil)))
    )
    
    val document = Document(Root / "doc", sectionsWithTitle)
    val template = TemplateDocument(DefaultTemplatePath.forHTML, 
      TemplateRoot(TemplateContextReference(CursorKeys.documentContent, required = true, GeneratedSource)))
    val tree = DocumentTree(Root, content = List(document), templates = List(template))
    val result = for {
      rewrittenTree    <- tree.rewrite(OperationConfig.default.withBundlesFor(ReStructuredText).rewriteRulesFor(DocumentTreeRoot(tree), RewritePhase.Resolve))
      root              = DocumentTreeRoot(rewrittenTree)
      rules             = OperationConfig.default.rewriteRulesFor(root, RewritePhase.Render(HTML))
      templatesApplied <- TemplateRewriter.applyTemplates(root, rules, OutputContext(HTML))
    } yield templatesApplied.tree.content.collect { case doc: Document => doc }.head.content
    assertEquals(result, Right(expected))
  }
  
  
  test("raw - support raw input with one format") {
    val input = """.. raw:: format
      |
      | some input
      |
      | some more""".stripMargin
    val res = MarkupParser.of(ReStructuredText).withRawContent.build.parse(input).map(_.content)
    assertEquals(res, Right(RootElement(RawContent(NonEmptySet.one("format"), "some input\n\nsome more"))))
  }
  
  
}
