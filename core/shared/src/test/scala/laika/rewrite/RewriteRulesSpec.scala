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

package laika.rewrite

import laika.api.builder.OperationConfig
import laika.ast.Path.Root
import laika.ast.RelativePath.{ CurrentDocument, Parent }
import laika.ast._
import laika.ast.sample.SampleConfig.{ globalLinkValidation, siteBaseURL, targetFormats }
import laika.ast.sample.{
  ParagraphCompanionShortcuts,
  SampleSixDocuments,
  SampleTrees,
  TestSourceBuilders
}
import laika.config.Config.ConfigResult
import laika.config.{ Config, ConfigBuilder, LaikaKeys }
import laika.parse.GeneratedSource
import laika.rewrite.link.{ LinkConfig, TargetDefinition }
import laika.rewrite.nav.TargetFormats
import laika.rst.ast.Underline
import munit.FunSuite

class RewriteRulesSpec extends FunSuite with ParagraphCompanionShortcuts with TestSourceBuilders {

  def rewritten(root: RootElement, withTitles: Boolean = true): ConfigResult[RootElement] = {
    val config =
      if (withTitles)
        ConfigBuilder.empty.withValue(LaikaKeys.firstHeaderAsTitle, true).build
      else Config.empty
    val doc    = Document(Path.Root / "doc", root, config = config)
    OperationConfig.default
      .rewriteRulesFor(doc, RewritePhase.Resolve)
      .flatMap(doc.rewrite(_).map(_.content))
  }

  val refPath: Path = Root / "doc"

  def invalidSpan(message: String, fallback: String): InvalidSpan =
    InvalidSpan(message, source(fallback, fallback))

  def invalidBlock(message: String, fallback: Block): InvalidBlock =
    InvalidBlock(RuntimeMessage(MessageLevel.Error, message), GeneratedSource, fallback)

  def invalidSpan(message: String, fallback: Span): InvalidSpan =
    InvalidSpan(RuntimeMessage(MessageLevel.Error, message), GeneratedSource, fallback)

  def fnRefs(labels: FootnoteLabel*): Paragraph = Paragraph(labels.map { label =>
    FootnoteReference(label, generatedSource(toSource(label)))
  })

  def fnLinks(labels: (String, String)*): Paragraph = p(labels.map { label =>
    FootnoteLink(label._1, label._2)
  }: _*)

  def fn(label: FootnoteLabel, num: Any) =
    FootnoteDefinition(label, List(p(s"footnote$num")), GeneratedSource)

  def fn(id: String, label: String) = Footnote(label, List(p(s"footnote$label")), Id(id))

  def fn(label: String) = Footnote(label, List(p(s"footnote$label")))

  def linkIdRef(id: String = "name") =
    LinkIdReference(List(Text("text")), id, generatedSource(s"<<$id>>"))

  def pathRef(id: String = "name") =
    LinkPathReference(List(Text("text")), RelativePath.parse(s"#$id"), generatedSource(s"[<$id>]"))

  def extLink(url: String) = SpanLink.external(url)("text")

  def intLink(ref: String) = SpanLink(rootLinkTarget(ref))("text")

  def intLink(path: RelativePath) = SpanLink(InternalTarget(path).relativeTo(refPath))("text")

  def docLink(ref: String) =
    SpanLink(InternalTarget(CurrentDocument(ref)).relativeTo(refPath))("text")

  def rootLinkTarget(fragment: String): InternalTarget = InternalTarget(
    RelativePath.parse(s"#$fragment")
  )

  def simpleImgRef(id: String = "name") = ImageIdReference("text", id, generatedSource(s"!<$id>"))

  def runRoot(input: RootElement, expected: RootElement): Unit =
    assertEquals(rewritten(input), Right(expected))

  def runRootWithoutTitles(input: RootElement, expected: RootElement): Unit =
    assertEquals(rewritten(input, withTitles = false), Right(expected))

  test("citation rules - retain a single reference when it has a matching target") {
    val rootElem = RootElement(
      p(CitationReference("label", generatedSource("[label]_"))),
      Citation("label", List(p("citation")))
    )
    val resolved = RootElement(
      p(CitationLink("__cit-label", "label")),
      Citation("label", List(p("citation")), Id("__cit-label"))
    )
    runRoot(rootElem, resolved)
  }

  test("citation rules - retain multiple references when they all have a matching targets") {
    val rootElem = RootElement(
      p(
        CitationReference("label1", generatedSource("[label1]_")),
        CitationReference("label2", generatedSource("[label2]_")),
        CitationReference("label1", generatedSource("[label1]_"))
      ),
      Citation("label1", List(p("citation1"))),
      Citation("label2", List(p("citation2")))
    )
    val resolved = RootElement(
      p(
        CitationLink("__cit-label1", "label1"),
        CitationLink("__cit-label2", "label2"),
        CitationLink("__cit-label1", "label1")
      ),
      Citation("label1", List(p("citation1")), Id("__cit-label1")),
      Citation("label2", List(p("citation2")), Id("__cit-label2"))
    )
    runRoot(rootElem, resolved)
  }

  test("citation rules - replace a reference with an unknown label with an invalid span") {
    val sourceRef = generatedSource("[label1]_")
    val rootElem  = RootElement(
      p(CitationReference("label1", sourceRef)),
      Citation("label2", List(p("citation2")))
    )
    val resolved  = RootElement(
      p(invalidSpan("unresolved citation reference: label1", "[label1]_")),
      Citation("label2", List(p("citation2")), Id("__cit-label2"))
    )
    runRoot(rootElem, resolved)
  }

  test(
    "footnote rules - retain a group of footnotes with a mix of explicit numeric and autonumber labels"
  ) {
    val rootElem = RootElement(
      fnRefs(Autonumber, NumericLabel(1), Autonumber),
      fn(Autonumber, 1),
      fn(NumericLabel(1), 1),
      fn(Autonumber, 2)
    )
    val resolved = RootElement(
      fnLinks(("__fn-1", "1"), ("__fnl-1", "1"), ("__fn-2", "2")),
      fn("__fn-1", "1"),
      fn("__fnl-1", "1"),
      fn("__fn-2", "2")
    )
    runRoot(rootElem, resolved)
  }

  test(
    "footnote rules - retain a group of footnotes with a mix of explicit numeric, autonumber and autonumber-labeled footnotes"
  ) {
    val rootElem = RootElement(
      fnRefs(NumericLabel(2), Autonumber, AutonumberLabel("label")),
      fn(NumericLabel(2), 2),
      fn(AutonumberLabel("label"), 1),
      fn(Autonumber, 2)
    )
    val resolved = RootElement(
      fnLinks(("__fnl-2", "2"), ("__fn-2", "2"), ("label", "1")),
      fn("__fnl-2", "2"),
      fn("label", "1"),
      fn("__fn-2", "2")
    )
    runRoot(rootElem, resolved)
  }

  test("footnote rules - retain a group of footnotes with autosymbol labels") {
    val rootElem = RootElement(
      fnRefs(Autosymbol, Autosymbol, Autosymbol),
      fn(Autosymbol, "*"),
      fn(Autosymbol, "\u2020"),
      fn(Autosymbol, "\u2021")
    )
    val resolved = RootElement(
      fnLinks(("__fns-1", "*"), ("__fns-2", "\u2020"), ("__fns-3", "\u2021")),
      fn("__fns-1", "*"),
      fn("__fns-2", "\u2020"),
      fn("__fns-3", "\u2021")
    )
    runRoot(rootElem, resolved)
  }

  test(
    "footnote rules - replace references with unresolvable autonumber or numeric labels with invalid spans"
  ) {
    val rootElem = RootElement(
      fnRefs(NumericLabel(2), AutonumberLabel("labelA")),
      fn(NumericLabel(3), 3),
      fn(AutonumberLabel("labelB"), 1)
    )
    val resolved = RootElement(
      p(
        invalidSpan("unresolved footnote reference: 2", "[2]_"),
        invalidSpan("unresolved footnote reference: labelA", "[#labelA]_")
      ),
      fn("__fnl-3", "3"),
      fn("labelb", "1")
    )
    runRoot(rootElem, resolved)
  }

  test("footnote rules - replace surplus autonumber references with invalid spans") {
    val rootElem = RootElement(fnRefs(Autonumber, Autonumber), fn(Autonumber, 1))
    val resolved = RootElement(
      p(FootnoteLink("__fn-1", "1"), invalidSpan("too many autonumber references", "[#]_")),
      fn("__fn-1", "1")
    )
    runRoot(rootElem, resolved)
  }

  test("footnote rules - replace surplus autosymbol references with invalid spans") {
    val rootElem = RootElement(fnRefs(Autosymbol, Autosymbol), fn(Autosymbol, "*"))
    val resolved = RootElement(
      p(FootnoteLink("__fns-1", "*"), invalidSpan("too many autosymbol references", "[*]_")),
      fn("__fns-1", "*")
    )
    runRoot(rootElem, resolved)
  }

  test("link id refs - resolve external link definitions") {
    val rootElem =
      RootElement(p(linkIdRef()), LinkDefinition("name", ExternalTarget("http://foo/")))
    val resolved = RootElement(p(extLink("http://foo/")))
    runRoot(rootElem, resolved)
  }

  test("link id refs - resolve internal link definitions") {
    val rootElem = RootElement(
      p(linkIdRef()),
      LinkDefinition("name", InternalTarget(RelativePath.parse("foo.md#ref")))
    )
    val resolved = RootElement(p(intLink(RelativePath.parse("foo.md#ref"))))
    runRoot(rootElem, resolved)
  }

  test(
    "link id refs - interpret internal link definitions as external when they point upwards beyond the virtual root"
  ) {
    val rootElem = RootElement(
      p(linkIdRef()),
      LinkDefinition("name", InternalTarget(RelativePath.parse("../../foo.md#ref")))
    )
    val resolved = RootElement(p(extLink("../../foo.md#ref")))
    runRoot(rootElem, resolved)
  }

  test("link id refs - resolve internal link targets") {
    val rootElem = RootElement(p(linkIdRef("id 5")), InternalLinkTarget(Id("id-5")))
    val resolved = RootElement(p(docLink("id-5")), InternalLinkTarget(Id("id-5")))
    runRoot(rootElem, resolved)
  }

  test("link id refs - resolve anonymous link references") {
    val rootElem = RootElement(
      p(linkIdRef(""), linkIdRef("")),
      LinkDefinition("", ExternalTarget("http://foo/")),
      LinkDefinition("", ExternalTarget("http://bar/"))
    )
    val resolved = RootElement(p(extLink("http://foo/"), extLink("http://bar/")))
    runRoot(rootElem, resolved)
  }

  test("link id refs - resolve anonymous internal link definitions") {
    val rootElem = RootElement(
      p(linkIdRef(""), linkIdRef("")),
      LinkDefinition("", InternalTarget(RelativePath.parse("foo.md#ref"))),
      LinkDefinition("", InternalTarget(RelativePath.parse("bar.md#ref")))
    )
    val resolved = RootElement(
      p(intLink(RelativePath.parse("foo.md#ref")), intLink(RelativePath.parse("bar.md#ref")))
    )
    runRoot(rootElem, resolved)
  }

  test("link id refs - resolve anonymous external link definitions") {
    val rootElem = RootElement(
      p(linkIdRef(""), linkIdRef("")),
      LinkDefinition("", ExternalTarget("http://foo/")),
      LinkDefinition("", ExternalTarget("http://bar/"))
    )
    val resolved = RootElement(p(extLink("http://foo/"), extLink("http://bar/")))
    runRoot(rootElem, resolved)
  }

  test("link id refs - replace an unresolvable reference with an invalid span") {
    val rootElem = RootElement(p(linkIdRef()))
    val resolved = RootElement(p(invalidSpan("unresolved link id reference: name", "<<name>>")))
    runRoot(rootElem, resolved)
  }

  test("link id refs - replace a surplus anonymous reference with an invalid span") {
    val rootElem = RootElement(p(linkIdRef("")))
    val resolved = RootElement(p(invalidSpan("too many anonymous references", "<<>>")))
    runRoot(rootElem, resolved)
  }

  test("link id refs - resolve references when some parent element also gets rewritten") {
    val rootElem = RootElement(
      DecoratedHeader(Underline('#'), List(Text("text "), linkIdRef()), GeneratedSource),
      LinkDefinition("name", ExternalTarget("http://foo/"))
    )
    val resolved =
      RootElement(Title(List(Text("text "), extLink("http://foo/")), Id("text-text") + Style.title))
    runRoot(rootElem, resolved)
  }

  object IdRefs {

    private val linkTarget = RootElement(InternalLinkTarget(Id("ref")))

    private val linkConfig = LinkConfig(targets =
      Seq(
        TargetDefinition("int", InternalTarget(RelativePath.parse("../doc-1.md#ref"))),
        TargetDefinition("ext", ExternalTarget("https://www.foo.com/")),
        TargetDefinition("inv", InternalTarget(RelativePath.parse("../doc-99.md#ref")))
      )
    )

    def run(ref: LinkIdReference, expected: Block): Unit = {
      val root =
        SampleTrees.sixDocuments
          .root.config(_.withValue(linkConfig))
          .root.config(globalLinkValidation)
          .docContent(linkTarget)
          .doc3.content(p(ref))
          .suffix("md")
          .build

      val res = root
        .rewrite(OperationConfig.default.rewriteRulesFor(root, RewritePhase.Resolve))
        .map(_.tree.selectDocument("tree-1/doc-3.md").map(_.content.content.head))
      assertEquals(res, Right(Some(expected)))
    }

  }

  test("global link defs - resolve internal link references to a target in the parent tree") {
    val target = InternalTarget(RelativePath.parse("../doc-1.md#ref")).relativeTo(Root / "tree1")
    IdRefs.run(linkIdRef("int"), p(SpanLink(target)("text")))
  }

  test("global link defs - resolve external link references") {
    val externalLink = SpanLink.external("https://www.foo.com/")("text")
    IdRefs.run(linkIdRef("ext"), p(externalLink))
  }

  test("global link defs - produce an invalid span for an unresolved id") {
    val expected = p(invalidSpan("unresolved link id reference: missing", "<<missing>>"))
    IdRefs.run(linkIdRef("missing"), expected)
  }

  test("global link defs - produce an invalid span for an unresolved reference") {
    val expected = p(invalidSpan("unresolved internal reference: ../doc-99.md#ref", "<<inv>>"))
    IdRefs.run(linkIdRef("inv"), expected)
  }

  object InternalLinks {

    private val pathUnderTest = Root / "tree-1" / "doc-3.md"
    private val defaultTarget = InternalLinkTarget(Id("ref"))

    def build(path: RelativePath, targetFormats: TargetFormats = TargetFormats.All): SpanLink =
      SpanLink(
        InternalTarget(path).relativeTo(pathUnderTest).copy(internalFormats = targetFormats)
      )("text")

    def rewrittenTreeDoc(
        ref: Span,
        builder: SampleSixDocuments => SampleSixDocuments = identity
    ): ConfigResult[Option[Block]] = {

      val root =
        SampleTrees.sixDocuments
          .docContent(defaultTarget)
          .doc3.content(p(ref), defaultTarget)
          .doc4.content(InternalLinkTarget(Id("target-4")))
          .suffix("md")
          .root.config(siteBaseURL("http://external/"))
          .root.config(globalLinkValidation)
          .apply(builder)
          .build

      root
        .rewrite(OperationConfig.default.rewriteRulesFor(root, RewritePhase.Resolve))
        .map(_.tree.selectDocument(pathUnderTest.relative).map(_.content.content.head))
    }

    def run(
        ref: String,
        expected: Span,
        builder: SampleSixDocuments => SampleSixDocuments = identity
    ): Unit = {
      val refInstance =
        LinkPathReference(List(Text("text")), RelativePath.parse(ref), generatedSource(s"[<$ref>]"))
      run(refInstance, expected, builder)
    }

    def run(
        ref: Reference,
        expected: Span,
        builder: SampleSixDocuments => SampleSixDocuments
    ): Unit =
      assertEquals(rewrittenTreeDoc(ref, builder), Right(Some(p(expected))))

  }

  test("internal links - resolve internal link references to a target in the same document") {
    InternalLinks.run("#ref", InternalLinks.build(CurrentDocument("ref")))
  }

  test("internal links - resolve internal link references to a target in the same tree") {
    val relPath = "doc-4.md#target-4"
    InternalLinks.run(relPath, InternalLinks.build(RelativePath.parse(relPath)))
  }

  test("internal links - resolve internal link references to a target in the parent tree") {
    InternalLinks.run(
      "../doc-1.md#ref",
      InternalLinks.build(RelativePath.parse("../doc-1.md#ref"))
    )
  }

  test("internal links - resolve internal link references to a target in a sibling tree") {
    InternalLinks.run(
      "../tree-2/doc-5.md#ref",
      InternalLinks.build(RelativePath.parse("../tree-2/doc-5.md#ref"))
    )
  }

  test("internal links - resolve internal link references to a markup document") {
    InternalLinks.run(
      "../tree-2/doc-5.md",
      InternalLinks.build(RelativePath.parse("../tree-2/doc-5.md"))
    )
  }

  test("internal links - resolve internal link references to a static document") {
    val relPath = "../images/frog.txt"
    val absPath = Root / "images" / "frog.txt"

    InternalLinks.run(
      relPath,
      InternalLinks.build(RelativePath.parse(relPath)),
      _.staticDoc(absPath)
    )
  }

  test("internal links - resolve internal link references to an image") {
    val relPath    = Parent(1) / "images" / "frog.jpg"
    val absPath    = Root / "images" / "frog.jpg"
    val imgPathRef = ImagePathReference(relPath, GeneratedSource, alt = Some("text"))
    val target     = InternalTarget(relPath).relativeTo(refPath)

    InternalLinks.run(
      imgPathRef,
      Image(target, alt = Some("text")),
      _.staticDoc(absPath)
    )
  }

  test("internal links - produce an invalid span for an unresolved reference") {
    val relPath = "../tree-2/doc99.md#ref"
    InternalLinks.run(
      relPath,
      invalidSpan(s"unresolved internal reference: $relPath", s"[<$relPath>]")
    )
  }

  test(
    "internal links - produce an invalid span for a reference to a markup document with fewer target formats than the source"
  ) {
    val relPath = "doc-4.md#target-4"
    val msg     = s"document for all output formats cannot reference document '$relPath' " +
      s"with restricted output formats unless html is one of the formats and siteBaseUrl is defined"

    InternalLinks.run(
      relPath,
      invalidSpan(msg, s"[<$relPath>]"),
      _.doc4.config(targetFormats("pdf"))
    )
  }

  test(
    "internal links - produce an invalid span for a reference to a static document with fewer target formats than the source"
  ) {
    val relPath = "../static/doc.html"
    val absPath = Root / "static" / "doc.html"
    val msg     = s"document for all output formats cannot reference document '$relPath' " +
      "with restricted output formats unless html is one of the formats and siteBaseUrl is defined"

    InternalLinks.run(
      relPath,
      invalidSpan(msg, s"[<$relPath>]"),
      _.staticDoc(absPath, "pdf")
    )
  }

  test(
    "internal links - add a restricted target format parameter for a reference to a markup document with fewer target formats than the source when siteBaseURL is defined"
  ) {
    val relPath = "doc-4.md#target-4"
    InternalLinks.run(
      relPath,
      InternalLinks.build(RelativePath.parse(relPath), TargetFormats.Selected("html")),
      _.doc4.config(targetFormats("html"))
    )
  }

  test(
    "internal links - add a restricted target format parameter for a reference to a static document with fewer target formats than the source when siteBaseURL is defined"
  ) {
    val relPath = "../static/doc.html"
    val absPath = Root / "static" / "doc.html"
    InternalLinks.run(
      relPath,
      InternalLinks.build(RelativePath.parse(relPath), TargetFormats.Selected("html")),
      _.staticDoc(absPath, "html")
    )
  }

  test("internal links - avoid validation for references beyond the virtual root") {
    val relPath = "../../doc99.md#ref"
    InternalLinks.run(relPath, extLink(relPath))
  }

  test("internal links - resolve a link id reference to a target in the same tree") {
    InternalLinks.run(
      linkIdRef("target-4"),
      InternalLinks.build(RelativePath.parse("doc-4.md#target-4")),
      identity(_)
    )
  }

  test(
    "internal links - resolve a link id reference to a header with a duplicate id by precedence"
  ) {
    def header(level: Int): Block = Header(level, Seq(Text("Header")))

    InternalLinks.run(
      linkIdRef("header"),
      InternalLinks.build(RelativePath.parse("../doc-1.md#header")),
      _
        .doc1.content(header(1))
        .doc2.content(header(2))
    )
  }

  test("link aliases - resolve indirect link references") {
    val rootElem =
      RootElement(p(pathRef()), LinkAlias("name", "ref"), InternalLinkTarget(Id("ref")))
    val expected = RootElement(p(docLink("ref")), InternalLinkTarget(Id("ref")))
    runRoot(rootElem, expected)
  }

  test("link aliases - replace an unresolvable reference to a link alias with an invalid span") {
    val rootElem = RootElement(p(pathRef()), LinkAlias("name", "ref"))
    val expected = RootElement(p(invalidSpan("unresolved link alias: ref", "[<name>]")))
    runRoot(rootElem, expected)
  }

  test("link aliases - replace circular indirect references with invalid spans") {
    val rootElem = RootElement(p(pathRef()), LinkAlias("name", "ref"), LinkAlias("ref", "name"))
    val expected = RootElement(p(invalidSpan("circular link reference: ref", "[<name>]")))
    runRoot(rootElem, expected)
  }

  test("image id refs - resolve external link references") {
    val rootElem = RootElement(
      p(simpleImgRef()),
      LinkDefinition("name", ExternalTarget("http://foo.com/bar.jpg"))
    )
    val expected =
      RootElement(p(Image(ExternalTarget("http://foo.com/bar.jpg"), alt = Some("text"))))
    runRoot(rootElem, expected)
  }

  test("image id refs - resolve internal link references") {
    val rootElem = RootElement(
      p(simpleImgRef()),
      LinkDefinition("name", InternalTarget(RelativePath.parse("foo.jpg")))
    )
    val expected = RootElement(
      p(Image(InternalTarget(Root / "foo.jpg").relativeTo(refPath), alt = Some("text")))
    )
    runRoot(rootElem, expected)
  }

  test("image id refs - replace an unresolvable reference with an invalid span") {
    val rootElem = RootElement(p(simpleImgRef()))
    val expected = RootElement(p(invalidSpan("unresolved image reference: name", "!<name>")))
    runRoot(rootElem, expected)
  }

  test("header ids - create the id for the header based on the header text") {
    val rootElem = RootElement(Header(1, List(Text("Header"))))
    val expected = RootElement(Title(List(Text("Header")), Id("header") + Style.title))
    runRoot(rootElem, expected)
  }

  test("decorated headers - set the level of the header in a flat list of headers") {
    val rootElem = RootElement(
      DecoratedHeader(Underline('#'), List(Text("Title")), GeneratedSource),
      DecoratedHeader(Underline('#'), List(Text("Header 1")), GeneratedSource),
      DecoratedHeader(Underline('#'), List(Text("Header 2")), GeneratedSource)
    )
    val expected = RootElement(
      Title(List(Text("Title")), Id("title") + Style.title),
      Section(Header(1, List(Text("Header 1")), Id("header-1") + Style.section), Nil),
      Section(Header(1, List(Text("Header 2")), Id("header-2") + Style.section), Nil)
    )
    runRoot(rootElem, expected)
  }

  test("decorated headers - set the level of the header in a nested list of headers") {
    val rootElem = RootElement(
      DecoratedHeader(Underline('#'), List(Text("Title")), GeneratedSource),
      DecoratedHeader(Underline('#'), List(Text("Header 1")), GeneratedSource),
      DecoratedHeader(Underline('-'), List(Text("Header 2")), GeneratedSource),
      DecoratedHeader(Underline('#'), List(Text("Header 3")), GeneratedSource)
    )
    val expected = RootElement(
      Title(List(Text("Title")), Id("title") + Style.title),
      Section(
        Header(1, List(Text("Header 1")), Id("header-1") + Style.section),
        List(Section(Header(2, List(Text("Header 2")), Id("header-2") + Style.section), Nil))
      ),
      Section(Header(1, List(Text("Header 3")), Id("header-3") + Style.section), Nil)
    )
    runRoot(rootElem, expected)
  }

  test(
    "decorated headers - do not create title nodes in the default configuration for orphan documents"
  ) {
    val rootElem = RootElement(
      DecoratedHeader(Underline('#'), List(Text("Title")), GeneratedSource),
      DecoratedHeader(Underline('#'), List(Text("Header 1")), GeneratedSource),
      DecoratedHeader(Underline('#'), List(Text("Header 2")), GeneratedSource)
    )
    val expected = RootElement(
      Section(Header(1, List(Text("Title")), Id("title") + Style.section), Nil),
      Section(Header(1, List(Text("Header 1")), Id("header-1") + Style.section), Nil),
      Section(Header(1, List(Text("Header 2")), Id("header-2") + Style.section), Nil)
    )
    runRootWithoutTitles(rootElem, expected)
  }

  test("duplicate ids - append auto-increment numbers") {
    val header   = Header(1, "Header")
    val rootElem = RootElement(header, header)
    val expected = RootElement(
      Title("Header").withOptions(Id("header-1") + Styles("title")),
      Section(header.withOptions(Id("header-2") + Styles("section")), Nil)
    )
    runRoot(rootElem, expected)
  }

  test("duplicate ids - remove invalid external link definitions altogether") {
    val target2a = LinkDefinition("id2", ExternalTarget("http://foo/"))
    val target2b = LinkDefinition("id2", ExternalTarget("http://bar/"))
    val rootElem = RootElement(target2a, target2b)
    val expected = RootElement.empty
    runRoot(rootElem, expected)
  }

  test("duplicate ids - replace ambiguous references to duplicate ids with invalid spans") {
    val target1a = LinkDefinition("name", ExternalTarget("http://foo/1"))
    val target1b = LinkDefinition("name", ExternalTarget("http://foo/2"))
    val msg      = "Ambiguous reference: more than one link definition with id 'name' in path /doc"
    val rootElem = RootElement(p(linkIdRef()), target1a, target1b)
    val expected = RootElement(p(invalidSpan(msg, "<<name>>")))
    runRoot(rootElem, expected)
  }

  test(
    "duplicate ids - replace ambiguous references for a link alias pointing to duplicate ids with invalid spans"
  ) {
    val target    = InternalLinkTarget(Id("ref"))
    val targetMsg = "Ambiguous reference: more than one link target with id 'ref' in path /doc"
    val rootElem  = RootElement(p(pathRef()), LinkAlias("name", "ref"), target, target)
    val expected  = RootElement(
      p(invalidSpan(targetMsg, "[<name>]")),
      InternalLinkTarget(Id("ref-1")),
      InternalLinkTarget(Id("ref-2"))
    )
    runRoot(rootElem, expected)
  }

}
