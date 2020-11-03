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
import laika.ast.RelativePath.{CurrentDocument, Parent}
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.ast.sample.SampleConfig.{noLinkValidation, siteBaseURL, targetFormats}
import laika.ast.sample.{SampleSixDocuments, SampleTrees}
import laika.config.LaikaKeys
import laika.parse.GeneratedSource
import laika.rewrite.link.{LinkConfig, TargetDefinition}
import laika.rewrite.nav.TargetFormats
import laika.rst.ast.Underline
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec


class RewriteRulesSpec extends AnyWordSpec
  with Matchers
  with ModelBuilder {

  def rewritten (root: RootElement, withTitles: Boolean = true): RootElement = {
    val config = if (withTitles) disableInternalLinkValidation.withValue(LaikaKeys.firstHeaderAsTitle, true).build
                 else disableInternalLinkValidation
    val doc = Document(Path.Root / "doc", root, config = config)
    val rules = OperationConfig.default.rewriteRulesFor(doc)
    doc.rewrite(rules).content
  }
  
  val refPath: Path = Root / "doc"

  def invalidSpan (message: String, fallback: String): InvalidSpan = InvalidSpan(message, source(fallback, fallback))

  def invalidBlock (message: String, fallback: Block): InvalidBlock =
    InvalidBlock(RuntimeMessage(MessageLevel.Error, message), GeneratedSource, fallback)

  def invalidSpan (message: String, fallback: Span): InvalidSpan =
    InvalidSpan(RuntimeMessage(MessageLevel.Error, message), GeneratedSource, fallback)

  def fnRefs (labels: FootnoteLabel*): Paragraph = p(labels.map { label => fnRef(label, toSource(label)) }:_*)

  def fnLinks (labels: (String,String)*): Paragraph = p(labels.map { label => FootnoteLink(label._1,label._2)}:_*)

  def fn (label: FootnoteLabel, num: Any) = FootnoteDefinition(label, List(p(s"footnote$num")), GeneratedSource)

  def fn (id: String, label: String) = Footnote(label, List(p(s"footnote$label")), Id(id))

  def fn (label: String) = Footnote(label, List(p(s"footnote$label")))

  def linkIdRef (id: String = "name") = LinkIdReference(List(Text("text")), id, generatedSource(s"<<$id>>"))

  def pathRef (id: String = "name") = LinkPathReference(List(Text("text")), RelativePath.parse(s"#$id"), generatedSource(s"[<$id>]"))

  def extLink (url: String) = SpanLink(List(Text("text")), ExternalTarget(url))

  def intLink (ref: String) = SpanLink(List(Text("text")), rootLinkTarget(ref))

  def intLink (path: RelativePath) = SpanLink(List(Text("text")), InternalTarget(path).relativeTo(refPath))

  def docLink (ref: String) = SpanLink(List(Text("text")), InternalTarget(CurrentDocument(ref)).relativeTo(refPath))

  def rootLinkTarget (fragment: String): InternalTarget = InternalTarget(RelativePath.parse(s"#$fragment"))

  def simpleImgRef (id: String = "name") = ImageIdReference("text", id, generatedSource(s"!<$id>"))


  "The rewrite rules for citations" should {

    "retain a single reference when it has a matching target" in {
      val rootElem = root(p(CitationReference("label", generatedSource("[label]_"))), Citation("label", List(p("citation"))))
      val resolved = root(p(CitationLink("__cit-label", "label")), Citation("label", List(p("citation")), Id("__cit-label")))
      rewritten(rootElem) should be(resolved)
    }

    "retain multiple references when they all have a matching targets" in {
      val rootElem = root(
        p(
          CitationReference("label1", generatedSource("[label1]_")), 
          CitationReference("label2", generatedSource("[label2]_")), 
          CitationReference("label1", generatedSource("[label1]_"))
        ),
        Citation("label1", List(p("citation1"))), 
        Citation("label2", List(p("citation2")))
      )
      val resolved = root(p(CitationLink("__cit-label1", "label1"), CitationLink("__cit-label2", "label2"), CitationLink("__cit-label1", "label1")),
        Citation("label1", List(p("citation1")), Id("__cit-label1")), Citation("label2", List(p("citation2")), Id("__cit-label2")))
      rewritten(rootElem) should be(resolved)
    }

    "replace a reference with an unknown label with an invalid span" in {
      val sourceRef = generatedSource("[label1]_")
      val rootElem = root(p(CitationReference("label1", sourceRef)), Citation("label2", List(p("citation2"))))
      rewritten(rootElem) should be(root(p(invalidSpan("unresolved citation reference: label1", "[label1]_")),
        Citation("label2", List(p("citation2")), Id("__cit-label2"))))
    }
  }


  "The rewrite rules for footnotes" should {

    "retain a group of footnotes with a mix of explicit numeric and autonumber labels" in {
      val rootElem = root(fnRefs(Autonumber, NumericLabel(1), Autonumber),
        fn(Autonumber, 1), fn(NumericLabel(1), 1), fn(Autonumber, 2))
      val resolved = root(fnLinks(("__fn-1", "1"), ("__fnl-1", "1"), ("__fn-2", "2")),
        fn("__fn-1", "1"), fn("__fnl-1", "1"), fn("__fn-2", "2"))
      rewritten(rootElem) should be(resolved)
    }

    "retain a group of footnotes with a mix of explicit numeric, autonumber and autonumber-labeled footnotes" in {
      val rootElem = root(fnRefs(NumericLabel(2), Autonumber, AutonumberLabel("label")),
        fn(NumericLabel(2), 2), fn(AutonumberLabel("label"), 1), fn(Autonumber, 2))
      val resolved = root(fnLinks(("__fnl-2", "2"), ("__fn-2", "2"), ("label", "1")),
        fn("__fnl-2", "2"), fn("label", "1"), fn("__fn-2", "2"))
      rewritten(rootElem) should be(resolved)
    }

    "retain a group of footnotes with autosymbol labels" in {
      val rootElem = root(fnRefs(Autosymbol, Autosymbol, Autosymbol),
        fn(Autosymbol, "*"), fn(Autosymbol, "\u2020"), fn(Autosymbol, "\u2021"))
      val resolved = root(fnLinks(("__fns-1", "*"), ("__fns-2", "\u2020"), ("__fns-3", "\u2021")),
        fn("__fns-1", "*"), fn("__fns-2", "\u2020"), fn("__fns-3", "\u2021"))
      rewritten(rootElem) should be(resolved)
    }

    "replace references with unresolvable autonumber or numeric labels with invalid spans" in {
      val rootElem = root(fnRefs(NumericLabel(2), AutonumberLabel("labelA")),
        fn(NumericLabel(3), 3), fn(AutonumberLabel("labelB"), 1))
      val resolved = root(p(invalidSpan("unresolved footnote reference: 2", "[2]_"),
        invalidSpan("unresolved footnote reference: labelA", "[#labelA]_")),
        fn("__fnl-3", "3"), fn("labelb", "1"))
      rewritten(rootElem) should be(resolved)
    }

    "replace surplus autonumber references with invalid spans" in {
      val rootElem = root(fnRefs(Autonumber, Autonumber), fn(Autonumber, 1))
      val resolved = root(p(FootnoteLink("__fn-1", "1"), invalidSpan("too many autonumber references", "[#]_")),
        fn("__fn-1", "1"))
      rewritten(rootElem) should be(resolved)
    }

    "replace surplus autosymbol references with invalid spans" in {
      val rootElem = root(fnRefs(Autosymbol, Autosymbol), fn(Autosymbol, "*"))
      val resolved = root(p(FootnoteLink("__fns-1", "*"), invalidSpan("too many autosymbol references", "[*]_")),
        fn("__fns-1", "*"))
      rewritten(rootElem) should be(resolved)
    }
  }


  "The rewrite rules for link id references" should {

    "resolve external link definitions" in {
      val rootElem = root(p(linkIdRef()), LinkDefinition("name", ExternalTarget("http://foo/")))
      rewritten(rootElem) should be(root(p(extLink("http://foo/"))))
    }

    "resolve internal link definitions" in {
      val rootElem = root(p(linkIdRef()), LinkDefinition("name", InternalTarget(RelativePath.parse("foo.md#ref"))))
      rewritten(rootElem) should be(root(p(intLink(RelativePath.parse("foo.md#ref")))))
    }

    "interpret internal link definitions as external when they point upwards beyond the virtual root" in {
      val rootElem = root(p(linkIdRef()), LinkDefinition("name", InternalTarget(RelativePath.parse("../../foo.md#ref"))))
      rewritten(rootElem) should be(root(p(extLink("../../foo.md#ref"))))
    }

    "resolve internal link targets" in {
      val rootElem = root(p(linkIdRef("id 5")), InternalLinkTarget(Id("id-5")))
      rewritten(rootElem) should be(root(p(docLink("id-5")), InternalLinkTarget(Id("id-5"))))
    }

    "resolve anonymous link references" in {
      val rootElem = root(p(linkIdRef(""), linkIdRef("")), LinkDefinition("", ExternalTarget("http://foo/")), LinkDefinition("", ExternalTarget("http://bar/")))
      rewritten(rootElem) should be(root(p(extLink("http://foo/"), extLink("http://bar/"))))
    }

    "resolve anonymous internal link definitions" in {
      val rootElem = root(
        p(linkIdRef(""), linkIdRef("")),
        LinkDefinition("", InternalTarget(RelativePath.parse("foo.md#ref"))),
        LinkDefinition("", InternalTarget(RelativePath.parse("bar.md#ref")))
      )
      rewritten(rootElem) should be(root(p(intLink(RelativePath.parse("foo.md#ref")), intLink(RelativePath.parse("bar.md#ref")))))
    }

    "resolve anonymous external link definitions" in {
      val rootElem = root(
        p(linkIdRef(""), linkIdRef("")),
        LinkDefinition("", ExternalTarget("http://foo/")),
        LinkDefinition("", ExternalTarget("http://bar/"))
      )
      rewritten(rootElem) should be(root(p(extLink("http://foo/"), extLink("http://bar/"))))
    }

    "replace an unresolvable reference with an invalid span" in {
      val rootElem = root(p(linkIdRef()))
      rewritten(rootElem) should be(root(p(invalidSpan("unresolved link id reference: name", "<<name>>"))))
    }

    "replace a surplus anonymous reference with an invalid span" in {
      val rootElem = root(p(linkIdRef("")))
      rewritten(rootElem) should be(root(p(invalidSpan("too many anonymous references", "<<>>"))))
    }

    "resolve references when some parent element also gets rewritten" in {
      val rootElem = root(
        DecoratedHeader(Underline('#'), List(Text("text "), linkIdRef()), GeneratedSource), 
        LinkDefinition("name", ExternalTarget("http://foo/"))
      )
      rewritten(rootElem) should be(root(Title(List(Text("text "), extLink("http://foo/")), Id("text-text") + Style.title)))
    }
  }

  "The rewrite rules for global link definitions" should {

    val linkTarget = root(InternalLinkTarget(Id("ref")))

    val linkConfig = LinkConfig(targets = Seq(
      TargetDefinition("int", InternalTarget(RelativePath.parse("../doc-1.md#ref"))),
      TargetDefinition("ext", ExternalTarget("https://www.foo.com/")),
      TargetDefinition("inv", InternalTarget(RelativePath.parse("../doc-99.md#ref")))
    ))

    def rewrittenTreeDoc (ref: LinkIdReference): Block = {
      val root =
        SampleTrees.sixDocuments
          .config(_.withValue(linkConfig))
          .docContent(linkTarget)
          .doc3.content(p(ref))
          .suffix("md")
          .build
      val rewrittenTree = root.rewrite(OperationConfig.default.rewriteRulesFor(root))
      rewrittenTree.tree.selectDocument("tree-1/doc-3.md").get.content.content.head
    }

    "resolve internal link references to a target in the parent tree" in {
      val internalLink = SpanLink(List(Text("text")), InternalTarget(RelativePath.parse("../doc-1.md#ref")).relativeTo(Root / "tree1"))
      rewrittenTreeDoc(linkIdRef("int")) should be(p(internalLink))
    }

    "resolve external link references" in {
      val externalLink = SpanLink(List(Text("text")), ExternalTarget("https://www.foo.com/"))
      rewrittenTreeDoc(linkIdRef("ext")) should be(p(externalLink))
    }

    "produce an invalid span for an unresolved id" in {
      val expected = p(invalidSpan("unresolved link id reference: missing", "<<missing>>"))
      rewrittenTreeDoc(linkIdRef("missing")) should be(expected)
    }

    "produce an invalid span for an unresolved reference" in {
      val expected = p(invalidSpan("unresolved internal reference: ../doc-99.md#ref", "<<inv>>"))
      rewrittenTreeDoc(linkIdRef("inv")) should be(expected)
    }

  }

  "The rewrite rules for internal links" should {
    
    val pathUnderTest = Root / "tree-1" / "doc-3.md"
    val defaultTarget = InternalLinkTarget(Id("ref"))

    def rewrittenTreeDoc (ref: Span, builder: SampleSixDocuments => SampleSixDocuments = identity): Block = {
      
      val root = 
        SampleTrees.sixDocuments
          .docContent(defaultTarget)
          .doc3.content(p(ref), defaultTarget)
          .doc4.content(InternalLinkTarget(Id("target-4")))
          .suffix("md")
          .config(siteBaseURL("http://external/"))
          .apply(builder)
          .build

      val rewrittenTree = root.rewrite(OperationConfig.default.rewriteRulesFor(root)) // TODO - integrate these two lines into builder API together with template rewriting
      rewrittenTree.tree.selectDocument(pathUnderTest.relative).get.content.content.head
    }

    def pathRef (ref: String) = LinkPathReference(List(Text("text")), RelativePath.parse(ref), generatedSource(s"[<$ref>]"))
    
    def internalLink (path: RelativePath, targetFormats: TargetFormats = TargetFormats.All) =
      SpanLink(List(Text("text")), InternalTarget(path).relativeTo(pathUnderTest).copy(internalFormats = targetFormats))

    "resolve internal link references to a target in the same document" in {
      rewrittenTreeDoc(pathRef("#ref")) should be(p(
        internalLink(CurrentDocument("ref"))
      ))
    }

    "resolve internal link references to a target in the same tree" in {
      val relPath = "doc-4.md#target-4"
      rewrittenTreeDoc(pathRef(relPath)) should be(p(
        internalLink(RelativePath.parse(relPath))
      ))
    }

    "resolve internal link references to a target in the parent tree" in {
      rewrittenTreeDoc(pathRef("../doc-1.md#ref")) should be(p(
        internalLink(RelativePath.parse("../doc-1.md#ref"))
      ))
    }

    "resolve internal link references to a target in a sibling tree" in {
      rewrittenTreeDoc(pathRef("../tree-2/doc-5.md#ref")) should be(p(
        internalLink(RelativePath.parse("../tree-2/doc-5.md#ref"))
      ))
    }

    "resolve internal link references to a markup document" in {
      rewrittenTreeDoc(pathRef("../tree-2/doc-5.md")) should be(p(
        internalLink(RelativePath.parse("../tree-2/doc-5.md"))
      ))
    }

    "resolve internal link references to a static document" in {
      val relPath = "../images/frog.txt"
      val absPath = Root / "images" / "frog.txt"
      
      rewrittenTreeDoc(pathRef(relPath), _.staticDoc(absPath)) should be(p(
        internalLink(RelativePath.parse(relPath))
      ))
    }

    "resolve internal link references to an image" in {
      val relPath = Parent(1) / "images" / "frog.jpg"
      val absPath = Root / "images" / "frog.jpg"
      val imgPathRef = ImagePathReference(relPath, GeneratedSource, alt = Some("text"))
      val target = InternalTarget(relPath).relativeTo(refPath)
      
      rewrittenTreeDoc(imgPathRef, _.staticDoc(absPath)) should be(p(
        Image(target, alt = Some("text"))
      ))
    }

    "produce an invalid span for an unresolved reference" in {
      val relPath = "../tree-2/doc99.md#ref"
      rewrittenTreeDoc(pathRef(relPath)) should be(p(
        invalidSpan(s"unresolved internal reference: $relPath", s"[<$relPath>]")
      ))
    }

    "produce an invalid span for a reference to a markup document with fewer target formats than the source" in {
      val relPath = "doc-4.md#target-4"
      val msg = s"document for all output formats cannot reference document '$relPath' " +
        s"with restricted output formats unless html is one of the formats and siteBaseUrl is defined"
      
      rewrittenTreeDoc(pathRef(relPath), _.doc4.config(targetFormats("pdf"))) should be(p(
        invalidSpan(msg, s"[<$relPath>]")
      ))
    }

    "produce an invalid span for a reference to a static document with fewer target formats than the source" in {
      val relPath = "../static/doc.html"
      val absPath = Root / "static" / "doc.html"
      val msg = s"document for all output formats cannot reference document '$relPath' " +
        "with restricted output formats unless html is one of the formats and siteBaseUrl is defined"
      
      rewrittenTreeDoc(pathRef(relPath), _.staticDoc(absPath, "pdf")) should be(p(
        invalidSpan(msg, s"[<$relPath>]")
      ))
    }

    "allow links to missing target documents when one of the parent trees has the validateLinks flag set to false" in {
      rewrittenTreeDoc(pathRef("doc99.md#ref"), _
        .doc4.config(targetFormats("pdf"))
        .tree1.config(noLinkValidation)
      ) should be(p(
        internalLink(RelativePath.parse("doc99.md#ref"))
      ))
    }

    "add a restricted target format parameter for a reference to a markup document with fewer target formats than the source when siteBaseURL is defined" in {
      val relPath = "doc-4.md#target-4"
      rewrittenTreeDoc(pathRef(relPath), _.doc4.config(targetFormats("html"))) should be(p(
        internalLink(RelativePath.parse(relPath), TargetFormats.Selected("html"))
      ))
    }

    "add a restricted target format parameter for a reference to a static document with fewer target formats than the source when siteBaseURL is defined" in {
      val relPath = "../static/doc.html"
      val absPath = Root / "static" / "doc.html"
      rewrittenTreeDoc(pathRef(relPath), _.staticDoc(absPath, "html")) should be(p(
        internalLink(RelativePath.parse(relPath), TargetFormats.Selected("html"))
      ))
    }

    "avoid validation for references beyond the virtual root" in {
      val relPath = "../../doc99.md#ref"
      rewrittenTreeDoc(pathRef(relPath)) should be(p(
        extLink(relPath)
      ))
    }

    "resolve a link id reference to a target in the same tree" in {
      rewrittenTreeDoc(linkIdRef("target-4")) should be(p(internalLink(RelativePath.parse("doc-4.md#target-4"))))
    }

    "resolve a link id reference to a header with a duplicate id by precedence" in {
      def header (level: Int): Block = Header(level, Seq(Text("Header")))
      
      rewrittenTreeDoc(linkIdRef("header"), _
        .doc1.content(header(1))
        .doc2.content(header(2))
      ) should be(p(
        internalLink(RelativePath.parse("../doc-1.md#header"))
      ))
    }

  }


  "The rewrite rules for link aliases" should {

    "resolve indirect link references" in {
      val rootElem = root(p(pathRef()), LinkAlias("name", "ref"), InternalLinkTarget(Id("ref")))
      rewritten(rootElem) should be(root(p(docLink("ref")), InternalLinkTarget(Id("ref"))))
    }

    "replace an unresolvable reference to a link alias with an invalid span" in {
      val rootElem = root(p(pathRef()), LinkAlias("name", "ref"))
      rewritten(rootElem) should be(root(p(invalidSpan("unresolved link alias: ref", "[<name>]"))))
    }

    "replace circular indirect references with invalid spans" in {
      val rootElem = root(p(pathRef()), LinkAlias("name", "ref"), LinkAlias("ref", "name"))
      rewritten(rootElem) should be(root(p(invalidSpan("circular link reference: ref", "[<name>]"))))
    }

  }


  "The rewrite rules for image id references" should {

    "resolve external link references" in {
      val rootElem = root(p(simpleImgRef()), LinkDefinition("name", ExternalTarget("http://foo.com/bar.jpg")))
      rewritten(rootElem) should be(root(p(Image(ExternalTarget("http://foo.com/bar.jpg"), alt = Some("text")))))
    }

    "resolve internal link references" in {
      val rootElem = root(p(simpleImgRef()), LinkDefinition("name", InternalTarget(RelativePath.parse("foo.jpg"))))
      rewritten(rootElem) should be(root(p(Image(InternalTarget(Root / "foo.jpg").relativeTo(refPath), alt = Some("text")))))
    }

    "replace an unresolvable reference with an invalid span" in {
      val rootElem = root(p(simpleImgRef()))
      rewritten(rootElem) should be(root(p(invalidSpan("unresolved image reference: name", "!<name>"))))
    }
  }

  "The rewrite rules for header ids" should {

    "create the id for the header based on the header text" in {
      val rootElem = root(Header(1, List(Text("Header"))))
      rewritten(rootElem) should be(root(Title(List(Text("Header")), Id("header") + Style.title)))
    }

  }


  "The rewrite rules for decorated headers" should {

    "set the level of the header in a flat list of headers" in {
      val rootElem = root(
        DecoratedHeader(Underline('#'), List(Text("Title")), GeneratedSource),
        DecoratedHeader(Underline('#'), List(Text("Header 1")), GeneratedSource),
        DecoratedHeader(Underline('#'), List(Text("Header 2")), GeneratedSource)
      )
      rewritten(rootElem) should be(root(
        Title(List(Text("Title")), Id("title") + Style.title),
        Section(Header(1, List(Text("Header 1")), Id("header-1") + Style.section), Nil),
        Section(Header(1, List(Text("Header 2")), Id("header-2") + Style.section), Nil)
      ))
    }

    "set the level of the header in a nested list of headers" in {
      val rootElem = root(
        DecoratedHeader(Underline('#'), List(Text("Title")), GeneratedSource),
        DecoratedHeader(Underline('#'), List(Text("Header 1")), GeneratedSource),
        DecoratedHeader(Underline('-'), List(Text("Header 2")), GeneratedSource),
        DecoratedHeader(Underline('#'), List(Text("Header 3")), GeneratedSource)
      )
      rewritten(rootElem) should be(root(
        Title(List(Text("Title")), Id("title") + Style.title),
        Section(Header(1, List(Text("Header 1")), Id("header-1") + Style.section), List(
          Section(Header(2, List(Text("Header 2")), Id("header-2") + Style.section), Nil))),
        Section(Header(1, List(Text("Header 3")), Id("header-3") + Style.section), Nil)))
    }
    
    "not create title nodes in the default configuration for orphan documents" in {
      val rootElem = root(
        DecoratedHeader(Underline('#'), List(Text("Title")), GeneratedSource),
        DecoratedHeader(Underline('#'), List(Text("Header 1")), GeneratedSource),
        DecoratedHeader(Underline('#'), List(Text("Header 2")), GeneratedSource)
      )
      rewritten(rootElem, withTitles = false) should be(root(
        Section(Header(1, List(Text("Title")), Id("title") + Style.section), Nil),
        Section(Header(1, List(Text("Header 1")), Id("header-1") + Style.section), Nil),
        Section(Header(1, List(Text("Header 2")), Id("header-2") + Style.section), Nil)
      ))
    }

  }


  "The link resolver for duplicate ids" should {

    "remove the id from all elements with duplicate ids" in {
      val target1a = Citation("name", List(p("citation 1")))
      val target1b = Citation("name", List(p("citation 2")))
      val msg = "More than one link target with id 'name' in path /doc"
      val rootElem = root(target1a, target1b)
      rewritten(rootElem) should be(root
      (invalidBlock(msg, target1a), invalidBlock(msg, target1b)))
    }

    "remove invalid external link definitions altogether" in {
      val target2a = LinkDefinition("id2", ExternalTarget("http://foo/"))
      val target2b = LinkDefinition("id2", ExternalTarget("http://bar/"))
      val rootElem = root(target2a, target2b)
      rewritten(rootElem) should be(root())
    }

    "replace ambiguous references to duplicate ids with invalid spans" in {
      val target1a = LinkDefinition("name", ExternalTarget("http://foo/1"))
      val target1b = LinkDefinition("name", ExternalTarget("http://foo/2"))
      val msg = "More than one link definition with id 'name' in path /doc"
      val rootElem = root(p(linkIdRef()), target1a, target1b)
      rewritten(rootElem) should be(root(p(invalidSpan(msg, "<<name>>"))))
    }

    "replace ambiguous references for a link alias pointing to duplicate ids with invalid spans" in {
      val target = InternalLinkTarget(Id("ref"))
      val targetMsg = "More than one link target with id 'ref' in path /doc"
      val invalidTarget = invalidBlock(targetMsg, InternalLinkTarget())
      val rootElem = root(p(pathRef()), LinkAlias("name", "ref"), target, target)
      rewritten(rootElem) should be(root(p(invalidSpan(targetMsg, "[<name>]")), invalidTarget, invalidTarget))
    }

  }


}
