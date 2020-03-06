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
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.rst.ast.Underline
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RewriteRulesSpec extends AnyWordSpec with Matchers with ModelBuilder {


  def rewritten (root: RootElement): RootElement = {
    val doc = Document(Path.Root / "doc", root)
    val rules = OperationConfig.default.rewriteRules(DocumentCursor(doc))
    doc.rewrite(rules).content
  }

  def invalidSpan (message: String, fallback: String): InvalidSpan = InvalidElement(message, fallback).asSpan

  def invalidBlock (message: String, fallback: Block): InvalidBlock =
    InvalidBlock(SystemMessage(MessageLevel.Error, message), fallback)

  def invalidSpan (message: String, fallback: Span): InvalidSpan =
    InvalidSpan(SystemMessage(MessageLevel.Error, message), fallback)

  def fnRefs (labels: FootnoteLabel*): Paragraph = p((labels map { label => FootnoteReference(label,toSource(label))}):_*)

  def fnLinks (labels: (String,String)*): Paragraph = p((labels map { label => FootnoteLink(label._1,label._2)}):_*)

  def fn (label: FootnoteLabel, num: Any) = FootnoteDefinition(label, List(p(s"footnote$num")))

  def fn (id: String, label: String) = Footnote(label, List(p(s"footnote$label")), Id(id))

  def fn (label: String) = Footnote(label, List(p(s"footnote$label")))

  def simpleLinkRef (id: String = "name") = LinkReference(List(Text("text")), id, "text")

  def crossLinkRef (id: String = "name") = CrossReference(List(Text("text")), RelativePath.parse(s"#$id"), "text")

  def extLink (url: String) = ExternalLink(List(Text("text")), url)

  def intLink (ref: String) = InternalLink(List(Text("text")), ref)

  def simpleImgRef (id: String = "name") = ImageReference("text", id, "text")


  "The rewrite rules for citations" should {

    "retain a single reference when it has a matching target" in {
      val rootElem = root(p(CitationReference("label", "[label]_")), Citation("label", List(p("citation"))))
      val resolved = root(p(CitationLink("label", "label")), Citation("label", List(p("citation")), Id("__cit-label")))
      rewritten(rootElem) should be(resolved)
    }

    "rewrite reference ids containing special characters" ignore {
      val rootElem = root(p(CitationReference("§label", "[§label]_")), Citation("§label", List(p("citation"))))
      val resolved = root(p(CitationLink("label", "§label")), Citation("§label", List(p("citation")), Id("label")))
      rewritten(rootElem) should be(resolved)
    }

    "retain multiple references when they all have a matching targets" in {
      val rootElem = root(p(CitationReference("label1", "[label1]_"), CitationReference("label2", "[label2]_"), CitationReference("label1", "[label1]_")),
        Citation("label1", List(p("citation1"))), Citation("label2", List(p("citation2"))))
      val resolved = root(p(CitationLink("label1", "label1"), CitationLink("label2", "label2"), CitationLink("label1", "label1")),
        Citation("label1", List(p("citation1")), Id("__cit-label1")), Citation("label2", List(p("citation2")), Id("__cit-label2")))
      rewritten(rootElem) should be(resolved)
    }

    "replace a reference with an unknown label with an invalid span" in {
      val rootElem = root(p(CitationReference("label1", "[label1]_")), Citation("label2", List(p("citation2"))))
      rewritten(rootElem) should be(root(p(invalidSpan("unresolved citation reference: label1", "[label1]_")),
        Citation("label2", List(p("citation2")), Id("__cit-label2"))))
    }
  }


  "The rewrite rules for footnotes" should {

    "retain a group of footnotes with a mix of explicit numeric and autonumber labels" ignore {
      val rootElem = root(fnRefs(Autonumber, NumericLabel(1), Autonumber),
        fn(Autonumber, 2), fn(NumericLabel(1), 1), fn(Autonumber, 3))
      val resolved = root(fnLinks(("2", "2"), ("1", "1"), ("3", "3")),
        fn("2", "2"), fn("1", "1"), fn("3", "3"))
      rewritten(rootElem) should be(resolved)
    }

    "retain a group of footnotes with a mix of explicit numeric, autonumber and autonumber-labeled footnotes" ignore {
      val rootElem = root(fnRefs(NumericLabel(2), Autonumber, AutonumberLabel("label")),
        fn(NumericLabel(2), 2), fn(AutonumberLabel("label"), 1), fn(Autonumber, 3))
      val resolved = root(fnLinks(("__fnl-2", "2"), ("3", "3"), ("label", "1")),
        fn("2", "2"), fn("label", "1"), fn("3", "3"))
      rewritten(rootElem) should be(resolved)
    }

    "retain a group of footnotes with autosymbol labels" ignore {
      // TODO - needs unique flag on selector
      val rootElem = root(fnRefs(Autosymbol, Autosymbol, Autosymbol),
        fn(Autosymbol, "*"), fn(Autosymbol, "\u2020"), fn(Autosymbol, "\u2021"))
      val resolved = root(fnLinks(("id-1", "*"), ("id-2", "\u2020"), ("id-3", "\u2021")),
        fn("id-1", "*"), fn("id-2", "\u2020"), fn("id-3", "\u2021"))
      rewritten(rootElem) should be(resolved)
    }

    "replace references with unresolvable autonumber or numeric labels with invalid spans" in {
      val rootElem = root(fnRefs(NumericLabel(2), AutonumberLabel("labelA")),
        fn(NumericLabel(3), 3), fn(AutonumberLabel("labelB"), 1))
      val resolved = root(p(invalidSpan("unresolved footnote reference: 2", "[2]_"),
        invalidSpan("unresolved footnote reference: labelA", "[#labelA]_")),
        fn("__fnl-3", "3"), fn("labelB", "1"))
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



  "The rewrite rules for link references" should {

    "resolve external link references" in {
      val rootElem = root(p(simpleLinkRef()), ExternalLinkDefinition("name", "http://foo/"))
      rewritten(rootElem) should be(root(p(extLink("http://foo/"))))
    }

    "resolve internal link references" ignore {
      // TODO - after merging of CrossLink + InternalLink
      val rootElem = root(p(crossLinkRef()), InternalLinkTarget(Id("name")))
      rewritten(rootElem) should be(root(p(intLink("name")), InternalLinkTarget(Id("name"))))
    }

    "use the headers text as link text when linking to a header" ignore {
      val header = Header(1, Seq(Text("Title 1")), Id("title-1"))
      val rootElem = root(p(LinkReference(List(Text("title-1")), "title-1", "title-1")), header)
      rewritten(rootElem) should be(root(p(InternalLink(List(Text("Title 1")), "title-1")), Title(header.content, header.options + Styles("title"))))
    }

    "resolve indirect link references" ignore {
      val rootElem = root(p(simpleLinkRef()), LinkAlias("name", "ref"), InternalLinkTarget(Id("ref")))
      rewritten(rootElem) should be(root(p(intLink("ref")), InternalLinkTarget(Id("ref"))))
    }

    "resolve anonymous link references" ignore {
      // TODO - revive after global/unique flags have been introduced
      val rootElem = root(p(simpleLinkRef(""), simpleLinkRef("")), ExternalLinkDefinition("", "http://foo/"), ExternalLinkDefinition("", "http://bar/"))
      rewritten(rootElem) should be(root(p(extLink("http://foo/"), extLink("http://bar/"))))
    }

    "replace an unresolvable reference with an invalid span" in {
      val rootElem = root(p(simpleLinkRef()))
      rewritten(rootElem) should be(root(p(invalidSpan("unresolved link reference: name", "text"))))
    }

    "replace an unresolvable reference to a link alias with an invalid span" ignore {
      val rootElem = root(p(simpleLinkRef()), LinkAlias("name", "ref"))
      rewritten(rootElem) should be(root(p(invalidSpan("unresolved link alias: ref", "text"))))
    }

    "replace a surplus anonymous reference with an invalid span" in {
      val rootElem = root(p(simpleLinkRef("")))
      rewritten(rootElem) should be(root(p(invalidSpan("too many anonymous link references", "text"))))
    }

    "replace circular indirect references with invalid spans" ignore {
      val rootElem = root(p(simpleLinkRef()), LinkAlias("name", "ref"), LinkAlias("ref", "name"))
      rewritten(rootElem) should be(root(p(invalidSpan("circular link reference: name", "text"))))
    }

    "resolve references when some parent element also gets rewritten" in {
      val rootElem = root(DecoratedHeader(Underline('#'), List(Text("text1"), simpleLinkRef()), Id("header")), ExternalLinkDefinition("name", "http://foo/"))
      rewritten(rootElem) should be(root(Title(List(Text("text1"), extLink("http://foo/")), Id("header") + Styles("title"))))
    }
  }


  "The rewrite rules for image references" should {

    "resolve external link references" in {
      val rootElem = root(p(simpleImgRef()), ExternalLinkDefinition("name", "foo.jpg"))
      rewritten(rootElem) should be(root(p(img("text", "foo.jpg", Some(PathInfo(Path.Root / "foo.jpg", RelativePath.Current / "foo.jpg"))))))
    }

    "replace an unresolvable reference with an invalid span" in {
      val rootElem = root(p(simpleImgRef()))
      rewritten(rootElem) should be(root(p(invalidSpan("unresolved image reference: name", "text"))))
    }
  }

  "The rewrite rules for header ids" should {

    "retain the id of a header" in {
      val rootElem = root(Header(1, List(Text("text")), Id("header")))
      rewritten(rootElem) should be(root(Title(List(Text("text")), Id("header") + Styles("title"))))
    }

    "append numbers to duplicate ids" ignore {
      val rootElem = root(Header(1, List(Text("text1")), Id("header")), Header(1, List(Text("text2")), Id("header")))
      rewritten(rootElem) should be(root(Section(Header(1, List(Text("text1")), Id("header") + Styles("section")), Nil),
        Section(Header(1, List(Text("text2")), Id("header-1") + Styles("section")), Nil)))
    }
  }


  "The rewrite rules for decorated headers" should {

    "set the level of the header in a flat list of headers" in {
      val rootElem = root(DecoratedHeader(Underline('#'), List(Text("text1")), Id("header1")),
        DecoratedHeader(Underline('#'), List(Text("text2")), Id("header2")))
      rewritten(rootElem) should be(root(
        Section(Header(1, List(Text("text1")), Id("header1") + Styles("section")), Nil),
        Section(Header(1, List(Text("text2")), Id("header2") + Styles("section")), Nil)))
    }

    "set the level of the header in a nested list of headers" in {
      val rootElem = root(DecoratedHeader(Underline('#'), List(Text("text")), Id("header1")),
        DecoratedHeader(Underline('-'), List(Text("nested")), Id("header2")),
        DecoratedHeader(Underline('#'), List(Text("text")), Id("header3")))
      rewritten(rootElem) should be(root(
        Section(Header(1, List(Text("text")), Id("header1") + Styles("section")), List(
          Section(Header(2, List(Text("nested")), Id("header2") + Styles("section")), Nil))),
        Section(Header(1, List(Text("text")), Id("header3") + Styles("section")), Nil)))
    }

    "append numbers to duplicate ids" ignore {
      val rootElem = root(DecoratedHeader(Underline('#'), List(Text("text1")), Id("header")),
        DecoratedHeader(Underline('#'), List(Text("text2")), Id("header")))
      rewritten(rootElem) should be(root(
        Section(Header(1, List(Text("text1")), Id("header") + Styles("section")), Nil),
        Section(Header(1, List(Text("text2")), Id("header-1") + Styles("section")), Nil)))
    }
  }


  "The link resolver" should {

    "remove the id from all elements with duplicate ids" ignore {
      val target1a = Citation("name", List(p("citation")))
      val target1b = fn(AutonumberLabel("name"), 1)
      val msg = "duplicate target id: name"
      val rootElem = root(target1a, target1b)
      rewritten(rootElem) should be(root
      (invalidBlock(msg, target1a), invalidBlock(msg, Footnote("1", List(p("footnote1"))))))
    }

    "remove the id from elements with duplicate ids, but remove invalid external link definitions altogether" ignore {
      val target1 = Citation("id1", List(p("citation")))
      val target2a = fn(AutonumberLabel("id2"), 1)
      val target2b = ExternalLinkDefinition("id2", "http://foo/")
      val msg = "duplicate target id: id2"
      val rootElem = root(target1, target2a, target2b)
      rewritten(rootElem) should be(root
      (target1.copy(options = Id("id1")), invalidBlock(msg, Footnote("1", List(p("footnote1"))))))
    }

    "replace ambiguous references to duplicate ids with invalid spans" in {
      val target1a = ExternalLinkDefinition("name", "http://foo/1")
      val target1b = ExternalLinkDefinition("name", "http://foo/2")
      val msg = "More than one link target with id 'name' in path /doc"
      val rootElem = root(p(simpleLinkRef()), target1a, target1b)
      rewritten(rootElem) should be(root(p(invalidSpan(msg, "text"))))
    }

    "replace ambiguous references a link alias pointing to duplicate ids with invalid spans" ignore {
      val target1a = ExternalLinkDefinition("id2", "http://foo/1")
      val target1b = ExternalLinkDefinition("id2", "http://foo/2")
      val msg = "duplicate target id: id2"
      val rootElem = root(p(simpleLinkRef()), LinkAlias("name", "id2"), target1a, target1b)
      rewritten(rootElem) should be(root(p(invalidSpan(msg, "text"))))
    }
  }


}
