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

package laika.tree

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import laika.tree.Elements._
import laika.tree.Documents._
import laika.parse.rst.Elements._
import laika.tree.helper.ModelBuilder
 
class RewriteRulesSpec extends FlatSpec 
                  with ShouldMatchers
                  with ModelBuilder {

  
  def rewritten (root: RootElement) = new Document(Root, Nil, DocumentInfo(), root, None, null).rewrite.content
  
  def invalidSpan (message: String, fallback: String) =
      InvalidSpan(SystemMessage(laika.tree.Elements.Error, message), Text(fallback))
      
  def invalidBlock (message: String, fallback: Block) =
      InvalidBlock(SystemMessage(laika.tree.Elements.Error, message), fallback)
      
  def invalidSpan (message: String, fallback: Span) =
      InvalidSpan(SystemMessage(laika.tree.Elements.Error, message), fallback)
      
  def fnRefs (labels: FootnoteLabel*) = p((labels map { label => FootnoteReference(label,toSource(label))}):_*)

  def fnLinks (labels: (String,String)*) = p((labels map { label => FootnoteLink(label._1,label._2)}):_*)
  
  def fn (label: FootnoteLabel, num: Any) = FootnoteDefinition(label, List(p("footnote"+num)))

  def fn (id: String, label: String) = Footnote(label, List(p("footnote"+label)), Id(id))

  def fn (label: String) = Footnote(label, List(p("footnote"+label)))
  
  def simpleLinkRef (id: String = "name") = LinkReference(List(txt("text")), id, "text")
 
  def extLink (url: String) = ExternalLink(List(txt("text")), url)

  def intLink (ref: String) = InternalLink(List(txt("text")), ref)
  
  def simpleImgRef (id: String = "name") = ImageReference("text", id, "text")
  
   
  "The rewrite rules for citations" should "retain a single reference when it has a matching target" in {
    val document = doc(p(CitationReference("label","[label]_")), Citation("label", List(p("citation"))))
    val resolved = doc(p(CitationLink("label","label")), Citation("label", List(p("citation")),Id("label")))
    rewritten (document) should be (resolved)
  }
  
  it should "rewrite reference ids containing special characters" in {
    val document = doc(p(CitationReference("§label","[§label]_")), Citation("§label", List(p("citation"))))
    val resolved = doc(p(CitationLink("label","§label")), Citation("§label", List(p("citation")),Id("label")))
    rewritten (document) should be (resolved)
  }
  
  it should "retain multiple references when they all have a matching targets" in {
    val document = doc(p(CitationReference("label1","[label1]_"),CitationReference("label2","[label2]_"),CitationReference("label1","[label1]_")), 
        Citation("label1", List(p("citation1"))),Citation("label2", List(p("citation2"))))
    val resolved = doc(p(CitationLink("label1","label1"),CitationLink("label2","label2"),CitationLink("label1","label1")), 
        Citation("label1", List(p("citation1")),Id("label1")),Citation("label2", List(p("citation2")),Id("label2")))
    rewritten (document) should be (resolved)
  }
  
  it should "replace a reference with an unknown label with an invalid span" in {
    val document = doc(p(CitationReference("label1","[label1]_")), Citation("label2", List(p("citation2"))))
    rewritten (document) should be (doc(p(invalidSpan("unresolved citation reference: label1", "[label1]_")), 
        Citation("label2", List(p("citation2")),Id("label2"))))
  }
  
  
  
  "The rewrite rules for footnotes" should "retain a group of footnotes with a mix of explicit numeric and autonumber labels" in {
    val document = doc(fnRefs(Autonumber, NumericLabel(1), Autonumber), 
        fn(Autonumber, 2), fn(NumericLabel(1), 1), fn(Autonumber, 3))
    val resolved = doc(fnLinks(("2","2"), ("1","1"), ("3","3")), 
        fn("2","2"), fn("1","1"), fn("3","3"))
    rewritten (document) should be (resolved)
  }
  
  it should "retain a group of footnotes with a mix of explicit numeric, autonumber and autonumber-labeled footnotes" in {
    val document = doc(fnRefs(NumericLabel(2), Autonumber, AutonumberLabel("label")), 
        fn(NumericLabel(2), 2), fn(AutonumberLabel("label"), 1), fn(Autonumber, 3))
    val resolved = doc(fnLinks(("2","2"), ("3","3"), ("label","1")), 
        fn("2","2"), fn("label","1"), fn("3","3"))
    rewritten (document) should be (resolved)
  }
  
  it should "retain a group of footnotes with autosymbol labels" in {
    val document = doc(fnRefs(Autosymbol, Autosymbol, Autosymbol), 
        fn(Autosymbol, "*"), fn(Autosymbol, "\u2020"), fn(Autosymbol, "\u2021"))
    val resolved = doc(fnLinks(("id-1","*"), ("id-2","\u2020"), ("id-3","\u2021")), 
        fn("id-1","*"), fn("id-2","\u2020"), fn("id-3","\u2021"))
    rewritten (document) should be (resolved)
  }
  
  it should "replace references with unresolvable autonumber or numeric labels with invalid spans" in {
    val document = doc(fnRefs(NumericLabel(2), AutonumberLabel("labelA")), 
        fn(NumericLabel(3), 3), fn(AutonumberLabel("labelB"), 1))
    val resolved = doc(p(invalidSpan("unresolved footnote reference: 2", "[2]_"), 
                        invalidSpan("unresolved footnote reference: labelA", "[#labelA]_")), 
        fn("3","3"), fn("labelb","1"))
    rewritten (document) should be (resolved)
  }
  
  it should "replace surplus autonumber references with invalid spans" in {
    val document = doc(fnRefs(Autonumber, Autonumber), fn(Autonumber, 1))
    val resolved = doc(p(FootnoteLink("1","1"), invalidSpan("too many autonumber references", "[#]_")), 
        fn("1","1"))
    rewritten (document) should be (resolved)
  }
  
  it should "replace surplus autosymbol references with invalid spans" in {
    val document = doc(fnRefs(Autosymbol, Autosymbol), fn(Autosymbol, "*"))
    val resolved = doc(p(FootnoteLink("id-1","*"), invalidSpan("too many autosymbol references", "[*]_")), 
        fn("id-1","*"))
    rewritten (document) should be (resolved)
  }
  

  
  "The rewrite rules for link references" should "resolve external link references" in {
    val document = doc(p(simpleLinkRef()), ExternalLinkDefinition("name", "http://foo/"))
    rewritten (document) should be (doc(p(extLink("http://foo/"))))
  }
  
  it should "resolve internal link references" in {
    val document = doc(p(simpleLinkRef()), InternalLinkTarget(Id("name")))
    rewritten (document) should be (doc(p(intLink("name")), InternalLinkTarget(Id("name"))))
  }
  
  it should "resolve indirect link references" in {
    val document = doc(p(simpleLinkRef()), LinkAlias("name","ref"), InternalLinkTarget(Id("ref")))
    rewritten (document) should be (doc(p(intLink("ref")), InternalLinkTarget(Id("ref"))))
  }
  
  it should "resolve anonymous link references" in {
    val document = doc(p(simpleLinkRef(""), simpleLinkRef("")), ExternalLinkDefinition("", "http://foo/"), ExternalLinkDefinition("", "http://bar/"))
    rewritten (document) should be (doc(p(extLink("http://foo/"), extLink("http://bar/"))))
  }
  
  it should "replace an unresolvable reference with an invalid span" in {
    val document = doc(p(simpleLinkRef()))
    rewritten (document) should be (doc(p(invalidSpan("unresolved link reference: name", txt("text")))))
  }
  
  it should "replace an unresolvable reference to a link alias with an invalid span" in {
    val document = doc(p(simpleLinkRef()), LinkAlias("name","ref"))
    rewritten (document) should be (doc(p(invalidSpan("unresolved link alias: ref", txt("text")))))
  }
  
  it should "replace a surplus anonymous reference with an invalid span" in {
    val document = doc(p(simpleLinkRef("")))
    rewritten (document) should be (doc(p(invalidSpan("too many anonymous link references", txt("text")))))
  }
  
  it should "replace circular indirect references with invalid spans" in {
    val document = doc(p(simpleLinkRef()), LinkAlias("name","ref"), LinkAlias("ref","name"))
    rewritten (document) should be (doc(p(invalidSpan("circular link reference: name", txt("text")))))
  }
  
  it should "resolve references when some parent element also gets rewritten" in {
    val document = doc(DecoratedHeader(Underline('#'), List(txt("text1"), simpleLinkRef()), Id("header")), ExternalLinkDefinition("name", "http://foo/"))
    rewritten (document) should be (doc(Section(Header(1, List(txt("text1"), extLink("http://foo/")), Id("header")), Nil, Id("header"))))
  }
  
  
  "The rewrite rules for image references" should "resolve external link references" in {
    val document = doc(p(simpleImgRef()), ExternalLinkDefinition("name", "foo.jpg"))
    rewritten (document) should be (doc(p(img("text", "foo.jpg"))))
  }
  
  it should "replace an unresolvable reference with an invalid span" in {
    val document = doc(p(simpleImgRef()))
    rewritten (document) should be (doc(p(invalidSpan("unresolved image reference: name", txt("text")))))
  }
  
  
  "The rewrite rules for header ids" should "retain the id of a header" in {
    val document = doc(Header(1, List(Text("text")), Id("header")))
    rewritten (document) should be (doc(Section(Header(1, List(Text("text")), Id("header")), Nil, Id("header"))))
  }
  
  it should "append numbers to duplicate ids" in {
    val document = doc(Header(1, List(Text("text1")), Id("header")), Header(1, List(Text("text2")), Id("header")))
    rewritten (document) should be (doc(Section(Header(1, List(Text("text1")), Id("header")), Nil, Id("header")), 
                                        Section(Header(1, List(Text("text2")), Id("header-1")), Nil, Id("header-1"))))
  }
  
  
  "The rewrite rules for decorated headers" should "set the level of the header in a flat list of headers" in {
    val document = doc(DecoratedHeader(Underline('#'), List(Text("text1")), Id("header1")),
                       DecoratedHeader(Underline('#'), List(Text("text2")), Id("header2")))
    rewritten (document) should be (doc(
        Section(Header(1, List(Text("text1")), Id("header1")), Nil, Id("header1")),
        Section(Header(1, List(Text("text2")), Id("header2")), Nil, Id("header2"))))
  }
  
  it should "set the level of the header in a nested list of headers" in {
    val document = doc(DecoratedHeader(Underline('#'), List(Text("text")), Id("header1")),
                       DecoratedHeader(Underline('-'), List(Text("nested")), Id("header2")),
                       DecoratedHeader(Underline('#'), List(Text("text")), Id("header3")))
    rewritten (document) should be (doc(
        Section(Header(1, List(Text("text")), Id("header1")), List(
            Section(Header(2, List(Text("nested")), Id("header2")), Nil, Id("header2"))), Id("header1")),
        Section(Header(1, List(Text("text")), Id("header3")), Nil, Id("header3"))))
  }
  
  it should "append numbers to duplicate ids" in {
    val document = doc(DecoratedHeader(Underline('#'), List(Text("text1")), Id("header")),
                       DecoratedHeader(Underline('#'), List(Text("text2")), Id("header")))
    rewritten (document) should be (doc(
        Section(Header(1, List(Text("text1")), Id("header")), Nil, Id("header")),
        Section(Header(1, List(Text("text2")), Id("header-1")), Nil, Id("header-1"))))
  }
  
  
  "The link resolver" should "remove the id from all elements with duplicate ids" in {
    val target1a = Citation("name", List(p("citation")))
    val target1b = fn(AutonumberLabel("name"), 1)
    val msg = "duplicate target id: name"
    val document = doc(target1a,target1b)
    rewritten (document) should be (doc
        (invalidBlock(msg,target1a),invalidBlock(msg,Footnote("1", List(p("footnote1"))))))
  }
  
  it should "remove the id from elements with duplicate ids, but remove invalid external link definitions altogether" in {
    val target1 = Citation("id1", List(p("citation")))
    val target2a = fn(AutonumberLabel("id2"), 1)
    val target2b = ExternalLinkDefinition("id2", "http://foo/")
    val msg = "duplicate target id: id2"
    val document = doc(target1,target2a,target2b)
    rewritten (document) should be (doc
        (target1.copy(options = Id("id1")),invalidBlock(msg,Footnote("1", List(p("footnote1"))))))
  }
  
  it should "replace ambiguous references to duplicate ids with invalid spans" in {
    val target1a = ExternalLinkDefinition("name", "http://foo/1")
    val target1b = ExternalLinkDefinition("name", "http://foo/2")
    val msg = "duplicate target id: name"
    val document = doc(p(simpleLinkRef()),target1a,target1b)
    rewritten (document) should be (doc(p(invalidSpan(msg, "text"))))
  }
  
  it should "replace ambiguous references a link alias pointing to duplicate ids with invalid spans" in {
    val target1a = ExternalLinkDefinition("id2", "http://foo/1")
    val target1b = ExternalLinkDefinition("id2", "http://foo/2")
    val msg = "duplicate target id: id2"
    val document = doc(p(simpleLinkRef()),LinkAlias("name","id2"),target1a,target1b)
    rewritten (document) should be (doc(p(invalidSpan(msg, "text"))))
  }
 
  
}