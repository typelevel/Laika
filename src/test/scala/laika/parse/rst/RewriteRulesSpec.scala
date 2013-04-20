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

package laika.parse.rst

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import laika.tree.Elements._
import laika.parse.rst.Elements._
import laika.tree.helper.ModelBuilder
 
class RewriteRulesSpec extends FlatSpec 
                  with ShouldMatchers
                  with ModelBuilder {

  
  def rewritten (doc: Document) = RewriteRules.applyDefaults(doc)
  
  def invalidSpan (message: String, fallback: String) =
      InvalidSpan(SystemMessage(laika.tree.Elements.Error, message), Text(fallback))
      
  def invalidSpan (message: String, fallback: Span) =
      InvalidSpan(SystemMessage(laika.tree.Elements.Error, message), fallback)
      
  def fnRefs (labels: FootnoteLabel*) = p((labels map { label => FootnoteReference(label,toSource(label))}):_*)

  def fnLinks (labels: (String,String)*) = p((labels map { label => FootnoteLink(label._1,label._2)}):_*)
  
  def fn (label: FootnoteLabel, num: Any) = FootnoteDefinition(label, List(p("footnote"+num)))

  def fn (id: String, label: String) = Footnote(id, label, List(p("footnote"+label)))
  
  def simpleLinkRef (id: String = "id") = LinkReference(List(txt("text")), id, "text")
 
  def extLink (url: String) = ExternalLink(List(txt("text")), url)

  def intLink (ref: String) = InternalLink(List(txt("text")), ref)
  
      
  "The rewrite rules for substitutions" should "replace a single reference with the target span" in {
    val document = doc(p(SubstitutionReference("id")), SubstitutionDefinition("id", txt("subst")))
    rewritten (document) should be (doc(p("subst")))
  }
  
  it should "replace multiple occurrences of the same reference with the same target span" in {
    val document = doc(p(SubstitutionReference("id"), txt(" foo "), SubstitutionReference("id")), SubstitutionDefinition("id", txt("subst")))
    rewritten (document) should be (doc(p(txt("subst"),txt(" foo "),txt("subst"))))
  }
  
  it should "replace a reference with an unknown substitution id with an invalid span" in {
    val document = doc(p(SubstitutionReference("id1")), SubstitutionDefinition("id2", txt("subst")))
    rewritten (document) should be (doc(p(invalidSpan("unknown substitution id: id1", "|id1|"))))
  }
  
  
  
  "The rewrite rules for interpreted text roles" should "replace a single reference with the result of applying the role function" in {
    val document = doc(p(InterpretedText("id", "foo","")), CustomizedTextRole("id", s => txt(":"+s+":")))
    rewritten (document) should be (doc(p(":foo:")))
  }
  
  it should "replace multiple references with the result of applying corresponding role functions" in {
    val document = doc(p(InterpretedText("id1", "foo",""),InterpretedText("id2", "bar",""),InterpretedText("id1", "baz","")), 
        CustomizedTextRole("id1", s => txt(":"+s+":")), CustomizedTextRole("id2", s => txt("."+s+".")))
    rewritten (document) should be (doc(p(txt(":foo:"),txt(".bar."),txt(":baz:"))))
  }
  
  it should "replace an unknown text role with an invalid span" in {
    val document = doc(p(InterpretedText("id1", "foo", "")), CustomizedTextRole("id2", s => txt("."+s+".")))
    rewritten (document) should be (doc(p(invalidSpan("unknown text role: id1", "`foo`"))))
  }
  
  
  
  "The rewrite rules for citations" should "retain a single reference when it has a matching target" in {
    val document = doc(p(CitationReference("label","[label]_")), Citation("label", List(p("citation"))))
    val resolved = doc(p(CitationLink("label")), Citation("label", List(p("citation"))))
    rewritten (document) should be (resolved)
  }
  
  it should "retain multiple references when they all have a matching targets" in {
    val document = doc(p(CitationReference("label1","[label1]_"),CitationReference("label2","[label2]_"),CitationReference("label1","[label1]_")), 
        Citation("label1", List(p("citation1"))),Citation("label2", List(p("citation2"))))
    val resolved = doc(p(CitationLink("label1"),CitationLink("label2"),CitationLink("label1")), 
        Citation("label1", List(p("citation1"))),Citation("label2", List(p("citation2"))))
    rewritten (document) should be (resolved)
  }
  
  it should "replace a reference with an unknown label with an invalid span" in {
    val document = doc(p(CitationReference("label1","[label1]_")), Citation("label2", List(p("citation2"))))
    rewritten (document) should be (doc(p(invalidSpan("unresolved citation reference: label1", "[label1]_")), Citation("label2", List(p("citation2")))))
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
    val resolved = doc(fnLinks(("*","*"), ("\u2020","\u2020"), ("\u2021","\u2021")), 
        fn("*","*"), fn("\u2020","\u2020"), fn("\u2021","\u2021"))
    rewritten (document) should be (resolved)
  }
  
  it should "replace references with unresolvable autonumber or numeric labels with invalid spans" in {
    val document = doc(fnRefs(NumericLabel(2), AutonumberLabel("labelA")), 
        fn(NumericLabel(3), 3), fn(AutonumberLabel("labelB"), 1))
    val resolved = doc(p(invalidSpan("unresolved footnote reference: 2", "[2]_"), 
                        invalidSpan("unresolved footnote reference: labelA", "[#labelA]_")), 
        fn("3","3"), fn("labelB","1"))
    rewritten (document) should be (resolved)
  }
  
  it should "replace surplus autonumber references with invalid spans" in {
    val document = doc(fnRefs(Autonumber, Autonumber), fn(Autonumber, 1))
    val resolved = doc(p(FootnoteLink("1","1"), invalidSpan("too many autonumer references", "[#]_")), 
        fn("1","1"))
    rewritten (document) should be (resolved)
  }
  
  it should "replace surplus autosymbol references with invalid spans" in {
    val document = doc(fnRefs(Autosymbol, Autosymbol), fn(Autosymbol, "*"))
    val resolved = doc(p(FootnoteLink("*","*"), invalidSpan("too many autosymbol references", "[*]_")), 
        fn("*","*"))
    rewritten (document) should be (resolved)
  }
  

  
  "The rewrite rules for link references" should "resolve external link references" in {
    val document = doc(p(simpleLinkRef()), ExternalLinkDefinition("id", "http://foo/"))
    rewritten (document) should be (doc(p(extLink("http://foo/"))))
  }
  
  it should "resolve internal link references" in {
    val document = doc(p(simpleLinkRef()), InternalLinkTarget("id"))
    rewritten (document) should be (doc(p(intLink("#id")), InternalLinkTarget("id")))
  }
  
  it should "resolve indirect link references" in {
    val document = doc(p(simpleLinkRef()), IndirectLinkDefinition("id",simpleLinkRef("ref")), InternalLinkTarget("ref"))
    rewritten (document) should be (doc(p(intLink("#ref")), InternalLinkTarget("ref")))
  }
  
  it should "resolve anonymous link references" in {
    val document = doc(p(simpleLinkRef(""), simpleLinkRef("")), ExternalLinkDefinition("", "http://foo/"), ExternalLinkDefinition("", "http://bar/"))
    rewritten (document) should be (doc(p(extLink("http://foo/"), extLink("http://bar/"))))
  }
  
  it should "replace an unresolvable reference with an invalid span" in {
    val document = doc(p(simpleLinkRef()))
    rewritten (document) should be (doc(p(invalidSpan("unresolved link reference: id", txt("text")))))
  }
  
  it should "replace a surplus anonymous reference with an invalid span" in {
    val document = doc(p(simpleLinkRef("")))
    rewritten (document) should be (doc(p(invalidSpan("too many anonymous link references", txt("text")))))
  }
  
  it should "replace circular indirect references with invalid spans" in {
    val document = doc(p(simpleLinkRef()), IndirectLinkDefinition("id",simpleLinkRef("ref")), IndirectLinkDefinition("ref",simpleLinkRef("id")))
    rewritten (document) should be (doc(p(invalidSpan("circular link reference: id", txt("text")))))
  }

  
}