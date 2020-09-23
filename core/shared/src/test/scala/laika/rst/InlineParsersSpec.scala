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

package laika.rst

import laika.ast.Path.Root
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.parse.Parser
import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import laika.rst.ast.{InterpretedText, RstStyle, SubstitutionReference}
import laika.rst.ext.{ExtensionProvider, RootParserProvider}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
     
class InlineParsersSpec extends AnyFlatSpec 
                        with Matchers 
                        with ParseResultHelpers
                        with DefaultParserHelpers[List[Span]] 
                        with ModelBuilder {


  val defaultTextRole = "foo"

  val rootParser = RootParserProvider.forBundle(ExtensionProvider.forDefaultTextRole(defaultTextRole))

  val defaultParser: Parser[List[Span]] = rootParser.standaloneSpanParser


  def subst (name: String) = SubstitutionReference(name)

  def pLinkRef (id: String, text: String): LinkIdReference = LinkIdReference(Seq(Text(text)), id, s"`$text`_")

  def pLinkRef (id: String): LinkIdReference = pLinkRef(id,id)
  
  def anonPLinkRef (text: String): LinkIdReference = LinkIdReference(Seq(Text(text)), "", s"`$text`__")
  
  def linkRef (id: String): LinkIdReference = LinkIdReference(Seq(Text(id)), id, id+"_")
  
  def anonLinkRef (text: String): LinkIdReference = LinkIdReference(Seq(Text(text)), "", text+"__")
  
  
  
  "The text parser" should "parse content without any markup as plain text" in {
    Parsing ("some text") should produce (spans(Text("some text")))
  }
  
  
  "The markup recognition rules" should "recognize markup surrounded by whitespace" in {
    Parsing ("some |replaced| text") should produce (spans(Text("some "), subst("replaced"), Text(" text")))
  }

  it should "recognize markup surrounded by punctuation" in {
    Parsing ("some (|replaced|) text") should produce (spans(Text("some ("), subst("replaced"), Text(") text")))
  }
  
  it should "recognize markup at the start of the input" in {
    Parsing ("|replaced| text") should produce (spans(subst("replaced"), Text(" text")))
  }
  
  it should "recognize markup at the end of the input" in {
    Parsing ("text |replaced|") should produce (spans(Text("text "), subst("replaced")))
  }
  
  it should "recognize markup at the end and the end of the input" in {
    Parsing ("|replaced|") should produce (spans(subst("replaced")))
  }
  
  it should "ignore markup surrounded by characters (rules 1 and 4)" in {
    Parsing ("some|replaced|text") should produce (spans(Text("some|replaced|text")))
  }
  
  it should "ignore markup with a space after the start character (rule 2)" in {
    Parsing ("some | replaced| text") should produce (spans(Text("some | replaced| text")))
  }
  
  it should "ignore markup with a space before the end character (rule 3)" in {
    Parsing ("some |replaced | text") should produce (spans(Text("some |replaced | text")))
  }
  
  it should "ignore markup end characters immediately following the start character (rule 6)" in {
    Parsing ("some || text") should produce (spans(Text("some || text")))
  }
  
  it should "ignore markup characters between matching punctuation characters (rule 5)" in {
    Parsing ("some (|)replaced| text") should produce (spans(Text("some (|)replaced| text")))
  }
  
  it should "recognize markup when preceding and following characters are escaped" in {
    Parsing ("""some\ |replaced|\ text""") should produce (spans(Text("some"), subst("replaced"), Text("text")))
  }
  
  
  
  "The em parser" should "parse content enclosed in *" in {
    Parsing ("some *text* here") should produce (spans(Text("some "),Emphasized("text"),Text(" here")))
  }
  
  it should "ignore an * character when it is not matched by a second *" in {
    Parsing ("some *text here") should produce (spans(Text("some *text here")))
  }
  
  it should "ignore markup for nested spans" in {
    Parsing ("some *nested ``code`` span* here") should produce (spans(Text("some "),Emphasized("nested ``code`` span"),Text(" here")))
  }
  
  it should "ignore markup for emphasis when strong failed at the same position based on the markup recognition rules" in {
    Parsing ("a**b O(N**2)") should produce (spans(Text("a**b O(N**2)")))
  }
    
  
  
  "The strong parser" should "parse content enclosed in **" in {
    Parsing ("some **text** here") should produce (spans(Text("some "),Strong("text"),Text(" here")))
  }
  
  it should "ignore a ** sequence when it is not matched by a second **" in {
    Parsing ("some **text here") should produce (spans(Text("some **text here")))
  }
  
  
  
  "The inline literal parser" should "parse content enclosed in ``" in {
    Parsing ("some ``text`` here") should produce (spans(Text("some "),Literal("text"),Text(" here")))
  }
  
  it should "ignore a ` character when it is not matched by a second `" in {
    Parsing ("some ``text here") should produce (spans(Text("some ``text here")))
  }
  
  it should "not treat a single ` as markup when the code span is enclosed in double ``" in {
    Parsing ("some ``text`text`` here") should produce (spans(Text("some "),Literal("text`text"),Text(" here")))
  }
  
  
  
  "The substitution reference" should "parse content enclosed in |" in {
    Parsing ("some |replaced| text") should produce (spans(Text("some "), subst("replaced"), Text(" text")))
  }
  
  it should "ignore a | character when it is not matched by a second |" in {
    Parsing ("some |text here") should produce (spans(Text("some |text here")))
  }
  
  
  
  "The internal link target parser" should "parse content enclosed in _` and `" in {
    Parsing ("some _`Text` here") should produce (spans(Text("some "), Text("Text", Id("text") + RstStyle.target), Text(" here")))
  }
  
  
  
  "The interpreted text parser" should "parse content enclosed in ` with implicit default role" in {
    Parsing ("some `text` here") should produce (spans(Text("some "), InterpretedText(defaultTextRole,"text","`text`"), Text(" here")))
  }
  
  it should "parse content enclosed in ` with role prefix" in {
    Parsing ("some :role:`text` here") should produce (spans(Text("some "), InterpretedText("role","text",":role:`text`"), Text(" here")))
  }
  
  it should "parse content enclosed in ` with role suffix" in {
    Parsing ("some `text`:role: here") should produce (spans(Text("some "), InterpretedText("role","text","`text`:role:"), Text(" here")))
  }
  
  it should "parse content enclosed in ` but ignore illegal role prefix" in {
    Parsing ("some :#*#:`text` here") should produce (spans(Text("some :#*#:"), InterpretedText(defaultTextRole,"text","`text`"), Text(" here")))
  }
  
  
  
  "The citation reference parser" should "parse content enclosed between [ and ]_" in {
    Parsing ("some [text]_ here") should produce (spans(Text("some "), citRef("text"), Text(" here")))
  }
  
  
  
  "The footnote reference parser" should "parse content enclosed between [ and ]_ with autonumber label" in {
    Parsing ("some [#]_ here") should produce (spans(Text("some "), fnRef(Autonumber), Text(" here")))
  }
  
  it should "parse content enclosed between [ and ]_ with autosymbol label" in {
    Parsing ("some [*]_ here") should produce (spans(Text("some "), fnRef(Autosymbol), Text(" here")))
  }
  
  it should "parse content enclosed between [ and ]_ with an autonumber named label" in {
    Parsing ("some [#foo]_ here") should produce (spans(Text("some "), fnRef(AutonumberLabel("foo")), Text(" here")))
  }
  
  it should "parse content enclosed between [ and ]_ with a numeric label" in {
    Parsing ("some [17]_ here") should produce (spans(Text("some "), fnRef(NumericLabel(17)), Text(" here")))
  }
  
  
  private val relPath = RelativePath.parse("../foo/bar.rst#ref")
  
  "The link reference parser" should "parse a phrase link without url" in {
    Parsing ("some `link`_ here") should produce (spans(Text("some "), pLinkRef("link"), Text(" here")))
  }
  
  it should "parse an external phrase link with text and url" in {
    val spanSeq = List(link(Text("link")).url("http://foo.com").toLink, LinkDefinition("link", ExternalTarget("http://foo.com")))
    Parsing ("some `link<http://foo.com>`_ here") should produce (spans(Text("some "), SpanSequence(spanSeq), Text(" here")))
  }

  it should "parse an internal phrase link with text and url" in {
    val linkSrc = "`link<../foo/bar.rst#ref>`_"
    val spanSeq = List(LinkPathReference(Seq(Text("link")), relPath, linkSrc), LinkDefinition("link", InternalTarget(relPath)))
    Parsing (s"some $linkSrc here") should produce (spans(Text("some "), SpanSequence(spanSeq), Text(" here")))
  }
  
  it should "parse a phrase link with only a url" in {
    Parsing ("some `<http://foo.com>`_ here") should produce (spans(Text("some "), 
        SpanSequence(link(Text("http://foo.com")).url("http://foo.com").toLink, LinkDefinition("http://foo.com", ExternalTarget("http://foo.com"))), Text(" here")))
  }

  it should "parse an internal phrase link with only a url" in {
    val linkSrc = "`<../foo/bar.rst#ref>`_"
    val spanSeq = List(LinkPathReference(Seq(Text("../foo/bar.rst#ref")), relPath, linkSrc), LinkDefinition("../foo/bar.rst#ref", InternalTarget(relPath)))
    Parsing ("some `<../foo/bar.rst#ref>`_ here") should produce (spans(Text("some "), SpanSequence(spanSeq), Text(" here")))
  }
  
  it should "remove whitespace from an url" in {
    val input = """some `<http://
      | foo.com>`_ here""".stripMargin
    Parsing (input) should produce (spans(Text("some "), 
        SpanSequence(link(Text("http://foo.com")).url("http://foo.com").toLink, LinkDefinition("http://foo.com", ExternalTarget("http://foo.com"))), Text(" here")))
  }
  
  it should "parse an anonymous phrase link without url" in {
    Parsing ("some `link`__ here") should produce (spans(Text("some "), anonPLinkRef("link"), Text(" here")))
  }
  
  it should "parse an anonymous phrase link with text and url" in {
    Parsing ("some `link<http://foo.com>`__ here") should produce (spans(Text("some "), 
        link(Text("link")).url("http://foo.com"), Text(" here")))
  }
  
  it should "parse an anonymous phrase link with only an url" in {
    Parsing ("some `<http://foo.com>`__ here") should produce (spans(Text("some "), 
        link(Text("http://foo.com")).url("http://foo.com"), Text(" here")))
  }
  
  it should "parse a named link reference" in {
    Parsing ("some link_ here") should produce (spans(Text("some "), linkRef("link"), Text(" here")))
  }
  
  it should "parse an anonymous link reference" in {
    Parsing ("some link__ here") should produce (spans(Text("some "), anonLinkRef("link"), Text(" here")))
  }
  
  it should "normalize the id of a phrase link" in {
    val input = """some `strange
      | phrase   link`_ here""".stripMargin
    Parsing (input) should produce (spans(Text("some "), pLinkRef("strange phrase link","strange\n phrase   link"), Text(" here")))
  }
  
  
  "The standalone link parser" should "parse a http URI" in {
    val uri = "http://www.link.com"
    Parsing ("some http://www.link.com here") should produce (spans(Text("some "),
        link(Text(uri)).url(uri), Text(" here")))
  }

  it should "parse a http URI containing an IP4 address" in {
    val uri = "http://127.0.0.1/path"
    Parsing (s"some $uri here") should produce (spans(Text("some "),
      link(Text(uri)).url(uri), Text(" here")))
  }
  
  it should "parse a https URI" in {
    val uri = "https://www.link.com"
    Parsing ("some https://www.link.com here") should produce (spans(Text("some "),
        link(Text(uri)).url(uri), Text(" here")))
  }
  
  it should "parse an email address" in {
    val email = "user@domain.com"
    Parsing ("some user@domain.com here") should produce (spans(Text("some "),
        link(Text(email)).url("mailto:"+email), Text(" here")))
  }
  
  it should "parse a http URI without trailing punctuation" in {
    val uri = "http://www.link.com"
    Parsing ("some http://www.link.com. here") should produce (spans(Text("some "),
        link(Text(uri)).url(uri), Text(". here")))
  }

  it should "not parse a URI containing unicode characters" in {
    val text = "some http://www.link.com/fooá here"
    Parsing (text) should produce (spans(Text(text)))
  }
  
  it should "parse an email address without surrounding punctuation" in {
    val email = "user@domain.com"
    Parsing ("some {user@domain.com} here") should produce (spans(Text("some {"),
        link(Text(email)).url("mailto:"+email), Text("} here")))
  }
  
  
  
  "A backslash " should "cause a following character not to be treated as markup" in {
    Parsing ("""some \*text* here""") should produce (spans(Text("some *text* here")))
  }
  
  
}
