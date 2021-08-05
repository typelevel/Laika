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

import laika.ast._
import laika.ast.sample.TestSourceBuilders
import laika.parse.Parser
import laika.parse.helper.MigrationFlatSpec
import laika.rst.ast.{InterpretedText, RstStyle, SubstitutionReference}
import laika.rst.ext.{ExtensionProvider, RootParserProvider}
import org.scalatest.Assertion
     
class InlineParsersSpec extends MigrationFlatSpec
  with TestSourceBuilders {


  val defaultTextRole = "foo"

  val defaultParser: Parser[List[Span]] = 
    RootParserProvider.forBundle(ExtensionProvider.forDefaultTextRole(defaultTextRole)).standaloneSpanParser


  trait RefBuilder {
    
    def input: String
    
    def subst (name: String) = SubstitutionReference(name, source(s"|$name|", input))
  
    def pLinkRef (id: String, text: String): LinkIdReference = LinkIdReference(Seq(Text(text)), id, source(s"`$text`_", input))
  
    def pLinkRef (id: String): LinkIdReference = pLinkRef(id,id)
    
    def anonPLinkRef (text: String): LinkIdReference = LinkIdReference(Seq(Text(text)), "", source(s"`$text`__", input))
    
    def linkRef (id: String): LinkIdReference = LinkIdReference(Seq(Text(id)), id, source(id+"_", input))
    
    def anonLinkRef (text: String): LinkIdReference = LinkIdReference(Seq(Text(text)), "", source(text+"__", input))
  }


  def run (input: String, spans: Span*): Assertion =
    assertEquals(defaultParser.parse(input).toEither, Right(spans.toList))

  def runEnclosed (input: String, middleSpan: Span): Assertion =
    run(input, Text("some "), middleSpan, Text(" here"))
  
  
  "The text parser" should "parse content without any markup as plain text" in {
    run("some text", Text("some text"))
  }
  
  
  "The markup recognition rules" should "recognize markup surrounded by whitespace" in new RefBuilder {
    val input = "some |replaced| text"
    run(input, Text("some "), subst("replaced"), Text(" text"))
  }

  it should "recognize markup surrounded by punctuation" in new RefBuilder {
    val input = "some (|replaced|) text"
    run(input, Text("some ("), subst("replaced"), Text(") text"))
  }
  
  it should "recognize markup at the start of the input" in new RefBuilder {
    val input = "|replaced| text"
    run(input, subst("replaced"), Text(" text"))
  }
  
  it should "recognize markup at the end of the input" in new RefBuilder {
    val input = "text |replaced|"
    run(input, Text("text "), subst("replaced"))
  }
  
  it should "recognize markup at the end and the end of the input" in new RefBuilder {
    val input = "|replaced|"
    run(input, subst("replaced"))
  }
  
  it should "ignore markup surrounded by characters (rules 1 and 4)" in {
    run("some|replaced|text", Text("some|replaced|text"))
  }
  
  it should "ignore markup with a space after the start character (rule 2)" in {
    run("some | replaced| text", Text("some | replaced| text"))
  }
  
  it should "ignore markup with a space before the end character (rule 3)" in {
    run("some |replaced | text", Text("some |replaced | text"))
  }
  
  it should "ignore markup end characters immediately following the start character (rule 6)" in {
    run("some || text", Text("some || text"))
  }
  
  it should "ignore markup characters between matching punctuation characters (rule 5)" in {
    run("some (|)replaced| text", Text("some (|)replaced| text"))
  }
  
  it should "recognize markup when preceding and following characters are escaped" in new RefBuilder {
    val input = """some\ |replaced|\ text"""
    run(input, Text("some"), subst("replaced"), Text("text"))
  }
  
  
  
  "The em parser" should "parse content enclosed in *" in {
    runEnclosed("some *text* here", Emphasized("text"))
  }
  
  it should "ignore an * character when it is not matched by a second *" in {
    run("some *text here", Text("some *text here"))
  }
  
  it should "ignore markup for nested spans" in {
    runEnclosed("some *nested ``code`` span* here", Emphasized("nested ``code`` span"))
  }
  
  it should "ignore markup for emphasis when strong failed at the same position based on the markup recognition rules" in {
    run("a**b O(N**2)", Text("a**b O(N**2)"))
  }
    
  
  
  "The strong parser" should "parse content enclosed in **" in {
    runEnclosed("some **text** here", Strong("text"))
  }
  
  it should "ignore a ** sequence when it is not matched by a second **" in {
    run("some **text here", Text("some **text here"))
  }
  
  
  
  "The inline literal parser" should "parse content enclosed in ``" in {
    runEnclosed("some ``text`` here", Literal("text"))
  }
  
  it should "ignore a ` character when it is not matched by a second `" in {
    run("some ``text here", Text("some ``text here"))
  }
  
  it should "not treat a single ` as markup when the code span is enclosed in double ``" in {
    runEnclosed("some ``text`text`` here", Literal("text`text"))
  }
  
  
  
  "The substitution reference" should "parse content enclosed in |" in new RefBuilder {
    val input = "some |replaced| here"
    runEnclosed(input,  subst("replaced"))
  }
  
  it should "ignore a | character when it is not matched by a second |" in {
    run("some |text here", Text("some |text here"))
  }
  
  
  
  "The internal link target parser" should "parse content enclosed in _` and `" in {
    runEnclosed("some _`Text` here",  Text("Text", Id("text") + RstStyle.target))
  }
  
  
  
  "The interpreted text parser" should "parse content enclosed in ` with implicit default role" in new RefBuilder {
    val input = "some `text` here"
    runEnclosed(input, InterpretedText(defaultTextRole, "text", source("`text`", input)))
  }
  
  it should "parse content enclosed in ` with role prefix" in new RefBuilder {
    val input = "some :role:`text` here"
    runEnclosed(input, InterpretedText("role", "text", source(":role:`text`", input)))
  }
  
  it should "parse content enclosed in ` with role suffix" in new RefBuilder {
    val input = "some `text`:role: here"
    runEnclosed(input, InterpretedText("role", "text", source("`text`:role:", input)))
  }
  
  it should "parse content enclosed in ` but ignore illegal role prefix" in new RefBuilder {
    val input = "some :#*#:`text` here"
    run(input, 
      Text("some :#*#:"),
      InterpretedText(defaultTextRole, "text", source("`text`", input)),
      Text(" here")
    )
  }
  
  
  
  "The citation reference parser" should "parse content enclosed between [ and ]_" in {
    val input = "some [text]_ here"
    runEnclosed(input, CitationReference("text", source("[text]_", input)))
  }
  
  
  
  "The footnote reference parser" should "parse content enclosed between [ and ]_ with autonumber label" in {
    val input = "some [#]_ here"
    runEnclosed(input, FootnoteReference(Autonumber, source(toSource(Autonumber), input)))
  }
  
  it should "parse content enclosed between [ and ]_ with autosymbol label" in {
    val input = "some [*]_ here"
    runEnclosed(input, FootnoteReference(Autosymbol, source(toSource(Autosymbol), input)))
  }
  
  it should "parse content enclosed between [ and ]_ with an autonumber named label" in {
    val input = "some [#foo]_ here"
    val label = AutonumberLabel("foo")
    runEnclosed(input, FootnoteReference(label, source(toSource(label), input)))
  }
  
  it should "parse content enclosed between [ and ]_ with a numeric label" in {
    val input = "some [17]_ here"
    val label = NumericLabel(17)
    runEnclosed(input, FootnoteReference(label, source(toSource(label), input)))
  }
  
  
  private val relPath = RelativePath.parse("../foo/bar.rst#ref")
  
  "The link reference parser" should "parse a phrase link without url" in new RefBuilder {
    val input = "some `link`_ here"
    runEnclosed(input, pLinkRef("link"))
  }
  
  it should "parse an external phrase link with text and url" in {
    val uri = "http://foo.com"
    val spanSeq = List(SpanLink.external(uri)("link"), LinkDefinition("link", ExternalTarget(uri)))
    runEnclosed(s"some `link<$uri>`_ here", SpanSequence(spanSeq))
  }

  it should "parse an internal phrase link with text and url" in new RefBuilder {
    val linkSrc = "`link<../foo/bar.rst#ref>`_"
    val input = s"some $linkSrc here"
    val spanSeq = List(
      LinkPathReference(Seq(Text("link")), relPath, source(linkSrc, input)), 
      LinkDefinition("link", InternalTarget(relPath))
    )
    runEnclosed(input, SpanSequence(spanSeq))
  }
  
  it should "parse a phrase link with only a url" in {
    val uri = "http://foo.com"
    runEnclosed(s"some `<$uri>`_ here",
      SpanSequence(
        SpanLink.external(uri)(uri), 
        LinkDefinition(uri, ExternalTarget(uri))
      )
    )
  }

  it should "parse an internal phrase link with only a url" in new RefBuilder {
    val input = "some `<../foo/bar.rst#ref>`_ here"
    val linkSrc = "`<../foo/bar.rst#ref>`_"
    val spanSeq = List(
      LinkPathReference(Seq(Text("../foo/bar.rst#ref")), relPath, source(linkSrc, input)), 
      LinkDefinition("../foo/bar.rst#ref", InternalTarget(relPath))
    )
    runEnclosed(input, SpanSequence(spanSeq))
  }
  
  it should "remove whitespace from an url" in {
    val uri = "http://foo.com"
    val input = """some `<http://
      | foo.com>`_ here""".stripMargin
    runEnclosed(input,
      SpanSequence(SpanLink.external(uri)(uri), LinkDefinition("http://foo.com", ExternalTarget("http://foo.com"))))
  }
  
  it should "parse an anonymous phrase link without url" in new RefBuilder {
    val input = "some `link`__ here"
    runEnclosed(input, anonPLinkRef("link"))
  }
  
  it should "parse an anonymous phrase link with text and url" in {
    runEnclosed("some `link<http://foo.com>`__ here", SpanLink.external("http://foo.com")("link"))
  }
  
  it should "parse an anonymous phrase link with only an url" in {
    val uri = "http://foo.com"
    runEnclosed("some `<http://foo.com>`__ here", SpanLink.external(uri)(uri))
  }
  
  it should "parse a named link reference" in new RefBuilder {
    val input = "some link_ here"
    runEnclosed(input, linkRef("link"))
  }
  
  it should "parse an anonymous link reference" in new RefBuilder {
    val input = "some link__ here"
    runEnclosed(input, anonLinkRef("link"))
  }
  
  it should "normalize the id of a phrase link" in new RefBuilder {
    val input = """some `strange
      | phrase   link`_ here""".stripMargin
    runEnclosed(input, pLinkRef("strange phrase link","strange\n phrase   link"))
  }
  
  
  "The standalone link parser" should "parse a http URI" in {
    val uri = "http://www.link.com"
    runEnclosed("some http://www.link.com here", SpanLink.external(uri)(uri))
  }

  it should "parse a http URI containing an IP4 address" in {
    val uri = "http://127.0.0.1/path"
    runEnclosed(s"some $uri here", SpanLink.external(uri)(uri))
  }
  
  it should "parse a https URI" in {
    val uri = "https://www.link.com"
    runEnclosed("some https://www.link.com here", SpanLink.external(uri)(uri))
  }
  
  it should "parse an email address" in {
    val email = "user@domain.com"
    runEnclosed("some user@domain.com here", SpanLink.external("mailto:"+email)(email))
  }
  
  it should "parse a http URI without trailing punctuation" in {
    val uri = "http://www.link.com"
    run("some http://www.link.com. here",
      Text("some "),
      SpanLink.external(uri)(uri),
      Text(". here")
    )
  }

  it should "not parse a URI containing unicode characters" in {
    val text = "some http://www.link.com/fooá here"
    run(text, Text(text))
  }
  
  it should "parse an email address without surrounding punctuation" in {
    val email = "user@domain.com"
    run("some {user@domain.com} here", 
      Text("some {"),
      SpanLink.external("mailto:"+email)(email),
      Text("} here")
    )
  }
  
  
  
  "A backslash " should "cause a following character not to be treated as markup" in {
    run("""some \*text* here""", Text("some *text* here"))
  }
  
  
}
