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

package laika.internal.rst

import laika.ast.*
import laika.ast.sample.TestSourceBuilders
import laika.internal.rst.ast.{ InterpretedText, RstStyle, SubstitutionReference }
import laika.internal.rst.ext.{ ExtensionProvider, RootParserProvider }
import laika.parse.Parser
import munit.FunSuite

class InlineParsersSpec extends FunSuite with TestSourceBuilders {

  val defaultTextRole = "foo"

  val defaultParser: Parser[List[Span]] =
    RootParserProvider.forBundle(
      ExtensionProvider.forDefaultTextRole(defaultTextRole)
    ).standaloneSpanParser

  def subst(name: String, input: String): SubstitutionReference =
    SubstitutionReference(name, source(s"|$name|", input))

  def pLinkRef(id: String, text: String, input: String): LinkIdReference =
    LinkIdReference(Seq(Text(text)), id, source(s"`$text`_", input))

  def pLinkRef(id: String, input: String): LinkIdReference = pLinkRef(id, id, input)

  def anonPLinkRef(text: String, input: String): LinkIdReference =
    LinkIdReference(Seq(Text(text)), "", source(s"`$text`__", input))

  def linkRef(id: String, input: String): LinkIdReference =
    LinkIdReference(Seq(Text(id)), id, source(id + "_", input))

  def anonLinkRef(text: String, input: String): LinkIdReference =
    LinkIdReference(Seq(Text(text)), "", source(text + "__", input))

  def run(input: String, spans: Span*)(implicit loc: munit.Location): Unit =
    assertEquals(defaultParser.parse(input).toEither, Right(spans.toList))

  def runEnclosed(input: String, middleSpan: Span)(implicit loc: munit.Location): Unit =
    run(input, Text("some "), middleSpan, Text(" here"))

  test("inline text - parse content without any markup as plain text") {
    run("some text", Text("some text"))
  }

  test("markup recognition rules - recognize markup surrounded by whitespace") {
    val input = "some |replaced| text"
    run(input, Text("some "), subst("replaced", input), Text(" text"))
  }

  test("markup recognition rules - recognize markup surrounded by punctuation") {
    val input = "some (|replaced|) text"
    run(input, Text("some ("), subst("replaced", input), Text(") text"))
  }

  test("markup recognition rules - recognize markup at the start of the input") {
    val input = "|replaced| text"
    run(input, subst("replaced", input), Text(" text"))
  }

  test("markup recognition rules - recognize markup at the end of the input") {
    val input = "text |replaced|"
    run(input, Text("text "), subst("replaced", input))
  }

  test("markup recognition rules - recognize markup at the end and the end of the input") {
    val input = "|replaced|"
    run(input, subst("replaced", input))
  }

  test("markup recognition rules - ignore markup surrounded by characters (rules 1 and 4)") {
    run("some|replaced|text", Text("some|replaced|text"))
  }

  test("markup recognition rules - ignore markup with a space after the start character (rule 2)") {
    run("some | replaced| text", Text("some | replaced| text"))
  }

  test("markup recognition rules - ignore markup with a space before the end character (rule 3)") {
    run("some |replaced | text", Text("some |replaced | text"))
  }

  test(
    "markup recognition rules - ignore markup end characters immediately following the start character (rule 6)"
  ) {
    run("some || text", Text("some || text"))
  }

  test(
    "markup recognition rules - ignore markup characters between matching punctuation characters (rule 5)"
  ) {
    run("some (|)replaced| text", Text("some (|)replaced| text"))
  }

  test(
    "markup recognition rules - recognize markup when preceding and following characters are escaped"
  ) {
    val input = """some\ |replaced|\ text"""
    run(input, Text("some"), subst("replaced", input), Text("text"))
  }

  test("em - content enclosed in *") {
    runEnclosed("some *text* here", Emphasized("text"))
  }

  test("em - ignore an * character when it is not matched by a second *") {
    run("some *text here", Text("some *text here"))
  }

  test("em - ignore markup for nested spans") {
    runEnclosed("some *nested ``code`` span* here", Emphasized("nested ``code`` span"))
  }

  test(
    "em - ignore markup for emphasis when strong failed at the same position based on the markup recognition rules"
  ) {
    run("a**b O(N**2)", Text("a**b O(N**2)"))
  }

  test("strong - content enclosed in **") {
    runEnclosed("some **text** here", Strong("text"))
  }

  test("strong - ignore a ** sequence when it is not matched by a second **") {
    run("some **text here", Text("some **text here"))
  }

  test("inline literal - content enclosed in ``") {
    runEnclosed("some ``text`` here", Literal("text"))
  }

  test("inline literal - ignore a ` character when it is not matched by a second `") {
    run("some ``text here", Text("some ``text here"))
  }

  test(
    "inline literal - do not treat a single ` as markup when the code span is enclosed in double ``"
  ) {
    runEnclosed("some ``text`text`` here", Literal("text`text"))
  }

  test("substitution reference - content enclosed in |") {
    val input = "some |replaced| here"
    runEnclosed(input, subst("replaced", input))
  }

  test("substitution reference - ignore a | character when it is not matched by a second |") {
    run("some |text here", Text("some |text here"))
  }

  test("internal link target - content enclosed in _` and `") {
    runEnclosed("some _`Text` here", Text("Text", Id("text") + RstStyle.target))
  }

  test("interpreted text - enclosed in ` with implicit default role") {
    val input = "some `text` here"
    runEnclosed(input, InterpretedText(defaultTextRole, "text", source("`text`", input)))
  }

  test("interpreted text - enclosed in ` with role prefix") {
    val input = "some :role:`text` here"
    runEnclosed(input, InterpretedText("role", "text", source(":role:`text`", input)))
  }

  test("interpreted text - enclosed in ` with role suffix") {
    val input = "some `text`:role: here"
    runEnclosed(input, InterpretedText("role", "text", source("`text`:role:", input)))
  }

  test("interpreted text - enclosed in ` but ignore illegal role prefix") {
    val input = "some :#*#:`text` here"
    run(
      input,
      Text("some :#*#:"),
      InterpretedText(defaultTextRole, "text", source("`text`", input)),
      Text(" here")
    )
  }

  test("citation reference - enclosed between [ and ]_") {
    val input = "some [text]_ here"
    runEnclosed(input, CitationReference("text", source("[text]_", input)))
  }

  test("footnote reference - enclosed between [ and ]_ with autonumber label") {
    val input = "some [#]_ here"
    runEnclosed(
      input,
      FootnoteReference(FootnoteLabel.Autonumber, source(toSource(FootnoteLabel.Autonumber), input))
    )
  }

  test("footnote reference - enclosed between [ and ]_ with autosymbol label") {
    val input = "some [*]_ here"
    runEnclosed(
      input,
      FootnoteReference(FootnoteLabel.Autosymbol, source(toSource(FootnoteLabel.Autosymbol), input))
    )
  }

  test("footnote reference - enclosed between [ and ]_ with an autonumber named label") {
    val input = "some [#foo]_ here"
    val label = FootnoteLabel.AutonumberLabel("foo")
    runEnclosed(input, FootnoteReference(label, source(toSource(label), input)))
  }

  test("footnote reference - enclosed between [ and ]_ with a numeric label") {
    val input = "some [17]_ here"
    val label = FootnoteLabel.NumericLabel(17)
    runEnclosed(input, FootnoteReference(label, source(toSource(label), input)))
  }

  private val relPath = RelativePath.parse("../foo/bar.rst#ref")

  test("phrase link without url") {
    val input = "some `link`_ here"
    runEnclosed(input, pLinkRef("link", input))
  }

  test("external phrase link with text and url") {
    val uri     = "http://foo.com"
    val spanSeq = List(SpanLink.external(uri)("link"), LinkDefinition("link", ExternalTarget(uri)))
    runEnclosed(s"some `link<$uri>`_ here", SpanSequence(spanSeq))
  }

  test("internal phrase link with text and url") {
    val linkSrc = "`link<../foo/bar.rst#ref>`_"
    val input   = s"some $linkSrc here"
    val spanSeq = List(
      LinkPathReference(Seq(Text("link")), relPath, source(linkSrc, input)),
      LinkDefinition("link", InternalTarget(relPath))
    )
    runEnclosed(input, SpanSequence(spanSeq))
  }

  test("phrase link with only a url") {
    val uri = "http://foo.com"
    runEnclosed(
      s"some `<$uri>`_ here",
      SpanSequence(
        SpanLink.external(uri)(uri),
        LinkDefinition(uri, ExternalTarget(uri))
      )
    )
  }

  test("internal phrase link with only a url") {
    val input   = "some `<../foo/bar.rst#ref>`_ here"
    val linkSrc = "`<../foo/bar.rst#ref>`_"
    val spanSeq = List(
      LinkPathReference(Seq(Text("../foo/bar.rst#ref")), relPath, source(linkSrc, input)),
      LinkDefinition("../foo/bar.rst#ref", InternalTarget(relPath))
    )
    runEnclosed(input, SpanSequence(spanSeq))
  }

  test("phrase link - remove whitespace from an url") {
    val uri   = "http://foo.com"
    val input = """some `<http://
                  | foo.com>`_ here""".stripMargin
    runEnclosed(
      input,
      SpanSequence(
        SpanLink.external(uri)(uri),
        LinkDefinition("http://foo.com", ExternalTarget("http://foo.com"))
      )
    )
  }

  test("anonymous phrase link without url") {
    val input = "some `link`__ here"
    runEnclosed(input, anonPLinkRef("link", input))
  }

  test("anonymous phrase link with text and url") {
    runEnclosed("some `link<http://foo.com>`__ here", SpanLink.external("http://foo.com")("link"))
  }

  test("anonymous phrase link with only an url") {
    val uri = "http://foo.com"
    runEnclosed("some `<http://foo.com>`__ here", SpanLink.external(uri)(uri))
  }

  test("named link reference") {
    val input = "some link_ here"
    runEnclosed(input, linkRef("link", input))
  }

  test("anonymous link reference") {
    val input = "some link__ here"
    runEnclosed(input, anonLinkRef("link", input))
  }

  test("normalize the id of a phrase link") {
    val input = """some `strange
                  | phrase   link`_ here""".stripMargin
    runEnclosed(input, pLinkRef("strange phrase link", "strange\n phrase   link", input))
  }

  test("standalone link - http URI") {
    val uri = "http://www.link.com"
    runEnclosed("some http://www.link.com here", SpanLink.external(uri)(uri))
  }

  test("standalone link - http URI containing an IP4 address") {
    val uri = "http://127.0.0.1/path"
    runEnclosed(s"some $uri here", SpanLink.external(uri)(uri))
  }

  test("standalone link - https URI") {
    val uri = "https://www.link.com"
    runEnclosed("some https://www.link.com here", SpanLink.external(uri)(uri))
  }

  test("standalone link - email address") {
    val email = "user@domain.com"
    runEnclosed("some user@domain.com here", SpanLink.external("mailto:" + email)(email))
  }

  test("standalone link - http URI without trailing punctuation") {
    val uri = "http://www.link.com"
    run(
      "some http://www.link.com. here",
      Text("some "),
      SpanLink.external(uri)(uri),
      Text(". here")
    )
  }

  test("standalone link - do not parse a URI containing unicode characters") {
    val text = "some http://www.link.com/fooá here"
    run(text, Text(text))
  }

  test("standalone link - email address without surrounding punctuation") {
    val email = "user@domain.com"
    run(
      "some {user@domain.com} here",
      Text("some {"),
      SpanLink.external("mailto:" + email)(email),
      Text("} here")
    )
  }

  test("backslash escape") {
    run("""some \*text* here""", Text("some *text* here"))
  }

}
