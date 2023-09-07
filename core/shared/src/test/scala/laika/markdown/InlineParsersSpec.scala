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

package laika.markdown

import laika.api.builder.OperationConfig
import laika.ast._
import laika.ast.sample.TestSourceBuilders
import laika.format.Markdown
import laika.parse.Parser
import laika.parse.markup.RootParserProvider.RootParserWrapper
import munit.FunSuite

class InlineParsersSpec extends FunSuite with TestSourceBuilders {

  val rootParser =
    new RootParserWrapper(Markdown, new OperationConfig(Markdown.extensions).markupExtensions)

  val defaultParser: Parser[List[Span]] = rootParser.standaloneSpanParser

  def run(input: String, spans: Span*)(implicit loc: munit.Location): Unit =
    assertEquals(defaultParser.parse(input).toEither, Right(spans.toList))

  def runEnclosed(input: String, middleSpan: Span)(implicit loc: munit.Location): Unit =
    run(input, Text("some "), middleSpan, Text(" here"))

  test("content without any markup") {
    run("some text", Text("some text"))
  }

  test("em - enclosed in * at the beginning of a phrase") {
    run("*some* text", Emphasized("some"), Text(" text"))
  }

  test("em - enclosed in * at the end of a phrase") {
    run("some *text*", Text("some "), Emphasized("text"))
  }

  test("em - enclosed in * in the middle of a phrase") {
    runEnclosed("some *text* here", Emphasized("text"))
  }

  test("em - enclosed in * when it spans the entire phrase") {
    run("*text*", Emphasized("text"))
  }

  test("em - ignore an * character when it is enclosed in spaces") {
    run("some * text * here", Text("some * text * here"))
  }

  test("em - ignore an * start markup when it is at the end of the line") {
    run("some *\ntext* here", Text("some *\ntext* here"))
  }

  test("em - ignore an * character when it is not matched by a second *") {
    run("some *text here", Text("some *text here"))
  }

  test("em - treat an '_' the same as an '*'") {
    runEnclosed("some _text_ here", Emphasized("text"))
  }

  test("strong - enclosed in ** at the beginning of a phrase") {
    run("**some** text", Strong("some"), Text(" text"))
  }

  test("strong - enclosed in ** at the end of a phrase") {
    run("some **text**", Text("some "), Strong("text"))
  }

  test("strong - enclosed in ** in the middle of a phrase") {
    runEnclosed("some **text** here", Strong("text"))
  }

  test("strong - enclosed in ** with a nested em span") {
    runEnclosed("some ***text*** here", Strong(Emphasized("text")))
  }

  test("strong - enclosed in ** when it spans the entire phrase") {
    run("**text**", Strong("text"))
  }

  test("strong - ignore a ** sequence when it is enclosed in spaces") {
    run("some ** text ** here", Text("some ** text ** here"))
  }

  test("strong - ignore a ** sequence when it is not matched by a second **") {
    run("some **text here", Text("some **text here"))
  }

  test("strong - treat an '_' the same as an '*'") {
    runEnclosed("some __text__ here", Strong("text"))
  }

  test("literal - enclosed in ` at the beginning of a phrase") {
    run("`some` text", Literal("some"), Text(" text"))
  }

  test("literal - enclosed in ` at the end of a phrase") {
    run("some `text`", Text("some "), Literal("text"))
  }

  test("literal - enclosed in ` in the middle of a phrase") {
    runEnclosed("some `text` here", Literal("text"))
  }

  test("literal - enclosed in ` when it spans the entire phrase") {
    run("`text`", Literal("text"))
  }

  test("literal - treat a ` character as markup even when it is enclosed in spaces") {
    runEnclosed("some ` text ` here", Literal("text"))
  }

  test("literal - ignore a ` character when it is not matched by a second `") {
    run("some `text here", Text("some `text here"))
  }

  test("literal - do not treat a single ` as markup when the code span is enclosed in double ``") {
    runEnclosed("some ``text`text`` here", Literal("text`text"))
  }

  test("literal - support three ` characters as delimiter") {
    runEnclosed("some ``` text`text ``` here", Literal("text`text"))
  }

  test("literal - support five ` characters as delimiter") {
    runEnclosed("some ````` text```text ````` here", Literal("text```text"))
  }

  test("nested spans - code span inside emphasized text") {
    runEnclosed(
      "some *nested `code` span* here",
      Emphasized(Text("nested "), Literal("code"), Text(" span"))
    )
  }

  test("nested spans - ignore the attempt to close an outer span inside an inner span") {
    run(
      "some *nested `code* span` here",
      Text("some *nested "),
      Literal("code* span"),
      Text(" here")
    )
  }

  test("escapes - escapable character not treated as markup") {
    run("""some \*text* here""", Text("some *text* here"))
  }

  test(
    "escapes - backslash treated as a literal character when the following character is not escapable"
  ) {
    run("""some \?text here""", Text("""some \?text here"""))
  }

  test("links - inline link without title") {
    runEnclosed("some [link](http://foo) here", SpanLink.external("http://foo")("link"))
  }

  test("links - inline link with nested parenthesis") {
    runEnclosed(
      "some [link](http://foo?param=foo(bar)baz) here",
      SpanLink.external("http://foo?param=foo(bar)baz")("link")
    )
  }

  test("links - recognize email link as external link") {
    runEnclosed(
      "some [link](mailto:nobody@nowhere.com) here",
      SpanLink.external("mailto:nobody@nowhere.com")("link")
    )
  }

  test("links - inline link with an optional title enclosed in double quotes") {
    runEnclosed(
      """some [link](http://foo "a title") here""",
      SpanLink.external("http://foo")("link").copy(title = Some("a title"))
    )
  }

  test("links - inline link destination within angle brackets") {
    runEnclosed(
      """some [link](<http://foo>) here""",
      SpanLink.external("http://foo")("link")
    )
  }

  test("links - inline link destination containing spaces within angle brackets") {
    val linkSrc = "[link](<foo bar.md>)"
    val input   = s"some $linkSrc here"
    val linkAST =
      LinkPathReference(Seq(Text("link")), RelativePath.parse("foo bar.md"), source(linkSrc, input))
    runEnclosed(input, linkAST)
  }

  test("links - inline link with an optional title enclosed in single quotes") {
    runEnclosed(
      """some [link](http://foo 'a title') here""",
      SpanLink.external("http://foo")("link").copy(title = Some("a title"))
    )
  }

  test("links - inline link with a malformed title") {
    val input = """some [link](http://foo 'a title) here"""
    val ref   = LinkIdReference("link", source("[link]", input))("link")
    run(input, Text("some "), ref, Text("(http://foo 'a title) here"))
  }

  test("links - markup inside the text of an inline link") {
    runEnclosed(
      "some [link *text*](http://foo) here",
      SpanLink.external("http://foo")(Text("link "), Emphasized("text"))
    )
  }

  test("links - properly parse escape sequences in the text of an inline link") {
    runEnclosed(
      "some [link \\_text\\_](http://foo) here",
      SpanLink.external("http://foo")("link _text_")
    )
  }

  test("images - inline image without title") {
    runEnclosed(
      "some ![link](http://foo.jpg) here",
      Image(ExternalTarget("http://foo.jpg"), alt = Some("link"))
    )
  }

  test("images - inline image with internal target") {
    val input = "some ![link](images/foo.jpg) here"
    runEnclosed(
      input,
      ImagePathReference(
        RelativePath.parse("images/foo.jpg"),
        source("![link](images/foo.jpg)", input),
        alt = Some("link")
      )
    )
  }

  test("images - inline image with an optional title enclosed in double quotes") {
    runEnclosed(
      """some ![link](http://foo.jpg "a title") here""",
      Image(ExternalTarget("http://foo.jpg"), alt = Some("link"), title = Some("a title"))
    )
  }

  test("images - inline image with an optional title enclosed in single quotes") {
    runEnclosed(
      """some ![link](http://foo.jpg 'a title') here""",
      Image(ExternalTarget("http://foo.jpg"), alt = Some("link"), title = Some("a title"))
    )
  }

  test("images - ignore an inline image with a malformed title") {
    val input = """some ![link](http://foo.jpg 'a title) here"""
    run(
      input,
      Text("some "),
      ImageIdReference("link", "link", source("![link]", input)),
      Text("(http://foo.jpg 'a title) here")
    )
  }

  test("images - do not parse markup inside the text of an inline link") {
    runEnclosed(
      "some ![link *text*](http://foo.jpg) here",
      Image(ExternalTarget("http://foo.jpg"), alt = Some("link *text*"))
    )
  }

  test("link reference with an explicit id") {
    val input = "some [link][id] here"
    val ref   = LinkIdReference("id", source("[link][id]", input))("link")
    runEnclosed(input, ref)
  }

  test("link reference with an empty id") {
    val input = "some [link][] here"
    val ref   = LinkIdReference("link", source("[link][]", input))("link")
    runEnclosed(input, ref)
  }

  test("link reference with an explicit id separated by a space") {
    val input = "some [link] [id] here"
    val ref   = LinkIdReference("id", source("[link] [id]", input))("link")
    runEnclosed(input, ref)
  }

  test("link reference with an empty id separated by a space") {
    val input = "some [link] [] here"
    val ref   = LinkIdReference("link", source("[link] []", input))("link")
    runEnclosed(input, ref)
  }

  test("link reference with an implicit id") {
    val input = "some [link] here"
    val ref   = LinkIdReference("link", source("[link]", input))("link")
    runEnclosed(input, ref)
  }

  test("image reference with an explicit id") {
    val input = "some ![image][id] here"
    runEnclosed(input, ImageIdReference("image", "id", source("![image][id]", input)))
  }

  test("image reference with an empty id") {
    val input = "some ![image][] here"
    runEnclosed(input, ImageIdReference("image", "image", source("![image][]", input)))
  }

  test("image reference with an implicit id") {
    val input = "some ![image] here"
    runEnclosed(input, ImageIdReference("image", "image", source("![image]", input)))
  }

  test("link enclosed in angle brackets and set the url as the link text") {
    runEnclosed("some <http://foo> here", SpanLink.external("http://foo")("http://foo"))
  }

}
