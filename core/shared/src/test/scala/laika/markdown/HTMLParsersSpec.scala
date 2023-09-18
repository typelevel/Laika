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
import laika.ast.html.HTMLAttribute
import laika.ast.{ Emphasized, Span, Text }
import laika.format.Markdown
import laika.parse.Parser
import laika.parse.markup.RootParserProvider.RootParserWrapper
import munit.FunSuite

class HTMLParsersSpec extends FunSuite with HTMLModelBuilder {

  val rootParser = new RootParserWrapper(
    Markdown,
    new OperationConfig(Markdown.extensions).forRawContent.markupExtensions
  )

  val defaultParser: Parser[List[Span]] = rootParser.standaloneSpanParser

  def run(input: String, spans: Span*)(implicit loc: munit.Location): Unit =
    assertEquals(defaultParser.parse(input).toEither, Right(spans.toList))

  def runEnclosed(input: String, middleSpan: Span)(implicit loc: munit.Location): Unit =
    run(input, Text("some "), middleSpan, Text(" here"))

  test("single start tag without attributes in flow content") {
    val input = """some <tag> here"""
    runEnclosed(input, startTag("tag"))
  }

  test("single empty tag without attributes in flow content") {
    val input = """some <tag/> here"""
    runEnclosed(input, emptyTag("tag"))
  }

  test("single empty tag with whitespace without attributes in flow content") {
    val input = """some <tag /> here"""
    runEnclosed(input, emptyTag("tag"))
  }

  test("single end tag in flow content") {
    val input = """some </tag> here"""
    runEnclosed(input, endTag("tag"))
  }

  test("single element in flow content") {
    val input = """some <tag>element</tag> here"""
    runEnclosed(input, element("tag", Text("element")))
  }

  test("two nested elements in flow content") {
    val input = """aaa<outer>bbb<inner>ccc</inner>ddd</outer>eee"""
    val inner = element("inner", Text("ccc"))
    val outer = element("outer", Text("bbb"), inner, Text("ddd"))
    run(input, Text("aaa"), outer, Text("eee"))
  }

  test("Markdown markup inside HTML elements") {
    val input = """aaa <outer>bbb *ccc* ddd</outer> eee"""
    val inner = Emphasized("ccc")
    val outer = element("outer", Text("bbb "), inner, Text(" ddd"))
    run(input, Text("aaa "), outer, Text(" eee"))
  }

  test("start tag with an attribute value in double quotes") {
    val input = """some <tag attr="value"> here"""
    runEnclosed(input, startTag("tag", "attr" -> Text("value")))
  }

  test("start tag with an attribute value in single quotes") {
    val input = """some <tag attr='value'> here"""
    runEnclosed(input, startTag("tag", HTMLAttribute("attr", List(Text("value")), Some('\''))))
  }

  test("start tag with an unquoted attribute value") {
    val input = """some <tag attr=value> here"""
    runEnclosed(input, startTag("tag", HTMLAttribute("attr", List(Text("value")), None)))
  }

  test("start tag with an attribute without value") {
    val input = """some <tag attr> here"""
    runEnclosed(input, startTag("tag", HTMLAttribute("attr", Nil, None)))
  }

  test("ignore a start tag with a malformed attribute") {
    val input = """some <tag attr="foo> here"""
    run(input, Text("""some <tag attr="foo> here"""))
  }

  test("start tag with two attribute values") {
    val input = """some <tag attr1="value1" attr2="value2"> here"""
    runEnclosed(input, startTag("tag", "attr1" -> Text("value1"), "attr2" -> Text("value2")))
  }

  test("named reference") {
    run("some &amp; ref", Text("some "), charRef("&amp;"), Text(" ref"))
  }

  test("decimal reference") {
    run("some &#201; ref", Text("some "), charRef("&#201;"), Text(" ref"))
  }

  test("hex reference") {
    run("some &#x2e; ref", Text("some "), charRef("&#x2e;"), Text(" ref"))
  }

  test("ignore a malformed named reference") {
    run("some &amp ref", Text("some &amp ref"))
  }

  test("ignore a malformed decimal reference") {
    run("some &#2e; ref", Text("some &#2e; ref"))
  }

  test("ignore a malformed hex reference") {
    run("some &#x2g; ref", Text("some &#x2g; ref"))
  }

  test("standard HTML comment") {
    runEnclosed("some <!-- comment --> here", comment(" comment "))
  }

  test("ignore other HTML tags inside the HTML comment") {
    runEnclosed("some <!-- <ignored>tags</ignored> --> here", comment(" <ignored>tags</ignored> "))
  }

  test("ignore Markdown markup inside the HTML comment") {
    runEnclosed("some <!-- *comment* --> here", comment(" *comment* "))
  }

}
