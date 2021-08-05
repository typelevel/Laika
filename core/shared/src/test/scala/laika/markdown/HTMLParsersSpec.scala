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
import laika.ast.{Emphasized, Span, Text}
import laika.format.Markdown
import laika.markdown.ast.HTMLAttribute
import laika.parse.Parser
import laika.parse.helper.MigrationFlatSpec
import laika.parse.markup.RootParserProvider.RootParserWrapper
import org.scalatest.Assertion

class HTMLParsersSpec extends MigrationFlatSpec with HTMLModelBuilder {


  val rootParser = new RootParserWrapper(Markdown, OperationConfig(Markdown.extensions).forRawContent.markupExtensions)

  val defaultParser: Parser[List[Span]] = rootParser.standaloneSpanParser

  def run (input: String, spans: Span*): Assertion =
    assertEquals(defaultParser.parse(input).toEither, Right(spans.toList))

  def runEnclosed(input: String, middleSpan: Span): Assertion =
    run(input, Text("some "), middleSpan, Text(" here"))
  
  
  "The HTML tag parser" should "parse a single start tag without attributes in flow content" in {
    val input = """some <tag> here"""
    runEnclosed(input, startTag("tag"))
  }
  
  it should "parse a single empty tag without attributes in flow content" in {
    val input = """some <tag/> here"""
    runEnclosed(input, emptyTag("tag"))
  }
  
  it should "parse a single empty tag with whitespace without attributes in flow content" in {
    val input = """some <tag /> here"""
    runEnclosed(input, emptyTag("tag"))
  }

  it should "parse a single end tag in flow content" in {
    val input = """some </tag> here"""
    runEnclosed(input, endTag("tag"))
  }
  
  it should "parse a single element in flow content" in {
    val input = """some <tag>element</tag> here"""
    runEnclosed(input, element("tag", Text("element")))
  }
  
  it should "parse two nested elements in flow content" in {
    val input = """aaa<outer>bbb<inner>ccc</inner>ddd</outer>eee"""
    val inner = element("inner", Text("ccc"))
    val outer = element("outer", Text("bbb"), inner, Text("ddd"))
    run(input, Text("aaa"), outer, Text("eee"))
  }
  
  it should "parse Markdown markup inside HTML elements" in {
    val input = """aaa <outer>bbb *ccc* ddd</outer> eee"""
    val inner = Emphasized("ccc")
    val outer = element("outer", Text("bbb "), inner, Text(" ddd"))
    run(input, Text("aaa "), outer, Text(" eee"))
  }
  
  it should "parse a start tag with an attribute value in double quotes" in {
    val input = """some <tag attr="value"> here"""
    runEnclosed(input, startTag("tag", "attr" -> Text("value")))
  }
  
  it should "parse a start tag with an attribute value in single quotes" in {
    val input = """some <tag attr='value'> here"""
    runEnclosed(input, startTag("tag", HTMLAttribute("attr", List(Text("value")), Some('\''))))
  }
  
  it should "parse a start tag with an unquoted attribute value" in {
    val input = """some <tag attr=value> here"""
    runEnclosed(input, startTag("tag", HTMLAttribute("attr", List(Text("value")), None)))
  }
  
  it should "parse a start tag with an attribute without value" in {
    val input = """some <tag attr> here"""
    runEnclosed(input, startTag("tag", HTMLAttribute("attr", Nil, None)))
  }
  
  it should "ignore a start tag with a malformed attribute" in {
    val input = """some <tag attr="foo> here"""
    run(input, Text("""some <tag attr="foo> here"""))
  }
  
  it should "parse a start tag with two attribute values" in {
    val input = """some <tag attr1="value1" attr2="value2"> here"""
    runEnclosed(input, startTag("tag", "attr1" -> Text("value1"), "attr2" -> Text("value2")))
  }
  
  
  
  "The HTML character reference parser" should "parse a named reference" in {
    run("some &amp; ref", Text("some "), charRef("&amp;"), Text(" ref"))
  }
  
  it should "parse a decimal reference" in {
    run("some &#201; ref", Text("some "), charRef("&#201;"), Text(" ref"))
  }
  
  it should "parse a hex reference" in {
    run("some &#x2e; ref", Text("some "), charRef("&#x2e;"), Text(" ref"))
  }
  
  it should "ignore a malformed named reference" in {
    run("some &amp ref", Text("some &amp ref"))
  }
  
  it should "ignore a malformed decimal reference" in {
    run("some &#2e; ref", Text("some &#2e; ref"))
  }
  
  it should "ignore a malformed hex reference" in {
    run("some &#x2g; ref", Text("some &#x2g; ref"))
  }
  
  
  
  "The HTML comment parser" should "parse a standard HTML comment" in {
    runEnclosed("some <!-- comment --> here", comment(" comment "))
  }
  
  it should "ignore other HTML tags inside the HTML comment" in {
    runEnclosed("some <!-- <ignored>tags</ignored> --> here", comment(" <ignored>tags</ignored> "))
  }
  
  it should "ignore Markdown markup inside the HTML comment" in {
    runEnclosed("some <!-- *comment* --> here", comment(" *comment* "))
  }
  
  
}
