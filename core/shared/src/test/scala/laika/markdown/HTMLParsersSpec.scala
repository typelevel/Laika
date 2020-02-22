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
import laika.ast.helper.ModelBuilder
import laika.format.Markdown
import laika.markdown.ast.HTMLAttribute
import laika.parse.Parser
import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import laika.parse.markup.RootParser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HTMLParsersSpec extends AnyFlatSpec 
                      with Matchers 
                      with ParseResultHelpers
                      with DefaultParserHelpers[List[Span]] 
                      with ModelBuilder 
                      with HTMLModelBuilder {


  val rootParser = new RootParser(Markdown, OperationConfig(Markdown.extensions).forRawContent.markupExtensions)

  val defaultParser: Parser[List[Span]] = rootParser.recursiveSpans
  
  
  "The HTML tag parser" should "parse a single start tag without attributes in flow content" in {
    val input = """some <tag> here"""
    Parsing (input) should produce (spans(Text("some "), startTag("tag"), Text(" here")))
  }
  
  it should "parse a single empty tag without attributes in flow content" in {
    val input = """some <tag/> here"""
    Parsing (input) should produce (spans(Text("some "), emptyTag("tag"), Text(" here")))
  }
  
  it should "parse a single empty tag with whitespace without attributes in flow content" in {
    val input = """some <tag /> here"""
    Parsing (input) should produce (spans(Text("some "), emptyTag("tag"), Text(" here")))
  }

  it should "parse a single end tag in flow content" in {
    val input = """some </tag> here"""
    Parsing (input) should produce (spans(Text("some "), endTag("tag"), Text(" here")))
  }
  
  it should "parse a single element in flow content" in {
    val input = """some <tag>element</tag> here"""
    Parsing (input) should produce (spans(Text("some "), element("tag", Text("element")), Text(" here")): List[Span])
  }
  
  it should "parse two nested elements in flow content" in {
    val input = """aaa<outer>bbb<inner>ccc</inner>ddd</outer>eee"""
    val inner = element("inner", Text("ccc"))
    val outer = element("outer", Text("bbb"), inner, Text("ddd"))
    Parsing (input) should produce (spans(Text("aaa"), outer, Text("eee")))
  }
  
  it should "parse Markdown markup inside HTML elements" in {
    val input = """aaa <outer>bbb *ccc* ddd</outer> eee"""
    val inner = Emphasized("ccc")
    val outer = element("outer", Text("bbb "), inner, Text(" ddd"))
    Parsing (input) should produce (spans(Text("aaa "), outer, Text(" eee")))
  }
  
  it should "parse a start tag with an attribute value in double quotes" in {
    val input = """some <tag attr="value"> here"""
    Parsing (input) should produce (spans(Text("some "), startTag("tag", "attr" -> Text("value")), Text(" here")))
  }
  
  it should "parse a start tag with an attribute value in single quotes" in {
    val input = """some <tag attr='value'> here"""
    Parsing (input) should produce (spans(Text("some "), startTag("tag", HTMLAttribute("attr", List(Text("value")), Some('\''))), Text(" here")))
  }
  
  it should "parse a start tag with an unquoted attribute value" in {
    val input = """some <tag attr=value> here"""
    Parsing (input) should produce (spans(Text("some "), startTag("tag", HTMLAttribute("attr", List(Text("value")), None)), Text(" here")))
  }
  
  it should "parse a start tag with an attribute without value" in {
    val input = """some <tag attr> here"""
    Parsing (input) should produce (spans(Text("some "), startTag("tag", HTMLAttribute("attr", Nil, None)), Text(" here")))
  }
  
  it should "ignore a start tag with a malformed attribute" in {
    val input = """some <tag attr="foo> here"""
    Parsing (input) should produce (spans(Text("""some <tag attr="foo> here""")))
  }
  
  it should "parse a start tag with two attribute values" in {
    val input = """some <tag attr1="value1" attr2="value2"> here"""
    Parsing (input) should produce (spans(Text("some "), startTag("tag", "attr1" -> Text("value1"), "attr2" -> Text("value2")), Text(" here")))
  }
  
  
  
  "The HTML character reference parser" should "parse a named reference" in {
    Parsing ("some &amp; ref") should produce (spans(Text("some "), charRef("&amp;"), Text(" ref")))
  }
  
  it should "parse a decimal reference" in {
    Parsing ("some &#201; ref") should produce (spans(Text("some "), charRef("&#201;"), Text(" ref")))
  }
  
  it should "parse a hex reference" in {
    Parsing ("some &#x2e; ref") should produce (spans(Text("some "), charRef("&#x2e;"), Text(" ref")))
  }
  
  it should "ignore a malformed named reference" in {
    Parsing ("some &amp ref") should produce (spans(Text("some &amp ref")))
  }
  
  it should "ignore a malformed decimal reference" in {
    Parsing ("some &#2e; ref") should produce (spans(Text("some &#2e; ref")))
  }
  
  it should "ignore a malformed hex reference" in {
    Parsing ("some &#x2g; ref") should produce (spans(Text("some &#x2g; ref")))
  }
  
  
  
  "The HTML comment parser" should "parse a standard HTML comment" in {
    Parsing ("some <!-- comment --> here") should produce (spans(Text("some "), comment(" comment "), Text(" here")))
  }
  
  it should "ignore other HTML tags inside the HTML comment" in {
    Parsing ("some <!-- <ignored>tags</ignored> --> here") should produce (spans(Text("some "), comment(" <ignored>tags</ignored> "), Text(" here")))
  }
  
  it should "ignore Markdown markup inside the HTML comment" in {
    Parsing ("some <!-- *comment* --> here") should produce (spans(Text("some "), comment(" *comment* "), Text(" here")))
  }
  
  
}
