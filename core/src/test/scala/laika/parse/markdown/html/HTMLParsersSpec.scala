/*
 * Copyright 2013-2016 the original author or authors.
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

package laika.parse.markdown.html

import laika.api.ext.ParserDefinitionBuilders
import laika.parse.core.Parser
import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import laika.parse.markdown.RootParser
import laika.parse.markdown.html.HTMLElements.HTMLAttribute
import laika.tree.Elements.Span
import laika.tree.helper.ModelBuilder
import org.scalatest.{FlatSpec, Matchers}

class HTMLParsersSpec extends FlatSpec 
                      with Matchers 
                      with ParseResultHelpers
                      with DefaultParserHelpers[List[Span]] 
                      with ModelBuilder 
                      with HTMLModelBuilder {


  val rootParser = new RootParser(Map(), Map(), VerbatimHTML.parserDefinitions, isStrict = false)

  val defaultParser: Parser[List[Span]] = rootParser.recursiveSpans
  
  
  "The HTML tag parser" should "parse a single start tag without attributes in flow content" in {
    val input = """some <tag> here"""
    Parsing (input) should produce (spans(txt("some "), startTag("tag"), txt(" here")))
  }
  
  it should "parse a single empty tag without attributes in flow content" in {
    val input = """some <tag/> here"""
    Parsing (input) should produce (spans(txt("some "), emptyTag("tag"), txt(" here")))
  }
  
  it should "parse a single empty tag with whitespace without attributes in flow content" in {
    val input = """some <tag /> here"""
    Parsing (input) should produce (spans(txt("some "), emptyTag("tag"), txt(" here")))
  }

  it should "parse a single end tag in flow content" in {
    val input = """some </tag> here"""
    Parsing (input) should produce (spans(txt("some "), endTag("tag"), txt(" here")))
  }
  
  it should "parse a single element in flow content" in {
    val input = """some <tag>element</tag> here"""
    Parsing (input) should produce (spans(txt("some "), element("tag", txt("element")), txt(" here")))
  }
  
  it should "parse two nested elements in flow content" in {
    val input = """aaa<outer>bbb<inner>ccc</inner>ddd</outer>eee"""
    val inner = element("inner", txt("ccc"))
    val outer = element("outer", txt("bbb"), inner, txt("ddd"))
    Parsing (input) should produce (spans(txt("aaa"), outer, txt("eee")))
  }
  
  it should "parse Markdown markup inside HTML elements" in {
    val input = """aaa <outer>bbb *ccc* ddd</outer> eee"""
    val inner = em("ccc")
    val outer = element("outer", txt("bbb "), inner, txt(" ddd"))
    Parsing (input) should produce (spans(txt("aaa "), outer, txt(" eee")))
  }
  
  it should "parse a start tag with an attribute value in double quotes" in {
    val input = """some <tag attr="value"> here"""
    Parsing (input) should produce (spans(txt("some "), startTag("tag", "attr" -> txt("value")), txt(" here")))
  }
  
  it should "parse a start tag with an attribute value in single quotes" in {
    val input = """some <tag attr='value'> here"""
    Parsing (input) should produce (spans(txt("some "), startTag("tag", HTMLAttribute("attr", List(txt("value")), Some('\''))), txt(" here")))
  }
  
  it should "parse a start tag with an unquoted attribute value" in {
    val input = """some <tag attr=value> here"""
    Parsing (input) should produce (spans(txt("some "), startTag("tag", HTMLAttribute("attr", List(txt("value")), None)), txt(" here")))
  }
  
  it should "parse a start tag with an attribute without value" in {
    val input = """some <tag attr> here"""
    Parsing (input) should produce (spans(txt("some "), startTag("tag", HTMLAttribute("attr", Nil, None)), txt(" here")))
  }
  
  it should "ignore a start tag with a malformed attribute" in {
    val input = """some <tag attr="foo> here"""
    Parsing (input) should produce (spans(txt("""some <tag attr="foo> here""")))
  }
  
  it should "parse a start tag with two attribute values" in {
    val input = """some <tag attr1="value1" attr2="value2"> here"""
    Parsing (input) should produce (spans(txt("some "), startTag("tag", "attr1" -> txt("value1"), "attr2" -> txt("value2")), txt(" here")))
  }
  
  
  
  "The HTML character reference parser" should "parse a named reference" in {
    Parsing ("some &amp; ref") should produce (spans(txt("some "), charRef("&amp;"), txt(" ref")))
  }
  
  it should "parse a decimal reference" in {
    Parsing ("some &#201; ref") should produce (spans(txt("some "), charRef("&#201;"), txt(" ref")))
  }
  
  it should "parse a hex reference" in {
    Parsing ("some &#x2e; ref") should produce (spans(txt("some "), charRef("&#x2e;"), txt(" ref")))
  }
  
  it should "ignore a malformed named reference" in {
    Parsing ("some &amp ref") should produce (spans(txt("some &amp ref")))
  }
  
  it should "ignore a malformed decimal reference" in {
    Parsing ("some &#2e; ref") should produce (spans(txt("some &#2e; ref")))
  }
  
  it should "ignore a malformed hex reference" in {
    Parsing ("some &#x2g; ref") should produce (spans(txt("some &#x2g; ref")))
  }
  
  
  
  "The HTML comment parser" should "parse a standard HTML comment" in {
    Parsing ("some <!-- comment --> here") should produce (spans(txt("some "), comment(" comment "), txt(" here")))
  }
  
  it should "ignore other HTML tags inside the HTML comment" in {
    Parsing ("some <!-- <ignored>tags</ignored> --> here") should produce (spans(txt("some "), comment(" <ignored>tags</ignored> "), txt(" here")))
  }
  
  it should "ignore Markdown markup inside the HTML comment" in {
    Parsing ("some <!-- *comment* --> here") should produce (spans(txt("some "), comment(" *comment* "), txt(" here")))
  }
  
  
}
