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

package laika.parse.markdown.html

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import laika.parse.helper.DefaultParserHelpers
import laika.parse.helper.ParseResultHelpers
import laika.parse.markdown.html.HTMLElements.HTMLBlock
import laika.tree.Elements.RootElement
import laika.tree.helper.ModelBuilder
 
class HTMLBlockParserSpec extends FlatSpec 
                          with ShouldMatchers 
                          with HTMLParsers 
                          with ParseResultHelpers 
                          with DefaultParserHelpers[RootElement] 
                          with ModelBuilder 
                          with HTMLModelBuilder {
 
 
  
  val defaultParser: Parser[RootElement] = root
  
  
  "The HTML block parser" should "parse a block level HTML element with a nested element and text content" in {
    val input = """aaa
      |
      |<div>
      |  <span>foo</span>
      |</div>
      |
      |bbb""".stripMargin
    val inner = element("span", txt("foo"))
    val outer = element("div", txt("\n  "), inner, txt("\n"))
    Parsing (input) should produce (doc(p("aaa"), HTMLBlock(outer), p("bbb")))
  }
  
  it should "ignore Markdown markup inside a block level HTML element" in {
    val input = """aaa
      |
      |<div>
      |  <span>*foo*</span>
      |</div>
      |
      |bbb""".stripMargin
    val inner = element("span", txt("*foo*"))
    val outer = element("div", txt("\n  "), inner, txt("\n"))
    Parsing (input) should produce (doc(p("aaa"), HTMLBlock(outer), p("bbb")))
  }
  
  it should "ignore elements which are not listed as block-level elements" in {
    val input = """aaa
      |
      |<span>
      |  <span>foo</span>
      |</span>
      |
      |bbb""".stripMargin
    val inner = element("span", txt("foo"))
    val outer = element("span", txt("\n  "), inner, txt("\n"))
    Parsing (input) should produce (doc(p("aaa"), p(outer), p("bbb")))
  }
  
  it should "ignore elements which are not at the very start of a block" in {
    val input = """aaa
      |
      |xx<div>
      |  <span>foo</span>
      |</div>
      |
      |bbb""".stripMargin
    val inner = element("span", txt("foo"))
    val outer = element("div", txt("\n  "), inner, txt("\n"))
    Parsing (input) should produce (doc(p("aaa"), p(txt("xx"), outer), p("bbb")))
  }
  
  
  
  
}