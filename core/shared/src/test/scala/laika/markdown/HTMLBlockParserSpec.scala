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
import laika.ast.{RootElement, Text}
import laika.ast.helper.ParagraphCompanionShortcuts
import laika.format.Markdown
import laika.markdown.ast.{HTMLAttribute, HTMLBlock, HTMLScriptElement}
import laika.parse.Parser
import laika.parse.helper.{DefaultParserHelpers, ParseResultHelpers}
import laika.parse.markup.RootParser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
 
class HTMLBlockParserSpec extends AnyFlatSpec 
                          with Matchers 
                          with ParseResultHelpers
                          with DefaultParserHelpers[RootElement] 
                          with ParagraphCompanionShortcuts 
                          with HTMLModelBuilder {


  val rootParser = new RootParser(Markdown, OperationConfig(Markdown.extensions).forRawContent.markupExtensions)

  val defaultParser: Parser[RootElement] = rootParser.rootElement

  
  "The HTML block parser" should "parse a block level HTML element with a nested element and text content" in {
    val input = """aaa
      |
      |<div>
      |  <span>foo</span>
      |</div>
      |
      |bbb""".stripMargin
    val inner = element("span", Text("foo"))
    val outer = element("div", Text("\n  "), inner, Text("\n"))
    Parsing (input) should produce (root(p("aaa"), HTMLBlock(outer), p("bbb")))
  }
  
  it should "ignore Markdown markup inside a block level HTML element" in {
    val input = """aaa
      |
      |<div>
      |  <span>*foo*</span>
      |</div>
      |
      |bbb""".stripMargin
    val inner = element("span", Text("*foo*"))
    val outer = element("div", Text("\n  "), inner, Text("\n"))
    Parsing (input) should produce (root(p("aaa"), HTMLBlock(outer), p("bbb")))
  }

  it should "recognize a script tag inside a block level HTML element" in {
    val input = """aaa
                  |
                  |<div>
                  |  <script>
                  |    var x = [1, 2, 3];
                  |    var y = 'foo';
                  |  </script>
                  |</div>
                  |
                  |bbb""".stripMargin
    val inner = HTMLScriptElement(Nil, "\n    var x = [1, 2, 3];\n    var y = 'foo';\n  ")
    val outer = element("div", Text("\n  "), inner, Text("\n"))
    Parsing (input) should produce (root(p("aaa"), HTMLBlock(outer), p("bbb")))
  }

  it should "recognize a script tag with attributes inside a block level HTML element" in {
    val input = """aaa
                  |
                  |<div>
                  |  <script type="text/javascript" defer>
                  |    var x = [1, 2, 3];
                  |    var y = 'foo';
                  |  </script>
                  |</div>
                  |
                  |bbb""".stripMargin
    val inner = HTMLScriptElement(List(
      HTMLAttribute("type", List(Text("text/javascript")),Some('"')),
      HTMLAttribute("defer", List(),None)
    ), "\n    var x = [1, 2, 3];\n    var y = 'foo';\n  ")
    val outer = element("div", Text("\n  "), inner, Text("\n"))
    Parsing (input) should produce (root(p("aaa"), HTMLBlock(outer), p("bbb")))
  }
  
  it should "ignore elements which are not listed as block-level elements" in {
    val input = """aaa
      |
      |<span>
      |  <span>foo</span>
      |</span>
      |
      |bbb""".stripMargin
    val inner = element("span", Text("foo"))
    val outer = element("span", Text("\n  "), inner, Text("\n"))
    Parsing (input) should produce (root(p("aaa"), p(outer), p("bbb")))
  }
  
  it should "ignore elements which are not at the very start of a block" in {
    val input = """aaa
      |
      |xx<div>
      |  <span>foo</span>
      |</div>
      |
      |bbb""".stripMargin
    val inner = element("span", Text("foo"))
    val outer = element("div", Text("\n  "), inner, Text("\n"))
    Parsing (input) should produce (root(p("aaa"), p(Text("xx"), outer), p("bbb")))
  }
  
  
}
