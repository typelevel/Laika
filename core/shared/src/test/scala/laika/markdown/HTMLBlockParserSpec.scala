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
import laika.ast.html.{ HTMLAttribute, HTMLBlock, HTMLScriptElement }
import laika.ast.sample.ParagraphCompanionShortcuts
import laika.ast.{ Block, RootElement, Text, html }
import laika.format.Markdown
import laika.internal.parse.markup.RootParser
import laika.parse.Parser
import munit.FunSuite

class HTMLBlockParserSpec extends FunSuite
    with ParagraphCompanionShortcuts
    with HTMLModelBuilder {

  val rootParser =
    new RootParser(
      Markdown,
      new OperationConfig(Markdown.extensions).forRawContent.markupExtensions
    )

  val defaultParser: Parser[RootElement] = rootParser.rootElement

  def run(input: String, blocks: Block*)(implicit loc: munit.Location): Unit =
    assertEquals(defaultParser.parse(input).toEither, Right(RootElement(blocks)))

  test("block level HTML element with a nested element and text content") {
    val input = """aaa
                  |
                  |<div>
                  |  <span>foo</span>
                  |</div>
                  |
                  |bbb""".stripMargin
    val inner = element("span", Text("foo"))
    val outer = element("div", Text("\n  "), inner, Text("\n"))
    run(input, p("aaa"), HTMLBlock(outer), p("bbb"))
  }

  test("ignore Markdown markup inside a block level HTML element") {
    val input = """aaa
                  |
                  |<div>
                  |  <span>*foo*</span>
                  |</div>
                  |
                  |bbb""".stripMargin
    val inner = element("span", Text("*foo*"))
    val outer = element("div", Text("\n  "), inner, Text("\n"))
    run(input, p("aaa"), HTMLBlock(outer), p("bbb"))
  }

  test("recognize a script tag inside a block level HTML element") {
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
    run(input, p("aaa"), html.HTMLBlock(outer), p("bbb"))
  }

  test("recognize a script tag with attributes inside a block level HTML element") {
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
    val inner = HTMLScriptElement(
      List(
        HTMLAttribute("type", List(Text("text/javascript")), Some('"')),
        HTMLAttribute("defer", List(), None)
      ),
      "\n    var x = [1, 2, 3];\n    var y = 'foo';\n  "
    )
    val outer = element("div", Text("\n  "), inner, Text("\n"))
    run(input, p("aaa"), html.HTMLBlock(outer), p("bbb"))
  }

  test("recognize a script tag as a root element") {
    val input  = """aaa
                  |
                  |<script type="text/javascript" defer>
                  |  var x = [1, 2, 3];
                  |  var y = 'foo';
                  |</script>
                  |
                  |bbb""".stripMargin
    val script = HTMLScriptElement(
      List(
        HTMLAttribute("type", List(Text("text/javascript")), Some('"')),
        HTMLAttribute("defer", List(), None)
      ),
      "\n  var x = [1, 2, 3];\n  var y = 'foo';\n"
    )
    run(input, p("aaa"), script, p("bbb"))
  }

  test("ignore elements which are not listed as block-level elements") {
    val input = """aaa
                  |
                  |<span>
                  |  <span>foo</span>
                  |</span>
                  |
                  |bbb""".stripMargin
    val inner = element("span", Text("foo"))
    val outer = element("span", Text("\n  "), inner, Text("\n"))
    run(input, p("aaa"), p(outer), p("bbb"))
  }

  test("ignore elements which are not at the very start of a block") {
    val input = """aaa
                  |
                  |xx<div>
                  |  <span>foo</span>
                  |</div>
                  |
                  |bbb""".stripMargin
    val inner = element("span", Text("foo"))
    val outer = element("div", Text("\n  "), inner, Text("\n"))
    run(input, p("aaa"), p(Text("xx"), outer), p("bbb"))
  }

}
