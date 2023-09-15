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

package laika.rst.bundle

import laika.api.Renderer
import laika.api.errors.RendererError
import laika.ast.*
import laika.ast.sample.ParagraphCompanionShortcuts
import laika.format.{ HTML, ReStructuredText }
import laika.rst.ast.*
import munit.FunSuite

class ExtendedHTMLRendererSpec extends FunSuite with ParagraphCompanionShortcuts {

  def render(elem: Element): Either[RendererError, String] =
    Renderer.of(HTML).using(ReStructuredText.extensions: _*).build.render(elem)

  def render(elem: Element, messageFilter: MessageFilter): Either[RendererError, String] =
    Renderer.of(HTML).renderMessages(messageFilter).using(
      ReStructuredText.extensions: _*
    ).build.render(elem)

  test("render a doctest block") {
    val elem = DoctestBlock("some text")
    val html = """<pre class="doctest-block">&gt;&gt;&gt; some text</pre>"""
    assertEquals(render(elem), Right(html))
  }

  test("render a field list") {
    val elem = FieldList(
      List(
        Field(List(Text("name 1")), List(p("value 1"))),
        Field(List(Text("name 2")), List(p("value 2")))
      )
    )
    val html = """<table class="field-list">
                 |  <colgroup>
                 |    <col class="field-name"></col>
                 |    <col class="field-body"></col>
                 |  </colgroup>
                 |  <tbody>
                 |    <tr>
                 |      <th>name 1:</th>
                 |      <td>value 1</td>
                 |    </tr>
                 |    <tr>
                 |      <th>name 2:</th>
                 |      <td>value 2</td>
                 |    </tr>
                 |  </tbody>
                 |</table>""".stripMargin
    assertEquals(render(elem), Right(html))
  }

  test("render a list of program options") {
    val option1 = OptionListItem(
      List(ProgramOption("-a", Some(OptionArgument("arg", " ")))),
      List(p("Description 1"))
    )
    val option2 = OptionListItem(
      List(ProgramOption("-b", None), ProgramOption("--foo", Some(OptionArgument("bar", "=")))),
      List(p("Description 2"))
    )
    val elem    = OptionList(List(option1, option2))
    val html    =
      """<table class="option-list">
        |  <colgroup>
        |    <col class="option"></col>
        |    <col class="description"></col>
        |  </colgroup>
        |  <tbody>
        |    <tr>
        |      <td>
        |        <kbd><span class="option">-a <var>arg</var></span></kbd>
        |      </td>
        |      <td>Description 1</td>
        |    </tr>
        |    <tr>
        |      <td>
        |        <kbd><span class="option">-b</span>, <span class="option">--foo=<var>bar</var></span></kbd>
        |      </td>
        |      <td>Description 2</td>
        |    </tr>
        |  </tbody>
        |</table>""".stripMargin
    assertEquals(render(elem), Right(html))
  }

  test("render nested line blocks") {
    val elem = LineBlock(LineBlock(Line("1"), Line("2")), Line("3"))
    val html = """<div class="line-block">
                 |  <div class="line-block">
                 |    <div class="line">1</div>
                 |    <div class="line">2</div>
                 |  </div>
                 |  <div class="line">3</div>
                 |</div>""".stripMargin
    assertEquals(render(elem), Right(html))
  }

}
