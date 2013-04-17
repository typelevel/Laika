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

package laika.parse.rst

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import laika.render.HTML
import laika.api.Render
import laika.tree.Elements._
import laika.parse.rst.Elements._
import laika.tree.helper.ModelBuilder

class ExtendedHTMLSpec extends FlatSpec 
                       with ShouldMatchers
                       with ModelBuilder {
 
  
  def render (elem: Element) = Render as HTML using ExtendedHTML from elem toString 
  
  def render (elem: Element, messageLevel: MessageLevel) = 
    Render as (HTML withMessageLevel messageLevel) using ExtendedHTML from elem toString
  
  
  "The Extended HTML renderer" should "render a doctest block" in {
    val elem = DoctestBlock("some text")
    render (elem) should be ("""<pre class="doctest-block">&gt;&gt;&gt; some text</pre>""") 
  }
  
  it should "render a field list" in {
    val elem = FieldList(List(
      Field(List(txt("name 1")), List(fc("value 1"))),
      Field(List(txt("name 2")), List(fc("value 2")))
    ))
    val html = """<table class="field-list">
      |  <colgroup>
      |    <col class="field-name" />
      |    <col class="field-body" />
      |  </colgroup>
      |  <tbody>
      |    <tr>
      |      <td>name 1:</td>
      |      <td>value 1</td>
      |    </tr>
      |    <tr>
      |      <td>name 2:</td>
      |      <td>value 2</td>
      |    </tr>
      |  </tbody>
      |</table>""".stripMargin
    render (elem) should be (html)
  }
  
  it should "render a list of program options" in {
    val option1 = OptionListItem(List(ProgramOption("-a", Some(OptionArgument("arg"," ")))), List(fc("Description 1")))
    val option2 = OptionListItem(List(ProgramOption("-b", None), ProgramOption("--foo", Some(OptionArgument("bar","=")))), List(fc("Description 2")))
    val elem = OptionList(List(option1,option2))
    val html = """<table class="option-list">
      |  <colgroup>
      |    <col class="option" />
      |    <col class="description" />
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
    render (elem) should be (html)
  }
  
  
  
}