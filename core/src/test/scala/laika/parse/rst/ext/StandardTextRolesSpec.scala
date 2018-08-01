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

package laika.parse.rst.ext

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import laika.tree.helper.ModelBuilder
import laika.tree.Elements._
import laika.parse.rst.ReStructuredText
import laika.api.Parse

/**
 * @author Jens Halm
 */
class StandardTextRolesSpec extends FlatSpec 
                            with Matchers 
                            with ModelBuilder {

  
  def parse (input: String): RootElement = (Parse as ReStructuredText fromString input).content
  
  
  "The emphasis text role" should "produce an Emphasized node without styles" in {
    val input = "some :emphasis:`text`"
    val result = root(p(txt("some "),em("text")))
    parse(input) should be (result)
  }
  
  it should "be customizable with a custom style" in {
    val input = """.. role:: foo(emphasis)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    val result = root(p(txt("some "), Emphasized(List(Text("text")),Styles("foo"))))
    parse(input) should be (result)
  }
  
  
  "The strong text role" should "produce a Strong node without styles" in {
    val input = "some :strong:`text`"
    val result = root(p(txt("some "),str("text")))
    parse(input) should be (result)
  }
  
  it should "be customizable with a custom style" in {
    val input = """.. role:: foo(strong)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    val result = root(p(txt("some "), Strong(List(Text("text")),Styles("foo"))))
    parse(input) should be (result)
  }
  
  
  "The literal text role" should "produce a Literal node without styles" in {
    val input = "some :literal:`text`"
    val result = root(p(txt("some "), Literal("text")))
    parse(input) should be (result)
  }
  
  it should "be customizable with a custom style" in {
    val input = """.. role:: foo(literal)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    val result = root(p(txt("some "), Literal("text",Styles("foo"))))
    parse(input) should be (result)
  }
  
  
  "The subscript text role" should "produce a text node with subscript style" in {
    val input = "some :subscript:`text`"
    val result = root(p(txt("some "), Text("text", Styles("subscript"))))
    parse(input) should be (result)
  }
  
  it should "be customizable with a custom style" in {
    val input = """.. role:: foo(subscript)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    val result = root(p(txt("some "), Text("text",Styles("foo","subscript"))))
    parse(input) should be (result)
  }
  
  it should "support the sub alias" in {
    val input = "some :sub:`text`"
    val result = root(p(txt("some "), Text("text", Styles("subscript"))))
    parse(input) should be (result)
  }
  
  
  "The superscript text role" should "produce a text node with superscript style" in {
    val input = "some :superscript:`text`"
    val result = root(p(txt("some "), Text("text", Styles("superscript"))))
    parse(input) should be (result)
  }
  
  it should "be customizable with a custom style" in {
    val input = """.. role:: foo(superscript)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    val result = root(p(txt("some "), Text("text",Styles("foo","superscript"))))
    parse(input) should be (result)
  }
  
  it should "support the sup alias" in {
    val input = "some :sup:`text`"
    val result = root(p(txt("some "), Text("text", Styles("superscript"))))
    parse(input) should be (result)
  }
  
  
  "The title-reference text role" should "produce an Emphasized node with title-reference style" in {
    val input = "some :title-reference:`text`"
    val result = root(p(txt("some "), Emphasized(List(Text("text")), Styles("title-reference"))))
    parse(input) should be (result)
  }
  
  it should "be customizable with a custom style" in {
    val input = """.. role:: foo(title-reference)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    val result = root(p(txt("some "), Emphasized(List(Text("text")),Styles("foo","title-reference"))))
    parse(input) should be (result)
  }
  
  it should "support the title alias" in {
    val input = "some :title:`text`"
    val result = root(p(txt("some "), Emphasized(List(Text("text")), Styles("title-reference"))))
    parse(input) should be (result)
  }
  
  it should "be used as the default role if none is specified" in {
    val input = "some `text`"
    val result = root(p(txt("some "), Emphasized(List(Text("text")), Styles("title-reference"))))
    parse(input) should be (result)
  }
  
  
  "The code text role" should "produce a code span without styles" in {
    val input = "some :code:`text`"
    val result = root(p(txt("some "), Code("", List(Text("text")))))
    parse(input) should be (result)
  }
  
  it should "be customizable with a custom style and language" in {
    val input = """.. role:: foo(code)
      | :class: foo
      | :language: banana-script
      |
      |some :foo:`text`""".stripMargin
    val result = root(p(txt("some "), Code("banana-script", List(Text("text")), Styles("foo"))))
    parse(input) should be (result)
  }
  
  
  "The raw text role" should "produce raw content with format and style options" in {
    val input = """.. role:: foo(raw)
      | :class: foo
      | :format: AML BML CML
      |
      |some :foo:`text`""".stripMargin
    val result = root(p(txt("some "), RawContent(List("AML","BML","CML"), "text", Styles("foo"))))
    Parse.as(ReStructuredText).withRawContent.fromString(input).content should be (result)
  }

  it should "be disabled by default" in {
    val input = """.. role:: foo(raw)
      | :format: AML BML CML
      |
      |some :foo:`text`""".stripMargin
    val result = root(InvalidElement("unknown text role: raw", ".. role::foo(raw) \n:format: AML BML CML").asBlock,
        p(txt("some "), InvalidElement("unknown text role: foo", "`text`").asSpan))
    parse(input) should be (result)
  }
  
  
  "The default text role" should "be adjustable through the API" in {
    val input = "some `text`"
    val result = root(p(txt("some "), Emphasized(List(Text("text")))))
    (Parse as ReStructuredText using ExtensionProvider.forDefaultTextRole("emphasis") fromString input)
      .content should be (result)
  } 
  
  
}
