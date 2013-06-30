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

package laika.parse.rst.ext

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import laika.tree.helper.ModelBuilder
import laika.tree.Elements._
import laika.parse.rst.ReStructuredText
import laika.api.Parse

/**
 * @author Jens Halm
 */
class StandardTextRolesSpec extends FlatSpec 
                            with ShouldMatchers 
                            with ModelBuilder {

  
  "The emphasis text role" should "produce an Emphasized node without styles" in {
    val input = "some :emphasis:`text`"
    val result = doc(p(txt("some "),em("text")))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "be customizable with a custom style" in {
    val input = """.. role:: foo(emphasis)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    val result = doc(p(txt("some "), Emphasized(List(Text("text")),Styles("foo"))))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  
  "The strong text role" should "produce a Strong node without styles" in {
    val input = "some :strong:`text`"
    val result = doc(p(txt("some "),str("text")))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "be customizable with a custom style" in {
    val input = """.. role:: foo(strong)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    val result = doc(p(txt("some "), Strong(List(Text("text")),Styles("foo"))))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  
  "The literal text role" should "produce a Literal node without styles" in {
    val input = "some :literal:`text`"
    val result = doc(p(txt("some "), Literal("text")))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "be customizable with a custom style" in {
    val input = """.. role:: foo(literal)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    val result = doc(p(txt("some "), Literal("text",Styles("foo"))))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  
  "The subscript text role" should "produce a text node with subscript style" in {
    val input = "some :subscript:`text`"
    val result = doc(p(txt("some "), Text("text", Styles("subscript"))))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "be customizable with a custom style" in {
    val input = """.. role:: foo(subscript)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    val result = doc(p(txt("some "), Text("text",Styles("foo","subscript"))))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "support the sub alias" in {
    val input = "some :sub:`text`"
    val result = doc(p(txt("some "), Text("text", Styles("subscript"))))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  
  "The superscript text role" should "produce a text node with superscript style" in {
    val input = "some :superscript:`text`"
    val result = doc(p(txt("some "), Text("text", Styles("superscript"))))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "be customizable with a custom style" in {
    val input = """.. role:: foo(superscript)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    val result = doc(p(txt("some "), Text("text",Styles("foo","superscript"))))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "support the sup alias" in {
    val input = "some :sup:`text`"
    val result = doc(p(txt("some "), Text("text", Styles("superscript"))))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  
  "The title-reference text role" should "produce an Emphasized node with title-reference style" in {
    val input = "some :title-reference:`text`"
    val result = doc(p(txt("some "), Emphasized(List(Text("text")), Styles("title-reference"))))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "be customizable with a custom style" in {
    val input = """.. role:: foo(title-reference)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    val result = doc(p(txt("some "), Emphasized(List(Text("text")),Styles("foo","title-reference"))))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "support the title alias" in {
    val input = "some :title:`text`"
    val result = doc(p(txt("some "), Emphasized(List(Text("text")), Styles("title-reference"))))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "be used as the default role if none is specified" in {
    val input = "some `text`"
    val result = doc(p(txt("some "), Emphasized(List(Text("text")), Styles("title-reference"))))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  
  "The code text role" should "produce a code span without styles" in {
    val input = "some :code:`text`"
    val result = doc(p(txt("some "), Code("", List(Text("text")))))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  it should "be customizable with a custom style and language" in {
    val input = """.. role:: foo(code)
      | :class: foo
      | :language: banana-script
      |
      |some :foo:`text`""".stripMargin
    val result = doc(p(txt("some "), Code("banana-script", List(Text("text")), Styles("foo"))))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  
  "The raw text role" should "produce raw content with format and style options" in {
    val input = """.. role:: foo(raw)
      | :class: foo
      | :format: AML BML CML
      |
      |some :foo:`text`""".stripMargin
    val result = doc(p(txt("some "), RawContent(List("AML","BML","CML"), "text", Styles("foo"))))
    Parse as ReStructuredText.withRawContent fromString input should be (result)
  }

  it should "be disabled by default" in {
    val input = """.. role:: foo(raw)
      | :format: AML BML CML
      |
      |some :foo:`text`""".stripMargin
    val result = doc(InvalidBlock(SystemMessage(Error, "unknown text role: raw"), LiteralBlock(".. role::foo(raw) \n:format: AML BML CML")),
        p(txt("some "), InvalidSpan(SystemMessage(Error, "unknown text role: foo"), Text("`text`"))))
    Parse as ReStructuredText fromString input should be (result)
  }
  
  
  
}