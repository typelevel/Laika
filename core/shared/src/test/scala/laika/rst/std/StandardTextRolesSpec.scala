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

package laika.rst.std

import cats.data.NonEmptySet
import laika.api.MarkupParser
import laika.ast._
import laika.ast.helper.{ParagraphCompanionShortcuts, TestSourceBuilders}
import laika.format.ReStructuredText
import laika.rst.ast.RstStyle
import laika.rst.ext.ExtensionProvider
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * @author Jens Halm
 */
class StandardTextRolesSpec extends AnyFlatSpec 
  with Matchers 
  with ParagraphCompanionShortcuts
  with TestSourceBuilders {

  private val parser = MarkupParser
    .of(ReStructuredText)
    .failOnMessages(MessageFilter.None)
    .build
  
  def parse (input: String): RootElement = parser.parse(input).toOption.get.content
  
  
  "The emphasis text role" should "produce an Emphasized node without styles" in {
    val input = "some :emphasis:`text`"
    val result = RootElement(p(Text("some "),Emphasized("text")))
    parse(input) should be (result)
  }
  
  it should "be customizable with a custom style" in {
    val input = """.. role:: foo(emphasis)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    val result = RootElement(p(Text("some "), Emphasized(List(Text("text")),Styles("foo"))))
    parse(input) should be (result)
  }
  
  
  "The strong text role" should "produce a Strong node without styles" in {
    val input = "some :strong:`text`"
    val result = RootElement(p(Text("some "),Strong("text")))
    parse(input) should be (result)
  }
  
  it should "be customizable with a custom style" in {
    val input = """.. role:: foo(strong)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    val result = RootElement(p(Text("some "), Strong(List(Text("text")),Styles("foo"))))
    parse(input) should be (result)
  }
  
  
  "The literal text role" should "produce a Literal node without styles" in {
    val input = "some :literal:`text`"
    val result = RootElement(p(Text("some "), Literal("text")))
    parse(input) should be (result)
  }
  
  it should "be customizable with a custom style" in {
    val input = """.. role:: foo(literal)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    val result = RootElement(p(Text("some "), Literal("text",Styles("foo"))))
    parse(input) should be (result)
  }
  
  
  "The subscript text role" should "produce a text node with subscript style" in {
    val input = "some :subscript:`text`"
    val result = RootElement(p(Text("some "), Text("text", RstStyle.subscript)))
    parse(input) should be (result)
  }
  
  it should "be customizable with a custom style" in {
    val input = """.. role:: foo(subscript)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    val result = RootElement(p(Text("some "), Text("text", Styles("foo") + RstStyle.subscript)))
    parse(input) should be (result)
  }
  
  it should "support the sub alias" in {
    val input = "some :sub:`text`"
    val result = RootElement(p(Text("some "), Text("text", RstStyle.subscript)))
    parse(input) should be (result)
  }
  
  
  "The superscript text role" should "produce a text node with superscript style" in {
    val input = "some :superscript:`text`"
    val result = RootElement(p(Text("some "), Text("text", RstStyle.superscript)))
    parse(input) should be (result)
  }
  
  it should "be customizable with a custom style" in {
    val input = """.. role:: foo(superscript)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    val result = RootElement(p(Text("some "), Text("text", Styles("foo") + RstStyle.superscript)))
    parse(input) should be (result)
  }
  
  it should "support the sup alias" in {
    val input = "some :sup:`text`"
    val result = RootElement(p(Text("some "), Text("text", RstStyle.superscript)))
    parse(input) should be (result)
  }
  
  
  "The title-reference text role" should "produce an Emphasized node with title-reference style" in {
    val input = "some :title-reference:`text`"
    val result = RootElement(p(Text("some "), Emphasized(List(Text("text")), RstStyle.titleReference)))
    parse(input) should be (result)
  }
  
  it should "be customizable with a custom style" in {
    val input = """.. role:: foo(title-reference)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    val result = RootElement(p(Text("some "), Emphasized(List(Text("text")), Styles("foo") + RstStyle.titleReference)))
    parse(input) should be (result)
  }
  
  it should "support the title alias" in {
    val input = "some :title:`text`"
    val result = RootElement(p(Text("some "), Emphasized(List(Text("text")), RstStyle.titleReference)))
    parse(input) should be (result)
  }
  
  it should "be used as the default role if none is specified" in {
    val input = "some `text`"
    val result = RootElement(p(Text("some "), Emphasized(List(Text("text")), RstStyle.titleReference)))
    parse(input) should be (result)
  }
  
  
  "The code text role" should "produce a code span without styles" in {
    val input = "some :code:`text`"
    val result = RootElement(p(Text("some "), InlineCode("", List(Text("text")))))
    parse(input) should be (result)
  }
  
  it should "be customizable with a custom style and language" in {
    val input = """.. role:: foo(code)
      | :class: foo
      | :language: banana-script
      |
      |some :foo:`text`""".stripMargin
    val result = RootElement(p(Text("some "), InlineCode("banana-script", List(Text("text")), Styles("foo"))))
    parse(input) should be (result)
  }
  
  
  "The raw text role" should "produce raw content with format and style options" in {
    val input = """.. role:: foo(raw)
      | :class: foo
      | :format: AML BML CML
      |
      |some :foo:`text`""".stripMargin
    val result = RootElement(p(Text("some "), RawContent(NonEmptySet.of("AML","BML","CML"), "text", Styles("foo"))))
    MarkupParser.of(ReStructuredText).withRawContent.build.parse(input).toOption.get.content should be (result)
  }

  it should "be disabled by default" in {
    val role = """.. role:: foo(raw)
                 | :format: AML BML CML""".stripMargin
    val input = s"""$role
      |
      |some :foo:`text`""".stripMargin
    val result = RootElement(
      InvalidBlock("unknown text role: raw", source(role, input)),
      p(Text("some "), InvalidSpan("unknown text role: foo", source(":foo:`text`", input)))
    )
    parse(input) should be (result)
  }
  
  
  "The default text role" should "be adjustable through the API" in {
    val input = "some `text`"
    val result = RootElement(p(Text("some "), Emphasized("text")))
    MarkupParser.of(ReStructuredText).using(ExtensionProvider.forDefaultTextRole("emphasis")).build.parse(input)
      .toOption.get.content should be (result)
  } 
  
  
}
