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
import laika.ast.sample.{ParagraphCompanionShortcuts, TestSourceBuilders}
import laika.format.ReStructuredText
import laika.rst.ast.RstStyle
import laika.rst.ext.ExtensionProvider
import munit.FunSuite

/**
 * @author Jens Halm
 */
class StandardTextRolesSpec extends FunSuite with ParagraphCompanionShortcuts with TestSourceBuilders {

  private val parser = MarkupParser
    .of(ReStructuredText)
    .failOnMessages(MessageFilter.None)
    .build
  
  def run (input: String, expected: Block*)(implicit loc: munit.Location): Unit =
    assertEquals(parser.parse(input).map(_.content), Right(RootElement(expected)))

  def run (input: String, expected: Span)(implicit loc: munit.Location): Unit = run(input, p(Text("some "), expected))
  
  
  test("emphasis - without styles") {
    val input = "some :emphasis:`text`"
    run(input, Emphasized("text"))
  }
  
  test("emphasis - with custom style") {
    val input = """.. role:: foo(emphasis)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    run(input, Emphasized(List(Text("text")),Styles("foo")))
  }
  
  
  test("strong - without styles") {
    val input = "some :strong:`text`"
    run(input, Strong("text"))
  }
  
  test("strong - with custom style") {
    val input = """.. role:: foo(strong)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    run(input, Strong(List(Text("text")),Styles("foo")))
  }
  
  
  test("literal - without styles") {
    val input = "some :literal:`text`"
    run(input, Literal("text"))
  }
  
  test("literal - with custom style") {
    val input = """.. role:: foo(literal)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    run(input, Literal("text",Styles("foo")))
  }
  
  
  test("subscript - without styles") {
    val input = "some :subscript:`text`"
    run(input, Text("text", RstStyle.subscript))
  }
  
  test("subscript - with custom style") {
    val input = """.. role:: foo(subscript)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    run(input, Text("text", Styles("foo") + RstStyle.subscript))
  }
  
  test("subscript - sub alias") {
    val input = "some :sub:`text`"
    run(input, Text("text", RstStyle.subscript))
  }
  
  
  test("superscript - without styles") {
    val input = "some :superscript:`text`"
    run(input, Text("text", RstStyle.superscript))
  }
  
  test("superscript - with custom style") {
    val input = """.. role:: foo(superscript)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    run(input, Text("text", Styles("foo") + RstStyle.superscript))
  }
  
  test("superscript - sup alias") {
    val input = "some :sup:`text`"
    run(input, Text("text", RstStyle.superscript))
  }
  
  
  test("title-reference - without styles") {
    val input = "some :title-reference:`text`"
    run(input, Emphasized(List(Text("text")), RstStyle.titleReference))
  }
  
  test("title-reference - with custom style") {
    val input = """.. role:: foo(title-reference)
      | :class: foo
      |
      |some :foo:`text`""".stripMargin
    run(input, Emphasized(List(Text("text")), Styles("foo") + RstStyle.titleReference))
  }
  
  test("support the title alias") {
    val input = "some :title:`text`"
    run(input, Emphasized(List(Text("text")), RstStyle.titleReference))
  }
  
  test("be used as the default role if none is specified") {
    val input = "some `text`"
    run(input, Emphasized(List(Text("text")), RstStyle.titleReference))
  }
  
  
  test("code - without styles") {
    val input = "some :code:`text`"
    run(input, InlineCode("", List(Text("text"))))
  }
  
  test("code - with custom style and language") {
    val input = """.. role:: foo(code)
      | :class: foo
      | :language: banana-script
      |
      |some :foo:`text`""".stripMargin
    run(input, InlineCode("banana-script", List(Text("text")), Styles("foo")))
  }
  
  
  test("raw - with format and style options") {
    val input = """.. role:: foo(raw)
      | :class: foo
      | :format: AML BML CML
      |
      |some :foo:`text`""".stripMargin
    val res = MarkupParser.of(ReStructuredText).withRawContent.build.parse(input).map(_.content)
    val expected = RootElement(p(Text("some "), RawContent(NonEmptySet.of("AML","BML","CML"), "text", Styles("foo"))))
    assertEquals(res, Right(expected))
  }

  test("raw - disabled by default") {
    val role = """.. role:: foo(raw)
                 | :format: AML BML CML""".stripMargin
    val input = s"""$role
      |
      |some :foo:`text`""".stripMargin
    run(input, 
      InvalidBlock("unknown text role: raw", source(role, input, defaultPath)),
      p(Text("some "), InvalidSpan("unknown text role: foo", source(":foo:`text`", input, defaultPath)))
    )
  }
  
  
  test("default text configurable through the API") {
    val input = "some `text`"
    val res = MarkupParser
      .of(ReStructuredText)
      .using(ExtensionProvider.forDefaultTextRole("emphasis"))
      .build
      .parse(input)
      .map(_.content)
    val expected = RootElement(p(Text("some "), Emphasized("text")))
    assertEquals(res, Right(expected))
  } 
  
  
}
