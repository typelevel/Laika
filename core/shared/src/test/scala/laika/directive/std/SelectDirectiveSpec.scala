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

package laika.directive.std

import laika.api.{ MarkupParser, RenderPhaseRewrite }
import laika.ast.Path.Root
import laika.ast.*
import laika.ast.sample.{ ParagraphCompanionShortcuts, TestSourceBuilders }
import laika.config.{ ChoiceConfig, MessageFilter, SelectionConfig, Selections }
import laika.format.{ HTML, Markdown }
import munit.FunSuite

class SelectDirectiveSpec extends FunSuite with ParagraphCompanionShortcuts
    with TestSourceBuilders with RenderPhaseRewrite {

  private val parser = MarkupParser
    .of(Markdown)
    .failOnMessages(MessageFilter.None)
    .withConfigValue(
      Selections(
        SelectionConfig("config", ChoiceConfig("a", "label-a"), ChoiceConfig("b", "label-b"))
      )
    )
    .build

  def parse(input: String): RootElement = parser.parse(input).toOption.get.content

  def run(input: String, expected: Block)(implicit loc: munit.Location): Unit = {
    assertEquals(
      parser.parse(input).flatMap(rewrite(parser, HTML)).map(_.content.content),
      Right(Seq(p("aa"), expected, p("bb")))
    )
  }

  test("body with two alternatives") {
    val input = """aa
                  |
                  |@:select(config)
                  |
                  |@:choice(a)
                  |11
                  |22
                  |
                  |@:choice(b)
                  |33
                  |44
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    val group = Selection(
      "config",
      Seq(
        Choice("a", "label-a", List(p("11\n22"))),
        Choice("b", "label-b", List(p("33\n44")))
      )
    )
    run(input, group)
  }

  test("body with a two alternatives and a common body") {
    val input = """aa
                  |
                  |@:select(config)
                  |
                  |common
                  |
                  |@:choice(a)
                  |11
                  |22
                  |
                  |@:choice(b)
                  |33
                  |44
                  |
                  |@:@
                  |
                  |bb""".stripMargin
    val group = Selection(
      "config",
      Seq(
        Choice("a", "label-a", List(p("common"), p("11\n22"))),
        Choice("b", "label-b", List(p("common"), p("33\n44")))
      )
    )
    run(input, group)
  }

  test("fail with less than two alternatives in the body") {
    val directive =
      """@:select(config)
        |
        |@:choice(a)
        |11
        |22
        |
        |@:@""".stripMargin
    val input     = s"""aa
                   |
                   |$directive
                   |
                   |bb""".stripMargin
    val message   =
      "One or more errors processing directive 'select': too few occurrences of separator directive 'choice': expected min: 2, actual: 1"
    val invalid   = InvalidBlock(message, source(directive, input, defaultPath))
    run(input, invalid)
  }

  test("fail when a label is missing in the configuration") {
    val directive =
      """@:select(config)
        |
        |@:choice(a)
        |11
        |22
        |
        |@:choice(c)
        |33
        |44
        |
        |@:@""".stripMargin
    val input     =
      s"""aa
         |
         |$directive
         |
         |bb""".stripMargin
    val message   =
      "One or more errors processing directive 'select': No label defined for choice 'c' in selection 'config'"
    val invalid   = InvalidBlock(message, source(directive, input, defaultPath))
    run(input, invalid)
  }

  test("unwrap a selected choice in the template rewrite rules") {
    val group  = Selection(
      "config",
      Seq(
        Choice("a", "label-a", List(p("common"), p("11\n22"))),
        Choice("b", "label-b", List(p("common"), p("33\n44")))
      )
    )
    val config = Selections(
      SelectionConfig(
        "config",
        ChoiceConfig("a", "label-a"),
        ChoiceConfig("b", "label-b").select
      )
    )
    val doc    = Document(Root / "doc", RootElement(group))
      .modifyConfig(_.withValue(config))

    assertEquals(
      rewrite(HTML)(doc).map(_.content),
      Right(RootElement(BlockSequence(List(p("common"), p("33\n44")))))
    )
  }

}
