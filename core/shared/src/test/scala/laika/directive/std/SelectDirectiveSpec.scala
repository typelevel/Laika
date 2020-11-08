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

import laika.api.MarkupParser
import laika.ast.Path.Root
import laika.ast._
import laika.ast.helper.{ModelBuilder, TestSourceBuilders}
import laika.config.ConfigBuilder
import laika.format.Markdown
import laika.rewrite.TemplateRewriter
import laika.rewrite.nav.{ChoiceConfig, SelectionConfig, Selections}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SelectDirectiveSpec extends AnyFlatSpec
  with Matchers
  with ModelBuilder
  with TestSourceBuilders {


  val parser = MarkupParser
    .of(Markdown)
    .failOnMessages(MessageFilter.None)
    .withConfigValue(Selections(
      SelectionConfig("config",
        ChoiceConfig("a", "label-a"),
        ChoiceConfig("b", "label-b")
      )
    ))
    .build

  def parse (input: String): RootElement = parser.parse(input).toOption.get.content
  

  "The select directive" should "parse a body with a two alternatives" in {
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
    val group = Selection("config", Seq(
      Choice("a","label-a", List(p("11\n22"))),
      Choice("b","label-b", List(p("33\n44")))
    ))
    parse(input) should be (RootElement(p("aa"), group, p("bb")))
  }

  it should "parse a body with a two alternatives and a common body" in {
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
    val group = Selection("config", Seq(
      Choice("a","label-a", List(p("common"), p("11\n22"))),
      Choice("b","label-b", List(p("common"), p("33\n44")))
    ))
    parse(input) should be (RootElement(p("aa"), group, p("bb")))
  }

  it should "fail with less than two alternatives in the body" in {
    val directive =
      """@:select(config)
        |
        |@:choice(a)
        |11
        |22
        |
        |@:@""".stripMargin
    val input = s"""aa
                   |
                  |$directive
                   |
                  |bb""".stripMargin
    val message = "One or more errors processing directive 'select': too few occurrences of separator directive 'choice': expected min: 2, actual: 1"
    val invalid = InvalidBlock(message, source(directive, input))
    parse(input) should be (RootElement(p("aa"), invalid, p("bb")))
  }

  it should "fail when a label is missing in the configuration" in {
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
    val input =
      s"""aa
         |
         |$directive
         |
         |bb""".stripMargin
    val message = "One or more errors processing directive 'select': No label defined for choice 'c' in selection 'config'"
    val invalid = InvalidBlock(message, source(directive, input))
    parse(input) should be (RootElement(p("aa"),
      invalid, p("bb")))
  }

  it should "unwrap a selected choice in the template rewrite rules" in {
    val group = Selection("config", Seq(
      Choice("a","label-a", List(p("common"), p("11\n22"))),
      Choice("b","label-b", List(p("common"), p("33\n44")))
    ))
    val config = Selections(
      SelectionConfig("config", ChoiceConfig("a", "label-a"), ChoiceConfig("b", "label-b", selected = true))
    )
    val doc = Document(Root / "doc", RootElement(group))
    val tree = DocumentTreeRoot(DocumentTree(Root, Seq(doc), config = ConfigBuilder.empty.withValue(config).build))
    val cursor = DocumentCursor(doc, TreeCursor(RootCursor(tree)), tree.config, TreePosition(Nil))
    val rewritten = TemplateRewriter.applyTemplate(cursor, TemplateDocument(Root, TemplateRoot.fallback))
    rewritten.map(_.content) shouldBe Right(RootElement(BlockSequence(List(p("common"), p("33\n44")))))
  }
  
}
