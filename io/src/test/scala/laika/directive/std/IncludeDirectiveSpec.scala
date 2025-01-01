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

import cats.effect.{ IO, Resource }
import laika.api.MarkupParser
import laika.api.builder.OperationConfig
import laika.ast.Path.Root
import laika.ast.*
import laika.format.{ HTML, Markdown }
import laika.io.api.TreeParser
import laika.io.helper.InputBuilder
import laika.io.internal.errors.ConfigException
import laika.io.syntax.*
import laika.theme.Theme
import munit.CatsEffectSuite

class IncludeDirectiveSpec extends CatsEffectSuite with InputBuilder {

  val parser: Resource[IO, TreeParser[IO]] = MarkupParser
    .of(Markdown)
    .parallel[IO]
    .withTheme(Theme.empty)
    .build

  def inputs(docUnderTest: String): Seq[(Path, String)] = {
    Seq(
      Root / "dir1" / "doc-1.md"           -> "# Ref",
      Root / "dir1" / "doc-2.md"           -> "# Ref",
      Root / "dir2" / "doc-3.md"           -> "# Ref",
      Root / "dir2" / "doc-4.md"           -> docUnderTest,
      Root / "inc" / "inc-1.md"            -> "aaa (${?_.key}) bbb",
      Root / "inc" / "inc-2.md"            ->
        """aaa (${?_.key}) bbb
          |
          |${?_.embeddedBody}
          |
          |ccc
        """.stripMargin,
      Root / "inc" / "header.md"           ->
        """# Header
          |
          |aaa (${?_.key}) bbb
          |""".stripMargin,
      Root / "inc" / "header-embed.md"     ->
        """# Header
          |
          |aaa (${?_.key}) bbb
          |
          |${?_.embeddedBody}
          |
          |ccc
          |""".stripMargin,
      Root / "inc" / "inc-1.template.html" -> "aaa (${?_.key}) bbb",
      Root / "inc" / "inc-2.template.html" -> """aaa (${?_.key}) bbb <${?_.embeddedBody}> ccc"""
    )
  }

  def embedResult(firstPara: String): Seq[Block] = embedResult(Seq(Paragraph(firstPara)))

  def embedResult(blocks: Seq[Block]): Seq[Block] = {
    val composedBlocks = blocks ++ Seq(
      Paragraph(
        TemplateElement(
          BlockSequence(
            Paragraph("Par 1"),
            Paragraph("Par 2")
          )
        )
      ),
      Paragraph("ccc")
    )
    composedBlocks
  }

  def parseAndExtract(input: String, template: Option[String] = None): IO[Seq[Block]] = {
    val inputTree = build(inputs(input) ++ template.map((DefaultTemplatePath.forHTML, _)).toSeq)
    parser.use {
      _
        .fromInput(inputTree)
        .parse
    }
      .flatMap { tree =>
        IO.fromEither {
          tree.root.applyTemplates(
            OperationConfig.default.rewriteRulesFor(tree.root, RewritePhase.Render(HTML)),
            OutputContext(HTML)
          )
            .left.map(ConfigException.apply)
            .flatMap { root =>
              root.tree.selectDocument("dir2/doc-4.md")
                .map(_.content.content)
                .toRight(new RuntimeException("document under test missing"))
            }
        }
      }
  }

  test("block include without attributes") {
    val markup = "@:include(../inc/inc-1.md)"
    parseAndExtract(markup).assertEquals(Seq(Paragraph("aaa () bbb")))
  }

  test("block include with header") {
    val markup = "@:include(../inc/header.md)"
    parseAndExtract(markup).assertEquals(
      Seq(Title("Header").withId("header").withStyle("title"), Paragraph("aaa () bbb"))
    )
  }

  test("block include with attributes") {
    val markup = "@:include(../inc/inc-1.md) { key = foo }"
    parseAndExtract(markup).assertEquals(Seq(Paragraph("aaa (foo) bbb")))
  }

  test("block embed without attributes") {
    val markup =
      """@:embed(../inc/inc-2.md)
        |
        |Par 1
        |
        |Par 2
        |
        |@:@
      """.stripMargin
    parseAndExtract(markup).assertEquals(embedResult("aaa () bbb"))
  }

  test("block embed with header") {
    val markup =
      """@:embed(../inc/header-embed.md)
        |
        |Par 1
        |
        |Par 2
        |
        |@:@
      """.stripMargin
    parseAndExtract(markup).assertEquals(
      embedResult(
        Seq(Title("Header").withId("header").withStyle("title"), Paragraph("aaa () bbb"))
      )
    )
  }

  test("block embed with attributes") {
    val markup =
      """@:embed(../inc/inc-2.md) { key = foo }
        |
        |Par 1
        |
        |Par 2
        |
        |@:@
      """.stripMargin
    parseAndExtract(markup).assertEquals(embedResult("aaa (foo) bbb"))
  }

  def includeTemplateResult(ref: String): TemplateRoot = TemplateRoot(
    TemplateString("111 "),
    TemplateSpanSequence(
      TemplateString("aaa ("),
      TemplateString(ref),
      TemplateString(") bbb")
    ),
    TemplateString(" 222 "),
    EmbeddedRoot(Paragraph("Text")),
    TemplateString(" 333")
  )

  def embedTemplateResult(ref: String): TemplateRoot = TemplateRoot(
    TemplateString("111 "),
    TemplateSpanSequence(
      TemplateString("aaa ("),
      TemplateString(ref),
      TemplateString(") bbb <"),
      TemplateSpanSequence(TemplateString(" body ")),
      TemplateString("> ccc")
    ),
    TemplateString(" 222 "),
    EmbeddedRoot(Paragraph("Text")),
    TemplateString(" 333")
  )

  test("template include without attributes") {
    val template =
      "111 @:include(../inc/inc-1.template.html) 222 ${cursor.currentDocument.content} 333"
    parseAndExtract("Text", Some(template)).assertEquals(Seq(includeTemplateResult("")))
  }

  test("template include with attributes") {
    val template =
      "111 @:include(../inc/inc-1.template.html) { key = foo } 222 ${cursor.currentDocument.content} 333"
    parseAndExtract("Text", Some(template)).assertEquals(Seq(includeTemplateResult("foo")))
  }

  test("template embed without attributes") {
    val template =
      "111 @:embed(../inc/inc-2.template.html) body @:@ 222 ${cursor.currentDocument.content} 333"
    parseAndExtract("Text", Some(template)).assertEquals(Seq(embedTemplateResult("")))
  }

  test("template embed with attributes") {
    val template =
      "111 @:embed(../inc/inc-2.template.html) { key = foo } body @:@ 222 ${cursor.currentDocument.content} 333"
    parseAndExtract("Text", Some(template)).assertEquals(Seq(embedTemplateResult("foo")))
  }

}
