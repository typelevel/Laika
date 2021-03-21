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

import cats.effect.{IO, Resource}
import laika.io.implicits._
import laika.api.MarkupParser
import laika.ast.{Block, BlockSequence, EmbeddedRoot, Paragraph, Path, TemplateElement, TemplateRoot, TemplateSpanSequence, TemplateString}
import laika.ast.Path.Root
import laika.config.ConfigException
import laika.format.Markdown
import laika.io.{FileIO, IOFunSuite}
import laika.io.api.TreeParser
import laika.io.helper.InputBuilder
import laika.rewrite.{DefaultTemplatePath, TemplateContext, TemplateRewriter}
import laika.theme.Theme
import org.scalatest.matchers.should.Matchers


class IncludeDirectiveSpec extends IOFunSuite with Matchers with InputBuilder with FileIO {

  val parser: Resource[IO, TreeParser[IO]] = MarkupParser
    .of(Markdown)
    .parallel[IO]
    .withTheme(Theme.empty)
    .build
  
  def inputs (docUnderTest: String): Seq[(Path, String)] = {
    Seq(
      Root / "dir1" / "doc-1.md" -> "# Ref",
      Root / "dir1" / "doc-2.md" -> "# Ref",
      Root / "dir2" / "doc-3.md" -> "# Ref",
      Root / "dir2" / "doc-4.md" -> docUnderTest,
      Root / "inc" / "inc-1.md" -> "aaa (${?_.key}) bbb",
      Root / "inc" / "inc-2.md" ->
        """aaa (${?_.key}) bbb
          |
          |${?_.embeddedBody}
          |
          |ccc
        """.stripMargin,
      Root / "inc" / "inc-1.template.html" -> "aaa (${?_.key}) bbb",
      Root / "inc" / "inc-2.template.html" -> """aaa (${?_.key}) bbb <${?_.embeddedBody}> ccc"""
    )
  }
  
  
  def embedResult (firstPara: String): Seq[Block] = Seq(BlockSequence(
    Paragraph(firstPara),
    Paragraph(
      TemplateElement(
        BlockSequence(
          Paragraph("Par 1"),
          Paragraph("Par 2")
        )
      )
    ),
    Paragraph("ccc")
  ))
  
  def parseAndExtract (input: String, template: Option[String] = None): IO[Seq[Block]] = {
    val inputTree = build(inputs(input) ++ template.map((DefaultTemplatePath.forHTML, _)).toSeq)
    parser.use { _
      .fromInput(inputTree)
      .parse
    }
    .flatMap { tree =>
      IO.fromEither {
        TemplateRewriter.applyTemplates(tree.root, TemplateContext("html", "html"))
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
    parseAndExtract(markup).assertEquals(Seq(BlockSequence(Paragraph("aaa () bbb"))))
  }

  test("block include with attributes") {
    val markup = "@:include(../inc/inc-1.md) { key = foo }"
    parseAndExtract(markup).assertEquals(Seq(BlockSequence(Paragraph("aaa (foo) bbb"))))
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
  
  def includeTemplateResult (ref: String): TemplateRoot = TemplateRoot(
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

  def embedTemplateResult (ref: String): TemplateRoot = TemplateRoot(
    TemplateString("111 "),
    TemplateSpanSequence(
      TemplateString("aaa ("),
      TemplateString(ref),
      TemplateString(") bbb <"),
      TemplateSpanSequence(TemplateString(" body ")),
      TemplateString("> ccc"),
    ),
    TemplateString(" 222 "),
    EmbeddedRoot(Paragraph("Text")),
    TemplateString(" 333")
  )

  test("template include without attributes") {
    val template = "111 @:include(../inc/inc-1.template.html) 222 ${cursor.currentDocument.content} 333"
    parseAndExtract("Text", Some(template)).assertEquals(Seq(includeTemplateResult("")))
  }

  test("template include with attributes") {
    val template = "111 @:include(../inc/inc-1.template.html) { key = foo } 222 ${cursor.currentDocument.content} 333"
    parseAndExtract("Text", Some(template)).assertEquals(Seq(includeTemplateResult("foo")))
  }

  test("template embed without attributes") {
    val template = "111 @:embed(../inc/inc-2.template.html) body @:@ 222 ${cursor.currentDocument.content} 333"
    parseAndExtract("Text", Some(template)).assertEquals(Seq(embedTemplateResult("")))
  }

  test("template embed with attributes") {
    val template = "111 @:embed(../inc/inc-2.template.html) { key = foo } body @:@ 222 ${cursor.currentDocument.content} 333"
    parseAndExtract("Text", Some(template)).assertEquals(Seq(embedTemplateResult("foo")))
  }
  
}
