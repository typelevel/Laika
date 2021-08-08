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

package laika.api

import laika.ast.Path.Root
import laika.ast._
import laika.ast.sample.{ParagraphCompanionShortcuts, TestSourceBuilders}
import laika.format.Markdown
import laika.parse.markup.DocumentParser.ParserError
import laika.rewrite.TemplateRewriter
import munit.FunSuite


class ParseAPISpec extends FunSuite
                   with ParagraphCompanionShortcuts
                   with TestSourceBuilders {
  
  
  val parser: MarkupParser = MarkupParser.of(Markdown).build

  
  test("Markdown from a string") {
    val input = """aaa
      |bbb
      |ccc""".stripMargin
    assertEquals(parser.parse(input).map(_.content), Right(RootElement(p(input))))
  }
  
  test("Markdown with all link references resolved through the default rewrite rules") {
    val input = """[link][id]
      |
      |[id]: http://foo/""".stripMargin
    assertEquals(parser.parse(input).map(_.content), Right(RootElement(p(
      SpanLink.external("http://foo/")("link")
    ))))
  }

  test("set a config value programmatically") {
    val input = "aa ${prop} bb"
    val parser = MarkupParser.of(Markdown).withConfigValue("prop", "foo").build
    assertEquals(parser.parse(input).map(_.content), Right(RootElement(p(
      Text("aa foo bb")
    ))))
  }
  
  test("parse Markdown into a raw document, without applying the default rewrite rules") {
    val input = """[link][id]
      |
      |[id]: http://foo/""".stripMargin
    val parser = MarkupParser.of(Markdown).build
    assertEquals(parser.parseUnresolved(input).map(_.document.content), Right(RootElement(
      p(LinkIdReference(List(Text("link")), "id", source("[link][id]", input, defaultPath))), 
      LinkDefinition("id", ExternalTarget("http://foo/"), None)
    )))
  }
  
  test("collect errors from runtime messages") {
    val input = """[invalid1]
                  |
                  |Text
                  |
                  |[invalid2]""".stripMargin
    val msg = """One or more error nodes in result:
                |  [1]: unresolved link id reference: invalid1
                |
                |  [invalid1]
                |  ^
                |
                |  [5]: unresolved link id reference: invalid2
                |
                |  [invalid2]
                |  ^""".stripMargin
    assertEquals(MarkupParser.of(Markdown).build.parse(input), Left(ParserError(msg, Root / "doc")))
  }
  
  test("replace unresolved nodes with invalid elements") {
    val input = """[invalid1]
                  |
                  |Text
                  |
                  |[invalid2]""".stripMargin
    val doc = MarkupParser.of(Markdown).build.parseUnresolved(input).toOption.get.document
    val res = DocumentCursor(doc)
      .flatMap(cursor => doc.rewrite(TemplateRewriter.rewriteRules(cursor)))
      .map(_.content)
    assertEquals(res, Right(RootElement(
      p(InvalidSpan("Unresolved link id reference 'invalid1'", source("[invalid1]", input, defaultPath))),
      p("Text"),
      p(InvalidSpan("Unresolved link id reference 'invalid2'", source("[invalid2]", input, defaultPath))),
    )))
  }
  
}
