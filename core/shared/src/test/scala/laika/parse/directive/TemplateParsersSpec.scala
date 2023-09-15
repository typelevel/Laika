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

package laika.parse.directive

import laika.api.config.Key
import laika.ast._
import laika.ast.sample.TestSourceBuilders
import laika.parse.Parser
import munit.FunSuite

class TemplateParsersSpec extends FunSuite with TestSourceBuilders {

  val defaultParser: Parser[List[Span]] = new TemplateParsers(Map()).templateSpans

  def parse(input: String): Either[String, List[Span]] = defaultParser.parse(input).toEither

  def result(spans: Span*): Either[String, List[Span]] = Right(spans.toList)

  test("content without any markup") {
    assertEquals(parse("some text"), result(TemplateString("some text")))
  }

  test("context reference as the only template content") {
    val input = "${document.content}"
    assertEquals(
      parse(input),
      result(
        TemplateContextReference(
          Key("document", "content"),
          required = true,
          generatedSource(input)
        )
      )
    )

  }

  test("context reference at the beginning of a template") {
    val ref   = "${document.content}"
    val input = s"$ref some text"
    assertEquals(
      parse(input),
      result(
        TemplateContextReference(Key("document", "content"), required = true, source(ref, input)),
        TemplateString(" some text")
      )
    )
  }

  test("context reference at the end of a template") {
    val ref   = "${document.content}"
    val input = s"some text $ref"
    assertEquals(
      parse(input),
      result(
        TemplateString("some text "),
        TemplateContextReference(Key("document", "content"), required = true, source(ref, input))
      )
    )

  }

  test("context reference in the middle of a template") {
    val ref   = "${document.content}"
    val input = s"some text $ref some more"
    assertEquals(
      parse(input),
      result(
        TemplateString("some text "),
        TemplateContextReference(Key("document", "content"), required = true, source(ref, input)),
        TemplateString(" some more")
      )
    )
  }

  test("optional context reference") {
    val ref   = "${?document.content}"
    val input = s"some text $ref some more"
    assertEquals(
      parse(input),
      result(
        TemplateString("some text "),
        TemplateContextReference(Key("document", "content"), required = false, source(ref, input)),
        TemplateString(" some more")
      )
    )
  }

  test("invalid context reference") {
    val errorMsg =
      """Invalid HOCON reference: '${document = content}': [1.22] failure: Invalid key: Illegal character in unquoted string, expected delimiter is '}'
        |
        |some text ${document = content} some more
        |                     ^""".stripMargin

    val ref   = "${document = content}"
    val input = s"some text $ref some more"
    assertEquals(
      parse(input),
      result(
        TemplateString("some text "),
        TemplateElement(InvalidSpan(errorMsg, source(ref, input))),
        TemplateString(" some more")
      )
    )

  }

}
