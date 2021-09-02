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

package laika.parse

import laika.api.MarkupParser
import laika.format.Markdown
import laika.markdown.github.GitHubFlavor
import laika.parse.markup.DocumentParser.ParserError
import munit.FunSuite

/**
  * @author Jens Halm
  */
class PositionTrackingSpec extends FunSuite {

  private val parser = MarkupParser.of(Markdown).using(GitHubFlavor).build

  def parseAndExtractMessage (input: String): String = parser.parse(input) match {
    case Left(ParserError(message, _)) => message
    case Right(doc) => s"Unexpected success: $doc"
  }
  
  def run (input: String, expectedMessage: String): Unit =
    assertEquals(parseAndExtractMessage(input), expectedMessage)

  test("report an invalid block directive") {
    val input =
      """
        |@:invalid
        |
        |content
        |
        |@:@
      """.stripMargin

    val expectedMessage =
      """One or more error nodes in result:
        |  [2]: One or more errors processing directive 'invalid': No block directive registered with name: invalid
        |
        |  @:invalid
        |  ^""".stripMargin
    run(input, expectedMessage)
  }

  test("report an invalid span directive") {
    val input =
      """
        |Some @:invalid content.
      """.stripMargin

    val expectedMessage =
      """One or more error nodes in result:
        |  [2]: One or more errors processing directive 'invalid': No span directive registered with name: invalid
        |
        |  Some @:invalid content.
        |       ^""".stripMargin
    run(input, expectedMessage)
  }

  test("report an invalid block directive inside a list element") {
    val input =
      """* line 1
        |
        |  @:invalid
        |
        |  content
        |
        |  @:@
        |  
        |  line 2
      """.stripMargin

    val expectedMessage =
      """One or more error nodes in result:
        |  [3]: One or more errors processing directive 'invalid': No block directive registered with name: invalid
        |
        |    @:invalid
        |    ^""".stripMargin
    run(input, expectedMessage)
  }

  test("report an invalid span directive indented inside a quoted block") {
    val input =
      """
        |> line 1
        |> Some @:invalid content
        |line 2
      """.stripMargin

    val expectedMessage =
      """One or more error nodes in result:
        |  [3]: One or more errors processing directive 'invalid': No span directive registered with name: invalid
        |
        |  > Some @:invalid content
        |         ^""".stripMargin
    run(input, expectedMessage)
  }

  test("report an invalid span directive not indented inside a quoted block") {
    val input =
      """
        |> line 1
        |Some @:invalid content
        |line 2
      """.stripMargin

    val expectedMessage =
      """One or more error nodes in result:
        |  [3]: One or more errors processing directive 'invalid': No span directive registered with name: invalid
        |
        |  Some @:invalid content
        |       ^""".stripMargin
    run(input, expectedMessage)
  }

  test("report an invalid span directive inside a table cell") {
    val input =
      """|| AAA | BBB |
         || --- | --- |
         || CCC | @:inv |
         |  EEE | FFF |
      """.stripMargin

    val expectedMessage =
      """One or more error nodes in result:
        |  [3]: One or more errors processing directive 'inv': No span directive registered with name: inv
        |
        |  | CCC | @:inv |
        |          ^""".stripMargin
    run(input, expectedMessage)
  }

  test("format multiple errors correctly") {
    val input =
      """
        |Some @:invalid content.
        |
        |Another @:broken directive.
      """.stripMargin

    val expectedMessage =
      """One or more error nodes in result:
        |  [2]: One or more errors processing directive 'invalid': No span directive registered with name: invalid
        |
        |  Some @:invalid content.
        |       ^
        |
        |  [4]: One or more errors processing directive 'broken': No span directive registered with name: broken
        |
        |  Another @:broken directive.
        |          ^""".stripMargin
    run(input, expectedMessage)
  }

}
