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

import laika.ast._
import laika.ast.sample.ParagraphCompanionShortcuts
import laika.format._
import munit.FunSuite

class RenderAPISpec extends FunSuite
    with ParagraphCompanionShortcuts {

  val rootElem = RootElement(p("aaö"), p("bbb"))

  val expected = """RootElement - Blocks: 2
                   |. Paragraph - Spans: 1
                   |. . Text - 'aaö'
                   |. Paragraph - Spans: 1
                   |. . Text - 'bbb'""".stripMargin

  test("document to string") {
    assertEquals(Renderer.of(AST).build.render(rootElem), Right(expected))
  }

  test("override default renderer for specific element types") {
    val renderer       = Renderer.of(AST).rendering { case (_, Text(content, _)) =>
      s"String - '$content'"
    }.build
    val modifiedResult = expected.replace("Text", "String")
    assertEquals(renderer.render(rootElem), Right(modifiedResult))
  }

}
