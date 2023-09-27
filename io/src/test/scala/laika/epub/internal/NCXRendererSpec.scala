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

package laika.epub.internal

import cats.effect.IO
import laika.io.model.RenderedTreeRoot
import munit.FunSuite

class NCXRendererSpec extends FunSuite {

  val renderer = new NCXRenderer
  val title    = "Tree 1"
  val uuid     = "some-uuid"

  def render(input: RenderedTreeRoot[IO], depth: Int = 1): String =
    renderer.render(input, title, uuid, Some(depth))

  def result(navPoints: String, depth: Int = 1): String =
    renderer.fileContent(uuid, title, navPoints, depth)

  def run(input: RenderedTreeRoot[IO], expectedNavPoints: String, depth: Int = 1)(implicit
      loc: munit.Location
  ): Unit =
    assertEquals(render(input, depth), result(expectedNavPoints, depth))

  test("render an empty tree") {
    run(EmptyTree.input, "")
  }

  test("render a tree with a single document") {
    val expected =
      """    <navPoint id="navPoint-0">
        |      <navLabel>
        |        <text>Title 2</text>
        |      </navLabel>
        |      <content src="content/foo.xhtml" />
        |
        |    </navPoint>""".stripMargin
    run(SingleDocument.input, expected)
  }

  test("render a tree with a two documents") {
    val expected =
      """    <navPoint id="navPoint-0">
        |      <navLabel>
        |        <text>Title 2</text>
        |      </navLabel>
        |      <content src="content/foo.xhtml" />
        |
        |    </navPoint>
        |    <navPoint id="navPoint-1">
        |      <navLabel>
        |        <text>Title 3</text>
        |      </navLabel>
        |      <content src="content/bar.xhtml" />
        |
        |    </navPoint>""".stripMargin
    run(TwoDocuments.input, expected)
  }

  test("render a tree with a nested tree") {

    val expected =
      """    <navPoint id="navPoint-0">
        |      <navLabel>
        |        <text>Title 2</text>
        |      </navLabel>
        |      <content src="content/foo.xhtml" />
        |
        |    </navPoint>
        |    <navPoint id="navPoint-1">
        |      <navLabel>
        |        <text>Tree 4</text>
        |      </navLabel>
        |      <content src="content/sub/bar.xhtml" />
        |    <navPoint id="navPoint-2">
        |      <navLabel>
        |        <text>Title 3</text>
        |      </navLabel>
        |      <content src="content/sub/bar.xhtml" />
        |
        |    </navPoint>
        |    </navPoint>""".stripMargin
    run(NestedTree.input, expected, depth = 2)
  }

  test("not render a nested tree if the depth is 1") {
    val expected =
      """    <navPoint id="navPoint-0">
        |      <navLabel>
        |        <text>Title 2</text>
        |      </navLabel>
        |      <content src="content/foo.xhtml" />
        |
        |    </navPoint>""".stripMargin
    run(NestedTree.input, expected)
  }

  test("render a document with sections when the depth is 2") {
    val expected = """    <navPoint id="navPoint-0">
                     |      <navLabel>
                     |        <text>Title 2</text>
                     |      </navLabel>
                     |      <content src="content/foo.xhtml" />
                     |    <navPoint id="navPoint-1">
                     |      <navLabel>
                     |        <text>Section A</text>
                     |      </navLabel>
                     |      <content src="content/foo.xhtml#A" />
                     |
                     |    </navPoint>
                     |    <navPoint id="navPoint-2">
                     |      <navLabel>
                     |        <text>Section B</text>
                     |      </navLabel>
                     |      <content src="content/foo.xhtml#B" />
                     |
                     |    </navPoint>
                     |    </navPoint>
                     |    <navPoint id="navPoint-3">
                     |      <navLabel>
                     |        <text>Title 3</text>
                     |      </navLabel>
                     |      <content src="content/bar.xhtml" />
                     |    <navPoint id="navPoint-4">
                     |      <navLabel>
                     |        <text>Section A</text>
                     |      </navLabel>
                     |      <content src="content/bar.xhtml#A" />
                     |
                     |    </navPoint>
                     |    <navPoint id="navPoint-5">
                     |      <navLabel>
                     |        <text>Section B</text>
                     |      </navLabel>
                     |      <content src="content/bar.xhtml#B" />
                     |
                     |    </navPoint>
                     |    </navPoint>""".stripMargin
    run(DocumentsWithSections.input, expected, depth = 2)
  }

  test("not render a document with sections when the depth is 1") {
    val expected = """    <navPoint id="navPoint-0">
                     |      <navLabel>
                     |        <text>Title 2</text>
                     |      </navLabel>
                     |      <content src="content/foo.xhtml" />
                     |
                     |    </navPoint>
                     |    <navPoint id="navPoint-1">
                     |      <navLabel>
                     |        <text>Title 3</text>
                     |      </navLabel>
                     |      <content src="content/bar.xhtml" />
                     |
                     |    </navPoint>""".stripMargin
    run(DocumentsWithSections.input, expected)
  }

  test("escape special characters in titles") {
    val expected =
      """    <navPoint id="navPoint-0">
        |      <navLabel>
        |        <text>This &amp; That</text>
        |      </navLabel>
        |      <content src="content/foo.xhtml" />
        |
        |    </navPoint>""".stripMargin
    run(DocumentWithSpecialChars.input, expected)
  }

}
