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

package laika.render.epub

import cats.effect.IO
import laika.io.model.RenderedTreeRoot
import munit.FunSuite

class HTMLNavRendererSpec extends FunSuite {

  val renderer = new HtmlNavRenderer
  val title    = "Tree 1"

  def result(navItems: String): String = renderer.fileContent(title, "", navItems)

  def render(input: RenderedTreeRoot[IO], depth: Int = 1): String =
    renderer.render(input, title, Some(depth))

  def run(input: RenderedTreeRoot[IO], expectedNavItems: String, depth: Int = 1)(implicit
      loc: munit.Location
  ): Unit =
    assertEquals(render(input, depth), result(expectedNavItems))

  test("render an empty tree") {
    run(EmptyTree.input, "")
  }

  test("render a tree with a single document") {
    val expected =
      """      <ol class="toc">
        |        <li id="toc-li-0">
        |          <a href="content/foo.xhtml">Title 2</a>
        |
        |        </li>
        |      </ol>""".stripMargin
    run(SingleDocument.input, expected)
  }

  test("render a tree with two documents") {
    val expected =
      """      <ol class="toc">
        |        <li id="toc-li-0">
        |          <a href="content/foo.xhtml">Title 2</a>
        |
        |        </li>
        |        <li id="toc-li-1">
        |          <a href="content/bar.xhtml">Title 3</a>
        |
        |        </li>
        |      </ol>""".stripMargin
    run(TwoDocuments.input, expected)
  }

  test("render a tree with a single document and a CSS file") {
    val html     =
      """      <ol class="toc">
        |        <li id="toc-li-0">
        |          <a href="content/foo.xhtml">Title 2</a>
        |
        |        </li>
        |      </ol>""".stripMargin
    val cssLink  = """<link rel="stylesheet" type="text/css" href="content/test-style.css" />"""
    val expected = renderer.fileContent(title, cssLink, html)
    assertEquals(render(DocumentPlusStyle.input), expected)
  }

  test("render a tree with a title document") {
    val html     =
      """      <ol class="toc">
        |        <li id="toc-li-0">
        |          <a href="content/bar.xhtml">Title 3</a>
        |
        |        </li>
        |      </ol>""".stripMargin
    val title    = "From TitleDoc"
    val expected =
      renderer.fileContent(title, "", html)
    val actual   = renderer.render(DocumentPlusTitle.input, "From TitleDoc", Some(1))
    assertEquals(actual, expected)
  }

  test("render a tree with a cover image") {
    val html     =
      """      <ol class="toc">
        |        <li id="toc-li-0">
        |          <a href="content/foo.xhtml">Title 2</a>
        |        </li>
        |        <li id="toc-li-1">
        |          <a href="content/bar.xhtml">Title 3</a>
        |        </li>
        |      </ol>""".stripMargin
    val expected =
      renderer.fileContent(title, "", html)
    assertEquals(render(DocumentPlusCover.input), expected)
  }

  test("render a tree with a nested tree") {
    val expected =
      """      <ol class="toc">
        |        <li id="toc-li-0">
        |          <a href="content/foo.xhtml">Title 2</a>
        |
        |        </li>
        |        <li id="toc-li-1">
        |          <a href="content/sub/bar.xhtml">Tree 4</a>
        |      <ol class="toc">
        |        <li id="toc-li-2">
        |          <a href="content/sub/bar.xhtml">Title 3</a>
        |
        |        </li>
        |      </ol>
        |        </li>
        |      </ol>""".stripMargin
    run(NestedTree.input, expected, depth = 2)
  }

  test("render a tree with a nested tree with a title document") {
    val expected =
      """      <ol class="toc">
        |        <li id="toc-li-0">
        |          <a href="content/foo.xhtml">Title 2</a>
        |
        |        </li>
        |        <li id="toc-li-1">
        |          <a href="content/sub/title.xhtml">From TitleDoc</a>
        |      <ol class="toc">
        |        <li id="toc-li-2">
        |          <a href="content/sub/bar.xhtml">Title 3</a>
        |
        |        </li>
        |      </ol>
        |        </li>
        |      </ol>""".stripMargin
    run(NestedTreeWithTitleDoc.input, expected, depth = 2)
  }

  test("not render a nested tree if the depth is 1") {
    val expected =
      """      <ol class="toc">
        |        <li id="toc-li-0">
        |          <a href="content/foo.xhtml">Title 2</a>
        |
        |        </li>
        |      </ol>""".stripMargin
    run(NestedTree.input, expected)
  }

  test("render a document with sections when the depth is 2") {
    val expected =
      """      <ol class="toc">
        |        <li id="toc-li-0">
        |          <a href="content/foo.xhtml">Title 2</a>
        |      <ol class="toc">
        |        <li id="toc-li-1">
        |          <a href="content/foo.xhtml#A">Section A</a>
        |
        |        </li>
        |        <li id="toc-li-2">
        |          <a href="content/foo.xhtml#B">Section B</a>
        |
        |        </li>
        |      </ol>
        |        </li>
        |        <li id="toc-li-3">
        |          <a href="content/bar.xhtml">Title 3</a>
        |      <ol class="toc">
        |        <li id="toc-li-4">
        |          <a href="content/bar.xhtml#A">Section A</a>
        |
        |        </li>
        |        <li id="toc-li-5">
        |          <a href="content/bar.xhtml#B">Section B</a>
        |
        |        </li>
        |      </ol>
        |        </li>
        |      </ol>""".stripMargin
    run(DocumentsWithSections.input, expected, depth = 2)
  }

  test("not render a document with sections when the depth is 1") {
    val expected = """      <ol class="toc">
                     |        <li id="toc-li-0">
                     |          <a href="content/foo.xhtml">Title 2</a>
                     |
                     |        </li>
                     |        <li id="toc-li-1">
                     |          <a href="content/bar.xhtml">Title 3</a>
                     |
                     |        </li>
                     |      </ol>""".stripMargin
    run(DocumentsWithSections.input, expected)
  }

  test("escape special characters in titles") {
    val expected =
      """      <ol class="toc">
        |        <li id="toc-li-0">
        |          <a href="content/foo.xhtml">This &amp; That</a>
        |
        |        </li>
        |      </ol>""".stripMargin
    run(DocumentWithSpecialChars.input, expected)
  }

}
