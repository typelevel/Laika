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
import laika.format.EPUB
import laika.io.model.RenderedTreeRoot
import munit.FunSuite

class ContainerWriterSpec extends FunSuite {

  val writer = new ContainerWriter

  val standardFiles = Seq(
    "/mimetype",
    "/META-INF/container.xml",
    "/META-INF/com.apple.ibooks.display-options.xml",
    "/EPUB/content.opf",
    "/EPUB/nav.xhtml",
    "/EPUB/toc.ncx"
  )

  def collectInputs(renderResult: RenderedTreeRoot[IO]): Seq[String] =
    writer
      .collectInputs(renderResult, EPUB.BookConfig())
      .map(_.path.toString)

  test("collect a single target document") {
    assertEquals(collectInputs(SingleDocument.input), standardFiles :+ "/EPUB/content/foo.xhtml")
  }

  test("render a tree with a two documents") {
    val result = Seq(
      "/EPUB/content/foo.xhtml",
      "/EPUB/content/bar.xhtml"
    )
    assertEquals(collectInputs(TwoDocuments.input), standardFiles ++ result)
  }

  test("render a tree with a nested tree") {
    val result = Seq(
      "/EPUB/content/foo.xhtml",
      "/EPUB/content/sub/bar.xhtml"
    )
    assertEquals(collectInputs(NestedTree.input), standardFiles ++ result)
  }

  test("render a tree with two nested trees") {
    val result = Seq(
      "/EPUB/content/foo.xhtml",
      "/EPUB/content/sub1/bar.xhtml",
      "/EPUB/content/sub1/baz.xhtml",
      "/EPUB/content/sub2/bar.xhtml",
      "/EPUB/content/sub2/baz.xhtml"
    )
    assertEquals(collectInputs(TwoNestedTrees.input), standardFiles ++ result)
  }

  test("render a tree with a nested tree and static documents") {
    val result = Seq(
      "/EPUB/content/foo.xhtml",
      "/EPUB/content/sub/bar.xhtml",
      "/EPUB/content/sub/image-1.5x.jpg",
      "/EPUB/content/sub/styles.epub.css"
    )
    assertEquals(collectInputs(TreeWithStaticDocuments.input), standardFiles ++ result)
  }

}
