/*
 * Copyright 2013-2018 the original author or authors.
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

import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.format.EPUB
import laika.io.OutputTree.{ResultTree, StringResult}
import org.scalatest.{FlatSpec, Matchers}


class ContainerWriterSpec extends FlatSpec with Matchers with ModelBuilder {


  val writer = new ContainerWriter

  val standardFiles = Seq(
    "/mimetype",
    "/META-INF/container.xml",
    "/META-INF/com.apple.ibooks.display-options.xml",
    "/EPUB/content.opf",
    "/EPUB/nav.xhtml",
    "/EPUB/toc.ncx"
  )


  def collectInputs (tree: DocumentTree): Seq[String] = {

    def toStringResult (doc: Document): StringResult =
      StringResult(doc.path.parent / (doc.path.basename + ".html"), "Content is irrelevant for this test")

    def toResultTree (currentTree: DocumentTree): ResultTree = {
      val docs = currentTree.content.collect { case doc: Document => toStringResult(doc) }
      val subTrees = currentTree.content.collect { case childTree: DocumentTree => toResultTree(childTree) }
      ResultTree(currentTree.path, docs, subTrees)
    }

    writer.collectInputs(tree, EPUB.Config.default, toResultTree(tree)).map(_.path.toString)
  }


  "The ContainerWriter" should "collect a single target document" in new SingleDocument {
    collectInputs(input) shouldBe standardFiles :+ "/EPUB/content/foo.xhtml"
  }

  it should "render a tree with a two documents" in new TwoDocuments {
    val result = Seq(
      "/EPUB/content/foo.xhtml",
      "/EPUB/content/bar.xhtml"
    )
    collectInputs(input) shouldBe standardFiles ++ result
  }

  it should "render a tree with a nested tree" in new NestedTree {
    val result = Seq(
      "/EPUB/content/foo.xhtml",
      "/EPUB/content/sub/bar.xhtml"
    )
    collectInputs(input) shouldBe standardFiles ++ result
  }

  it should "render a tree with two nested trees" in new TwoNestedTrees {
    val result = Seq(
      "/EPUB/content/foo.xhtml",
      "/EPUB/content/sub1/bar.xhtml",
      "/EPUB/content/sub1/baz.xhtml",
      "/EPUB/content/sub2/bar.xhtml",
      "/EPUB/content/sub2/baz.xhtml"
    )
    collectInputs(input) shouldBe standardFiles ++ result
  }

  it should "render a tree with a nested tree and static documents" in new TreeWithStaticDocuments {
    val result = Seq(
      "/EPUB/content/foo.xhtml",
      "/EPUB/content/sub/bar.xhtml",
      "/EPUB/content/sub/image.jpg",
      "/EPUB/content/sub/styles.css"
    )
    collectInputs(input) shouldBe standardFiles ++ result
  }

}
