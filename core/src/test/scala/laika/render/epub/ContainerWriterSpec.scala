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

import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}
import laika.ast._
import laika.ast.helper.ModelBuilder
import laika.format.EPUB
import laika.io.Input
import laika.io.OutputTree.{ResultTree, StringResult}
import org.scalatest.{FlatSpec, Matchers}


class ContainerWriterSpec extends FlatSpec with Matchers with ModelBuilder {


  val writer = new ContainerWriter(EPUB.Config.default)

  val standardFiles = Seq(
    "/mimetype",
    "/META-INF/container.xml",
    "/META-INF/com.apple.ibooks.display-options.xml",
    "/EPUB/content.opf",
    "/EPUB/nav.xhtml",
    "/EPUB/toc.ncx"
  )

  val uuid = "some-uuid"

  def rootElem(num: Int) = root(title(s"Title $num"), p("zzz"))

  def section(letter: Char) = Section(Header(1, Seq(Text(s"Section $letter")), Id(letter.toString)), Seq(p("zzz")))

  def rootElemWithSections(num: Int) = root(title(s"Title $num"), section('A'), section('B'))

  def configWithTreeTitle (num: Int): Config = ConfigFactory.empty
    .withValue("title", ConfigValueFactory.fromAnyRef(s"Tree $num"))

  def tree (path: Path, titleNum: Int, docs: TreeContent*): DocumentTree =
    DocumentTree(path, docs, config = configWithTreeTitle(titleNum))

  def collectInputs (tree: DocumentTree): Seq[String] = {

    def toStringResult (doc: Document): StringResult =
      StringResult(doc.path.parent / (doc.path.basename + ".html"), "Content is irrelevant for this test")

    def toResultTree (currentTree: DocumentTree): ResultTree = {
      val docs = currentTree.content.collect { case doc: Document => toStringResult(doc) }
      val subTrees = currentTree.content.collect { case childTree: DocumentTree => toResultTree(childTree) }
      ResultTree(currentTree.path, docs, subTrees)
    }

    writer.collectInputs(tree, toResultTree(tree)).map(_.path.toString)
  }


  "The ContainerWriter" should "collect a single target document" in {
    val doc = Document(Path.Root / "foo", rootElem(2))
    collectInputs(tree(Path.Root, 1, doc)) shouldBe standardFiles :+ "/EPUB/content/foo.xhtml"
  }

  it should "render a tree with a two documents" in {
    val doc1 = Document(Path.Root / "foo", rootElem(2))
    val doc2 = Document(Path.Root / "bar", rootElem(3))
    val result = Seq(
      "/EPUB/content/foo.xhtml",
      "/EPUB/content/bar.xhtml"
    )
    collectInputs(tree(Path.Root, 1, doc1, doc2)) shouldBe standardFiles ++ result
  }

  it should "render a tree with a nested tree" in {
    val doc1 = Document(Path.Root / "foo", rootElem(2))
    val doc2 = Document(Path.Root / "sub" / "bar", rootElem(3))
    val subtree = tree(Path.Root / "sub", 4, doc2)
    val result = Seq(
      "/EPUB/content/foo.xhtml",
      "/EPUB/content/sub/bar.xhtml"
    )
    collectInputs(tree(Path.Root, 1, doc1, subtree)) shouldBe standardFiles ++ result
  }

  it should "render a tree with two nested trees" in {
    val doc1 = Document(Path.Root / "foo", rootElem(2))
    val doc2 = Document(Path.Root / "sub1" / "bar", rootElem(3))
    val doc3 = Document(Path.Root / "sub1" / "baz", rootElem(4))
    val doc4 = Document(Path.Root / "sub2" / "bar", rootElem(5))
    val doc5 = Document(Path.Root / "sub2" / "baz", rootElem(6))
    val subtree1 = tree(Path.Root / "sub1", 2, doc2, doc3)
    val subtree2 = tree(Path.Root / "sub2", 3, doc4, doc5)
    val result = Seq(
      "/EPUB/content/foo.xhtml",
      "/EPUB/content/sub1/bar.xhtml",
      "/EPUB/content/sub1/baz.xhtml",
      "/EPUB/content/sub2/bar.xhtml",
      "/EPUB/content/sub2/baz.xhtml"
    )
    collectInputs(tree(Path.Root, 1, doc1, subtree1, subtree2)) shouldBe standardFiles ++ result
  }

  it should "render a tree with a nested tree and static documents" in {
    val doc1 = Document(Path.Root / "foo", rootElem(2))
    val doc2 = Document(Path.Root / "sub" / "bar", rootElem(3))
    val static1 = StaticDocument(Input.fromString("", Path("/sub/image.jpg")))
    val static2 = StaticDocument(Input.fromString("", Path("/sub/styles.css")))
    val unknown = StaticDocument(Input.fromString("", Path("/sub/doc.pdf")))
    val subtree = tree(Path.Root / "sub", 4, doc2).copy(additionalContent = Seq(static1, static2, unknown))
    val result = Seq(
      "/EPUB/content/foo.xhtml",
      "/EPUB/content/sub/bar.xhtml",
      "/EPUB/content/sub/image.jpg",
      "/EPUB/content/sub/styles.css"
    )
    collectInputs(tree(Path.Root, 1, doc1, subtree)) shouldBe standardFiles ++ result
  }

}
