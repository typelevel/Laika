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
import org.scalatest.{FlatSpec, Matchers}

class NCXRendererSpec extends FlatSpec with Matchers with ModelBuilder {

  val renderer = new NCXRenderer

  val uuid = "some-uuid"

  def rootElem(num: Int) = root(title(s"Title $num"), p("zzz"))

  def section(letter: Char) = Section(Header(1, Seq(Text(s"Section $letter")), Id(letter.toString)), Seq(p("zzz")))

  def rootElemWithSections(num: Int) = root(title(s"Title $num"), section('A'), section('B'))

  def configWithTreeTitle (num: Int): Config = ConfigFactory.empty
    .withValue("title", ConfigValueFactory.fromAnyRef(s"Tree $num"))

  def tree (path: Path, titleNum: Int, docs: TreeContent*): DocumentTree =
    DocumentTree(Path.Root, docs, config = configWithTreeTitle(titleNum))

  "The NCX Renderer" should "render an empty tree" in {
    renderer.render(tree(Path.Root, 1), uuid, 1) shouldBe renderer.fileContent(uuid, "Tree 1", "", 1)
  }

  it should "render a tree with a single document" in {
    val doc = Document(Path.Root / "foo", rootElem(2))
    val result = """    <navPoint id="navPoint-0">
     |      <navLabel>
     |        <text>Title 2</text>
     |      </navLabel>
     |      <content src="text/foo.xhtml" />
     |
     |    </navPoint>""".stripMargin
    renderer.render(tree(Path.Root, 1, doc), uuid, 1) shouldBe renderer.fileContent(uuid, "Tree 1", result, 1)
  }

  it should "render a tree with a two documents" in {
    val doc1 = Document(Path.Root / "foo", rootElem(2))
    val doc2 = Document(Path.Root / "bar", rootElem(3))
    val result = """    <navPoint id="navPoint-0">
     |      <navLabel>
     |        <text>Title 2</text>
     |      </navLabel>
     |      <content src="text/foo.xhtml" />
     |
     |    </navPoint>
     |    <navPoint id="navPoint-1">
     |      <navLabel>
     |        <text>Title 3</text>
     |      </navLabel>
     |      <content src="text/bar.xhtml" />
     |
     |    </navPoint>""".stripMargin
    renderer.render(tree(Path.Root, 1, doc1, doc2), uuid, 1) shouldBe renderer.fileContent(uuid, "Tree 1", result, 1)
  }

  it should "render a tree with a nested tree" in {
    val doc1 = Document(Path.Root / "foo", rootElem(2))
    val doc2 = Document(Path.Root / "sub" / "bar", rootElem(3))
    val subtree = tree(Path.Root / "sub", 4, doc2)
    val result = """    <navPoint id="navPoint-0">
     |      <navLabel>
     |        <text>Title 2</text>
     |      </navLabel>
     |      <content src="text/foo.xhtml" />
     |
     |    </navPoint>
     |    <navPoint id="navPoint-1">
     |      <navLabel>
     |        <text>Tree 4</text>
     |      </navLabel>
     |      <content src="text/sub/bar.xhtml" />
     |    <navPoint id="navPoint-2">
     |      <navLabel>
     |        <text>Title 3</text>
     |      </navLabel>
     |      <content src="text/sub/bar.xhtml" />
     |
     |    </navPoint>
     |    </navPoint>""".stripMargin
    renderer.render(tree(Path.Root, 1, doc1, subtree), uuid, 2) shouldBe renderer.fileContent(uuid, "Tree 1", result, 2)
  }

  it should "not render a nested tree if the depth is 1" in {
    val doc1 = Document(Path.Root / "foo", rootElem(2))
    val doc2 = Document(Path.Root / "sub" / "bar", rootElem(3))
    val subtree = tree(Path.Root / "sub", 4, doc2)
    val result = """    <navPoint id="navPoint-0">
     |      <navLabel>
     |        <text>Title 2</text>
     |      </navLabel>
     |      <content src="text/foo.xhtml" />
     |
     |    </navPoint>""".stripMargin
    renderer.render(tree(Path.Root, 1, doc1, subtree), uuid, 1) shouldBe renderer.fileContent(uuid, "Tree 1", result, 1)
  }

  it should "render a document with sections when the depth is 2" in {
    val doc1 = Document(Path.Root / "foo", rootElemWithSections(2))
    val doc2 = Document(Path.Root / "bar", rootElemWithSections(3))
    val result = """    <navPoint id="navPoint-0">
     |      <navLabel>
     |        <text>Title 2</text>
     |      </navLabel>
     |      <content src="text/foo.xhtml" />
     |    <navPoint id="navPoint-1">
     |      <navLabel>
     |        <text>Section A</text>
     |      </navLabel>
     |      <content src="text/foo.xhtml#A" />
     |
     |    </navPoint>
     |    <navPoint id="navPoint-2">
     |      <navLabel>
     |        <text>Section B</text>
     |      </navLabel>
     |      <content src="text/foo.xhtml#B" />
     |
     |    </navPoint>
     |    </navPoint>
     |    <navPoint id="navPoint-3">
     |      <navLabel>
     |        <text>Title 3</text>
     |      </navLabel>
     |      <content src="text/bar.xhtml" />
     |    <navPoint id="navPoint-4">
     |      <navLabel>
     |        <text>Section A</text>
     |      </navLabel>
     |      <content src="text/bar.xhtml#A" />
     |
     |    </navPoint>
     |    <navPoint id="navPoint-5">
     |      <navLabel>
     |        <text>Section B</text>
     |      </navLabel>
     |      <content src="text/bar.xhtml#B" />
     |
     |    </navPoint>
     |    </navPoint>""".stripMargin
    renderer.render(tree(Path.Root, 1, doc1, doc2), uuid, 2) shouldBe renderer.fileContent(uuid, "Tree 1", result, 2)
  }

  it should "not render a document with sections when the depth is 1" in {
    val doc1 = Document(Path.Root / "foo", rootElemWithSections(2))
    val doc2 = Document(Path.Root / "bar", rootElemWithSections(3))
    val result = """    <navPoint id="navPoint-0">
     |      <navLabel>
     |        <text>Title 2</text>
     |      </navLabel>
     |      <content src="text/foo.xhtml" />
     |
     |    </navPoint>
     |    <navPoint id="navPoint-1">
     |      <navLabel>
     |        <text>Title 3</text>
     |      </navLabel>
     |      <content src="text/bar.xhtml" />
     |
     |    </navPoint>""".stripMargin
    renderer.render(tree(Path.Root, 1, doc1, doc2), uuid, 1) shouldBe renderer.fileContent(uuid, "Tree 1", result, 1)
  }

}
