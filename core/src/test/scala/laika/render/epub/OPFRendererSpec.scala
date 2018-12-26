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

import java.time.Instant

import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}
import laika.ast._
import laika.ast.helper.ModelBuilder
import org.scalatest.{FlatSpec, Matchers}

class OPFRendererSpec extends FlatSpec with Matchers with ModelBuilder {

  val renderer = new OPFRenderer

  val uuid = "some-uuid"

  val timestamp = "2018-01-01T12:00:00Z"
  val instant = Instant.parse(timestamp)

  def rootElem(num: Int) = root(title(s"Title $num"), p("zzz"))

  def section(letter: Char) = Section(Header(1, Seq(Text(s"Section $letter")), Id(letter.toString)), Seq(p("zzz")))

  def configWithTreeTitle (num: Int): Config = ConfigFactory.empty
    .withValue("title", ConfigValueFactory.fromAnyRef(s"Tree $num"))

  def tree (path: Path, titleNum: Int, docs: TreeContent*): DocumentTree =
    DocumentTree(path, docs, config = configWithTreeTitle(titleNum))

  "The OPF Renderer" should "render an empty tree" in {
    renderer.render(tree(Path.Root, 1), uuid, instant) shouldBe fileContent("", "")
  }

  it should "render a tree with a single document" in {
    val doc = Document(Path.Root / "foo", rootElem(2))
    val manifestItems =
      """    <item id="foo_xhtml" href="text/foo.xhtml" media-type="application/xhtml+xml" />"""
    val spineRefs =
      """    <itemref idref="foo_xhtml" />"""
    renderer.render(tree(Path.Root, 1, doc), uuid, instant) shouldBe fileContent(manifestItems, spineRefs)
  }

  it should "render a tree with a two documents" in {
    val doc1 = Document(Path.Root / "foo", rootElem(2))
    val doc2 = Document(Path.Root / "bar", rootElem(3))
    val manifestItems =
      """    <item id="foo_xhtml" href="text/foo.xhtml" media-type="application/xhtml+xml" />
        |    <item id="bar_xhtml" href="text/bar.xhtml" media-type="application/xhtml+xml" />""".stripMargin
    val spineRefs =
      """    <itemref idref="foo_xhtml" />
        |    <itemref idref="bar_xhtml" />"""
    renderer.render(tree(Path.Root, 1, doc1, doc2), uuid, instant) shouldBe fileContent(manifestItems, spineRefs)
  }

  it should "render a tree with a nested tree" in {
    val doc1 = Document(Path.Root / "foo", rootElem(2))
    val doc2 = Document(Path.Root / "sub" / "bar", rootElem(3))
    val subtree = tree(Path.Root / "sub", 4, doc2)
    val manifestItems =
      """    <item id="foo_xhtml" href="text/foo.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub_bar_xhtml" href="text/sub/bar.xhtml" media-type="application/xhtml+xml" />""".stripMargin
    val spineRefs =
      """    <itemref idref="foo_xhtml" />
        |    <itemref idref="sub_bar_xhtml" />"""
    renderer.render(tree(Path.Root, 1, doc1, subtree), uuid, instant) shouldBe fileContent(manifestItems, spineRefs)
  }

  it should "render a tree with two nested trees" in {
    val doc1 = Document(Path.Root / "foo", rootElem(2))
    val doc2 = Document(Path.Root / "sub1" / "bar", rootElem(3))
    val doc3 = Document(Path.Root / "sub1" / "baz", rootElem(4))
    val doc4 = Document(Path.Root / "sub2" / "bar", rootElem(5))
    val doc5 = Document(Path.Root / "sub2" / "baz", rootElem(6))
    val subtree1 = tree(Path.Root / "sub1", 2, doc2, doc3)
    val subtree2 = tree(Path.Root / "sub2", 3, doc4, doc5)
    val manifestItems =
      """    <item id="foo_xhtml" href="text/foo.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub1_bar_xhtml" href="text/sub1/bar.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub1_baz_xhtml" href="text/sub1/baz.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub2_bar_xhtml" href="text/sub2/bar.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub2_baz_xhtml" href="text/sub2/baz.xhtml" media-type="application/xhtml+xml" />"""
    val spineRefs =
      """    <itemref idref="foo_xhtml" />
        |    <itemref idref="sub1_bar_xhtml" />
        |    <itemref idref="sub1_baz_xhtml" />
        |    <itemref idref="sub2_bar_xhtml" />
        |    <itemref idref="sub2_baz_xhtml" />"""
    renderer.render(tree(Path.Root, 1, doc1, subtree1, subtree2), uuid, instant) shouldBe fileContent(manifestItems, spineRefs)
  }

  def fileContent (manifestItems: String, spineRefs: String): String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<package
       |    version="3.0"
       |    xmlns="http://www.idpf.org/2007/opf"
       |    unique-identifier="epub-id-1"
       |    prefix="ibooks: http://vocabulary.itunes.apple.com/rdf/ibooks/vocabulary-extensions-1.0/">
       |  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:opf="http://www.idpf.org/2007/opf">
       |    <dc:identifier id="epub-id-1">urn:uuid:$uuid</dc:identifier>
       |    <dc:title>Tree 1</dc:title>
       |    <dc:date id="epub-date">$timestamp</dc:date>
       |    <dc:language>en-GB</dc:language>
       |    <meta property="dcterms:modified">$timestamp</meta>
       |  </metadata>
       |  <manifest>
       |    <item id="ncx" href="toc.ncx" media-type="application/x-dtbncx+xml" />
       |    <item id="nav" href="nav.xhtml" media-type="application/xhtml+xml" properties="nav" />
       |    <item id="style" href="styles/stylesheet1.css" media-type="text/css" />
       |$manifestItems
       |  </manifest>
       |  <spine toc="ncx">
       |$spineRefs
       |  </spine>
       |  <guide>
       |    <reference type="toc" title="Table of Content" href="nav.xhtml" />
       |  </guide>
       |</package>
    """.stripMargin

}
