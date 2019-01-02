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

import laika.ast._
import laika.ast.helper.ModelBuilder
import org.scalatest.{FlatSpec, Matchers}

class OPFRendererSpec extends FlatSpec with Matchers with ModelBuilder {

  val renderer = new OPFRenderer

  val timestamp = "2018-01-01T12:00:00Z"
  val instant = Instant.parse(timestamp)


  "The OPF Renderer" should "render an empty tree" in new InputTreeBuilder {
    renderer.render(tree(Path.Root, 1), uuid, instant) shouldBe fileContent("", "", uuid)
  }

  it should "render a tree with a single document" in new SingleDocument {
    val manifestItems =
      """    <item id="foo_xhtml" href="content/foo.xhtml" media-type="application/xhtml+xml" />"""
    val spineRefs =
      """    <itemref idref="foo_xhtml" />"""
    renderer.render(input, uuid, instant) shouldBe fileContent(manifestItems, spineRefs, uuid)
  }

  it should "render a tree with a two documents" in new TwoDocuments {
    val manifestItems =
      """    <item id="foo_xhtml" href="content/foo.xhtml" media-type="application/xhtml+xml" />
        |    <item id="bar_xhtml" href="content/bar.xhtml" media-type="application/xhtml+xml" />""".stripMargin
    val spineRefs =
      """    <itemref idref="foo_xhtml" />
        |    <itemref idref="bar_xhtml" />"""
    renderer.render(input, uuid, instant) shouldBe fileContent(manifestItems, spineRefs, uuid)
  }

  it should "render a tree with a nested tree" in new NestedTree {
    val manifestItems =
      """    <item id="foo_xhtml" href="content/foo.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub_bar_xhtml" href="content/sub/bar.xhtml" media-type="application/xhtml+xml" />""".stripMargin
    val spineRefs =
      """    <itemref idref="foo_xhtml" />
        |    <itemref idref="sub_bar_xhtml" />"""
    renderer.render(input, uuid, instant) shouldBe fileContent(manifestItems, spineRefs, uuid)
  }

  it should "render a tree with two nested trees" in new TwoNestedTrees {
    val manifestItems =
      """    <item id="foo_xhtml" href="content/foo.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub1_bar_xhtml" href="content/sub1/bar.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub1_baz_xhtml" href="content/sub1/baz.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub2_bar_xhtml" href="content/sub2/bar.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub2_baz_xhtml" href="content/sub2/baz.xhtml" media-type="application/xhtml+xml" />"""
    val spineRefs =
      """    <itemref idref="foo_xhtml" />
        |    <itemref idref="sub1_bar_xhtml" />
        |    <itemref idref="sub1_baz_xhtml" />
        |    <itemref idref="sub2_bar_xhtml" />
        |    <itemref idref="sub2_baz_xhtml" />"""
    renderer.render(input, uuid, instant) shouldBe fileContent(manifestItems, spineRefs, uuid)
  }

  it should "render a tree with a nested tree and static documents" in new TreeWithStaticDocuments {
    val manifestItems =
      """    <item id="foo_xhtml" href="content/foo.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub_bar_xhtml" href="content/sub/bar.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub_image_jpg" href="content/sub/image.jpg" media-type="image/jpeg" />
        |    <item id="sub_styles_css" href="content/sub/styles.css" media-type="text/css" />""".stripMargin
    val spineRefs =
      """    <itemref idref="foo_xhtml" />
        |    <itemref idref="sub_bar_xhtml" />"""
    renderer.render(input, uuid, instant) shouldBe fileContent(manifestItems, spineRefs, uuid)
  }

  def fileContent (manifestItems: String, spineRefs: String, uuid: String): String =
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
