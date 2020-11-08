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

import java.time.Instant
import java.util.{Date, Locale}

import cats.effect.IO
import laika.ast.Path.Root
import laika.ast._
import laika.format.EPUB
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OPFRendererSpec extends AnyFlatSpec with Matchers {

  val renderer = new OPFRenderer
  
  val title = "Tree 1"
  val timestamp = "2018-01-01T12:00:00Z"
  val instant = Date.from(Instant.parse(timestamp))
  val identifier = s"urn:uuid:${new InputTreeBuilder{}.uuid}"
  val config: EPUB.BookConfig = EPUB.BookConfig(metadata = DocumentMetadata(
    identifier = Some(identifier),
    date = Some(instant),
    language = Some(Locale.UK.toLanguageTag),
    authors = Seq("Mia Miller")
  ))

  case class CoverEntries (metadata: String, spine: String, guide: String)


  "The OPF Renderer" should "render an empty tree" in new InputTreeBuilder {
    renderer.render(rootTree(Path.Root, 1), title, config) shouldBe fileContent("", "", "", uuid)
  }

  it should "render a tree with a single document" in new SingleDocument {
    val manifestItems =
      """    <item id="foo_epub_xhtml" href="content/foo.epub.xhtml" media-type="application/xhtml+xml" />"""
    val spineRefs =
      """    <itemref idref="foo_epub_xhtml" />"""
    renderer.render[IO](input, title, config) shouldBe fileContent(manifestItems, "", spineRefs, uuid)
  }

  it should "render a tree with a single document with the default locale rendered correctly" in new SingleDocument {
    val manifestItems =
      """    <item id="foo_epub_xhtml" href="content/foo.epub.xhtml" media-type="application/xhtml+xml" />"""
    val spineRefs =
      """    <itemref idref="foo_epub_xhtml" />"""
    val configWithoutLang = config.copy(metadata = config.metadata.copy(language = None))
    renderer.render[IO](input, title, configWithoutLang) shouldBe fileContent(manifestItems, "", spineRefs, uuid, language = Locale.getDefault.toLanguageTag)
  }

  it should "render a tree with a single document with valid XML id for the name starting with a digit" in new InputTreeBuilder {
    val docRef = doc(Path.Root / "01-foo", 2)
    val input = rootTree(Path.Root, 1, docRef)
    val manifestItems =
      """    <item id="_01-foo_epub_xhtml" href="content/01-foo.epub.xhtml" media-type="application/xhtml+xml" />"""
    val spineRefs =
      """    <itemref idref="_01-foo_epub_xhtml" />"""
    val configWithoutLang = config.copy(metadata = config.metadata.copy(language = None))
    renderer.render[IO](input, title, configWithoutLang) shouldBe fileContent(manifestItems, "", spineRefs, uuid, language = Locale.getDefault.toLanguageTag)
  }

  it should "render a tree with a two documents" in new TwoDocuments {
    val manifestItems =
      """    <item id="foo_epub_xhtml" href="content/foo.epub.xhtml" media-type="application/xhtml+xml" />
        |    <item id="bar_epub_xhtml" href="content/bar.epub.xhtml" media-type="application/xhtml+xml" />""".stripMargin
    val spineRefs =
      """    <itemref idref="foo_epub_xhtml" />
        |    <itemref idref="bar_epub_xhtml" />"""
    renderer.render(input, title, config) shouldBe fileContent(manifestItems, "", spineRefs, uuid)
  }

  it should "render a tree with a title document" in new DocumentPlusTitle {
    val manifestItems =
      """    <item id="title_epub_xhtml" href="content/title.epub.xhtml" media-type="application/xhtml+xml" />
        |    <item id="bar_epub_xhtml" href="content/bar.epub.xhtml" media-type="application/xhtml+xml" />""".stripMargin
    val titleRef = """    <itemref idref="title_epub_xhtml" />"""
    val spineRefs =
      """    <itemref idref="bar_epub_xhtml" />"""
    renderer.render(input, "From TitleDoc", config) shouldBe fileContent(manifestItems, titleRef, spineRefs, uuid, "From TitleDoc")
  }

  it should "render a tree with a cover" in new DocumentPlusCover {
    val manifestItems =
      """    <item id="cover_epub_xhtml" href="content/cover.epub.xhtml" media-type="application/xhtml+xml" />
        |    <item id="foo_epub_xhtml" href="content/foo.epub.xhtml" media-type="application/xhtml+xml" />
        |    <item id="bar_epub_xhtml" href="content/bar.epub.xhtml" media-type="application/xhtml+xml" />
        |    <item id="cover_png" href="content/cover.png" media-type="image/png" />""".stripMargin
    val coverEntries = CoverEntries(
      metadata = """    <meta name="cover" content="cover_png" />""",
      spine =    """    <itemref idref="cover_epub_xhtml" />""",
      guide =    """    <reference type="cover" title="Cover" href="content/cover.epub.xhtml" />"""
    )
    val spineRefs =
      """    <itemref idref="foo_epub_xhtml" />
        |    <itemref idref="bar_epub_xhtml" />""".stripMargin
    renderer.render(input, title, config.copy(coverImage = Some(Root / "cover.png"))) shouldBe fileContent(manifestItems, "", spineRefs, uuid, coverEntries = Some(coverEntries))
  }

  it should "render a tree with a nested tree" in new NestedTree {
    val manifestItems =
      """    <item id="foo_epub_xhtml" href="content/foo.epub.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub_bar_epub_xhtml" href="content/sub/bar.epub.xhtml" media-type="application/xhtml+xml" />""".stripMargin
    val spineRefs =
      """    <itemref idref="foo_epub_xhtml" />
        |    <itemref idref="sub_bar_epub_xhtml" />"""
    renderer.render(input, title, config) shouldBe fileContent(manifestItems, "", spineRefs, uuid)
  }

  it should "render a tree with two nested trees" in new TwoNestedTrees {
    val manifestItems =
      """    <item id="foo_epub_xhtml" href="content/foo.epub.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub1_bar_epub_xhtml" href="content/sub1/bar.epub.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub1_baz_epub_xhtml" href="content/sub1/baz.epub.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub2_bar_epub_xhtml" href="content/sub2/bar.epub.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub2_baz_epub_xhtml" href="content/sub2/baz.epub.xhtml" media-type="application/xhtml+xml" />"""
    val spineRefs =
      """    <itemref idref="foo_epub_xhtml" />
        |    <itemref idref="sub1_bar_epub_xhtml" />
        |    <itemref idref="sub1_baz_epub_xhtml" />
        |    <itemref idref="sub2_bar_epub_xhtml" />
        |    <itemref idref="sub2_baz_epub_xhtml" />"""
    renderer.render(input, title, config) shouldBe fileContent(manifestItems, "", spineRefs, uuid)
  }

  it should "render a tree with a nested tree and static documents" in new TreeWithStaticDocuments {
    val manifestItems =
      """    <item id="foo_epub_xhtml" href="content/foo.epub.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub_bar_epub_xhtml" href="content/sub/bar.epub.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub_image_jpg" href="content/sub/image.jpg" media-type="image/jpeg" />
        |    <item id="sub_styles_epub_css" href="content/sub/styles.epub.css" media-type="text/css" />""".stripMargin
    val spineRefs =
      """    <itemref idref="foo_epub_xhtml" />
        |    <itemref idref="sub_bar_epub_xhtml" />"""
    renderer.render(input, title, config) shouldBe fileContent(manifestItems, "", spineRefs, uuid)
  }

  def fileContent (manifestItems: String, 
                   titleRef: String, 
                   spineRefs: String, 
                   uuid: String, 
                   title: String = "Tree 1",
                   language: String = "en-GB",
                   coverEntries: Option[CoverEntries] = None): String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<package
       |    version="3.0"
       |    xmlns="http://www.idpf.org/2007/opf"
       |    unique-identifier="epub-id-1"
       |    prefix="ibooks: http://vocabulary.itunes.apple.com/rdf/ibooks/vocabulary-extensions-1.0/">
       |  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:opf="http://www.idpf.org/2007/opf">
       |    <dc:identifier id="epub-id-1">urn:uuid:$uuid</dc:identifier>
       |    <dc:title>$title</dc:title>
       |    <dc:date id="epub-date">$timestamp</dc:date>
       |    <dc:language>$language</dc:language>
       |    <dc:creator>Mia Miller</dc:creator>
       |${coverEntries.fold("")(_.metadata)}
       |    <meta property="dcterms:modified">$timestamp</meta>
       |  </metadata>
       |  <manifest>
       |    <item id="ncx" href="toc.ncx" media-type="application/x-dtbncx+xml" />
       |    <item id="nav" href="nav.xhtml" media-type="application/xhtml+xml" properties="nav" />
       |$manifestItems
       |  </manifest>
       |  <spine toc="ncx">
       |${coverEntries.fold("")(_.spine)}
       |$titleRef
       |$spineRefs
       |  </spine>
       |  <guide>
       |    <reference type="toc" title="Table of Content" href="nav.xhtml" />
       |${coverEntries.fold("")(_.guide)}
       |  </guide>
       |</package>
    """.stripMargin.replaceAll("[\n]+", "\n")

}
