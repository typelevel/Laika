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

import java.time.{Instant, OffsetDateTime}
import java.util.{Date, Locale}
import cats.effect.IO
import laika.ast.Path.Root
import laika.ast._
import laika.config.ConfigBuilder
import laika.format.EPUB
import laika.format.EPUB.ScriptedTemplate
import laika.io.model.RenderedTreeRoot
import munit.FunSuite

class OPFRendererSpec extends FunSuite {

  val renderer = new OPFRenderer
  
  val title = "Tree 1"
  val uuid = "some-uuid"
  val timestamp = "2018-01-01T12:00:00Z"
  val instant: OffsetDateTime = OffsetDateTime.parse(timestamp)
  val identifier = s"urn:uuid:${new InputTreeBuilder{}.uuid}"
  val config: EPUB.BookConfig = EPUB.BookConfig(metadata = DocumentMetadata(
    identifier = Some(identifier),
    datePublished = Some(instant),
    language = Some(Locale.UK.toLanguageTag),
    authors = Seq("Mia Miller")
  ))

  case class CoverEntries (metadata: String, spine: String, guide: String)

  object TreeWithScriptedDocuments extends InputTreeBuilder {

    private val doc1 = doc(Path.Root / "foo", 2, config = ConfigBuilder.empty.withValue[ScriptedTemplate](ScriptedTemplate.Always).build)
    private val doc2 = doc(Path.Root / "sub" / "bar", 3, config = ConfigBuilder.empty.withValue[ScriptedTemplate](ScriptedTemplate.Auto).build)
    private val static1 = ByteInput("", Path.parse("/sub/code.shared.js"))
    private val static2 = ByteInput("", Path.parse("/sub/code.epub.js"))
    private val static3 = ByteInput("", Path.parse("/sub/styles.epub.css"))
    private val subtree = tree(Path.Root / "sub", 4, doc2)

    def input (hasJS: Boolean): RenderedTreeRoot[IO] = rootTree(Path.Root, 1, doc1, subtree)
      .copy[IO](staticDocuments = if (hasJS) Seq(static1, static2, static3) else Seq(static3))
  }
  
  def run (input: RenderedTreeRoot[IO], expected: String)(implicit loc: munit.Location): Unit = runWith(input, this.config, expected)
  
  def runWith (input: RenderedTreeRoot[IO], config: EPUB.BookConfig, expected: String)(implicit loc: munit.Location): Unit = {
    val actual = renderer.render(input, title, config)
    assertEquals(actual, expected)
  }
  

  test("render an empty tree") {
    run(EmptyTree.input, fileContent("", ""))
  }

  test("render a tree with a single document") {
    val manifestItems =
      """    <item id="foo_epub_xhtml" href="content/foo.epub.xhtml" media-type="application/xhtml+xml" />"""
    val spineRefs =
      """    <itemref idref="foo_epub_xhtml" />"""
    run(SingleDocument.input, fileContent(manifestItems, spineRefs))
  }

  test("render a tree with a single document with the default locale rendered correctly") {
    val manifestItems =
      """    <item id="foo_epub_xhtml" href="content/foo.epub.xhtml" media-type="application/xhtml+xml" />"""
    val spineRefs =
      """    <itemref idref="foo_epub_xhtml" />"""
    val configWithoutLang = config.copy(metadata = config.metadata.copy(language = None))
    val expected = fileContent(manifestItems, spineRefs, language = Locale.getDefault.toLanguageTag)
    runWith(SingleDocument.input, configWithoutLang, expected)
  }

  test("render a tree with a single document with valid XML id for the name starting with a digit") {
    val manifestItems =
      """    <item id="_01-foo_epub_xhtml" href="content/01-foo.epub.xhtml" media-type="application/xhtml+xml" />"""
    val spineRefs =
      """    <itemref idref="_01-foo_epub_xhtml" />"""
    val configWithoutLang = config.copy(metadata = config.metadata.copy(language = None))
    val expected = fileContent(manifestItems, spineRefs, language = Locale.getDefault.toLanguageTag)
    runWith(DocumentNameStartingWithDigit.input, configWithoutLang, expected)
  }

  test("render a tree with two documents") {
    val manifestItems =
      """    <item id="foo_epub_xhtml" href="content/foo.epub.xhtml" media-type="application/xhtml+xml" />
        |    <item id="bar_epub_xhtml" href="content/bar.epub.xhtml" media-type="application/xhtml+xml" />""".stripMargin
    val spineRefs =
      """    <itemref idref="foo_epub_xhtml" />
        |    <itemref idref="bar_epub_xhtml" />"""
    run(TwoDocuments.input, fileContent(manifestItems, spineRefs))
  }

  test("render a tree with a title document") {
    val manifestItems =
      """    <item id="title_epub_xhtml" href="content/title.epub.xhtml" media-type="application/xhtml+xml" />
        |    <item id="bar_epub_xhtml" href="content/bar.epub.xhtml" media-type="application/xhtml+xml" />""".stripMargin
    val titleRef = """    <itemref idref="title_epub_xhtml" />"""
    val spineRefs =
      """    <itemref idref="bar_epub_xhtml" />"""
    val expected = fileContent(manifestItems, spineRefs, titleRef = titleRef, title = "From TitleDoc")
    val actual = renderer.render(DocumentPlusTitle.input, "From TitleDoc", config)
    assertEquals(actual, expected)
  }

  test("render a tree with a cover") {
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
    val expected = fileContent(manifestItems, spineRefs, coverEntries = Some(coverEntries))
    val coverConfig = config.copy(coverImage = Some(Root / "cover.png"))
    runWith(DocumentPlusCover.input, coverConfig, expected)
  }

  test("render a tree with a nested tree") {
    val manifestItems =
      """    <item id="foo_epub_xhtml" href="content/foo.epub.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub_bar_epub_xhtml" href="content/sub/bar.epub.xhtml" media-type="application/xhtml+xml" />""".stripMargin
    val spineRefs =
      """    <itemref idref="foo_epub_xhtml" />
        |    <itemref idref="sub_bar_epub_xhtml" />"""
    run(NestedTree.input, fileContent(manifestItems, spineRefs))
  }

  test("render a tree with two nested trees") {
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
    run(TwoNestedTrees.input, fileContent(manifestItems, spineRefs))
  }

  test("render a tree with a nested tree and static documents") {
    val manifestItems =
      """    <item id="foo_epub_xhtml" href="content/foo.epub.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub_bar_epub_xhtml" href="content/sub/bar.epub.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub_image-1_5x_jpg" href="content/sub/image-1.5x.jpg" media-type="image/jpeg" />
        |    <item id="sub_styles_epub_css" href="content/sub/styles.epub.css" media-type="text/css" />""".stripMargin
    val spineRefs =
      """    <itemref idref="foo_epub_xhtml" />
        |    <itemref idref="sub_bar_epub_xhtml" />"""
    run(TreeWithStaticDocuments.input, fileContent(manifestItems, spineRefs))
  }

  test("render a tree with a nested tree and script documents") {
    val manifestItems =
      """    <item id="foo_epub_xhtml" href="content/foo.epub.xhtml" media-type="application/xhtml+xml" properties="scripted"/>
        |    <item id="sub_bar_epub_xhtml" href="content/sub/bar.epub.xhtml" media-type="application/xhtml+xml" properties="scripted"/>
        |    <item id="sub_code_shared_js" href="content/sub/code.shared.js" media-type="application/javascript" />
        |    <item id="sub_code_epub_js" href="content/sub/code.epub.js" media-type="application/javascript" />
        |    <item id="sub_styles_epub_css" href="content/sub/styles.epub.css" media-type="text/css" />""".stripMargin
    val spineRefs =
      """    <itemref idref="foo_epub_xhtml" />
        |    <itemref idref="sub_bar_epub_xhtml" />"""
    run(TreeWithScriptedDocuments.input(hasJS = true), fileContent(manifestItems, spineRefs))
  }

  test("render a tree with a nested tree and no script documents") {
    val manifestItems =
      """    <item id="foo_epub_xhtml" href="content/foo.epub.xhtml" media-type="application/xhtml+xml" properties="scripted"/>
        |    <item id="sub_bar_epub_xhtml" href="content/sub/bar.epub.xhtml" media-type="application/xhtml+xml" />
        |    <item id="sub_styles_epub_css" href="content/sub/styles.epub.css" media-type="text/css" />""".stripMargin
    val spineRefs =
      """    <itemref idref="foo_epub_xhtml" />
        |    <itemref idref="sub_bar_epub_xhtml" />"""
    run(TreeWithScriptedDocuments.input(hasJS = false), fileContent(manifestItems, spineRefs))
  }

  def fileContent (manifestItems: String, 
                   spineRefs: String, 
                   titleRef: String = "", 
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
