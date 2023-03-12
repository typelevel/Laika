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

package laika.rewrite

import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentTree
import laika.ast._
import laika.ast.sample.{ BuilderKey, SampleConfig, SampleTrees }
import laika.config.LaikaKeys
import laika.format.HTML
import laika.rewrite.nav.{
  ConfigurablePathTranslator,
  TargetFormats,
  TargetLookup,
  TranslatorConfig
}
import munit.FunSuite

class PathTranslatorSpec extends FunSuite {

  private val rootCursor = {

    val versions = Versions(Version("0.42.x", "0.42"), Nil)

    def doc2(key: BuilderKey): Seq[Block] = Seq(
      Header(1, "Title").withOptions(Id("ref")),
      Paragraph("text")
    )

    SampleTrees.sixDocuments
      .root.config(_.withValue(versions).withValue(LaikaKeys.siteBaseURL, "http://external.com/"))
      .root.config(SampleConfig.versioned(true))
      .doc1.config(SampleConfig.versioned(false))
      .doc2.config(SampleConfig.versioned(false))
      .static1.config(SampleConfig.versioned(false))
      .staticDoc(Root / "static-1" / "doc-7.txt")
      .staticDoc(Root / "static-2" / "doc-8.txt")
      .docContent(doc2 _)
      .suffix("md")
      .buildCursor
      .getOrElse(fail("unable to create cursor"))
  }

  val translatorConfig =
    TranslatorConfig.readFrom(rootCursor.config).getOrElse(TranslatorConfig.empty)

  val lookup = new TargetLookup(rootCursor)

  val versionedRef = ConfigurablePathTranslator(
    translatorConfig,
    OutputContext(HTML),
    Root / "tree-1" / "doc-3.md",
    lookup
  )

  val unversionedRef =
    ConfigurablePathTranslator(translatorConfig, OutputContext(HTML), Root / "doc-1.md", lookup)

  val epubRef = ConfigurablePathTranslator(
    translatorConfig,
    OutputContext("epub.xhtml", "epub"),
    Root / "tree-1" / "doc-3.md",
    lookup
  )

  test("between two unversioned documents") {
    val input    = ResolvedInternalTarget(Root / "doc-2.md", CurrentTree / "doc-2.md")
    val expected = ResolvedInternalTarget(Root / "doc-2.html", CurrentTree / "doc-2.html")
    assertEquals(unversionedRef.translate(input), expected)
  }

  test("between two versioned documents") {
    val input    =
      ResolvedInternalTarget(Root / "tree-2" / "doc-5.md", RelativePath.parse("../tree-2/doc-5.md"))
    val expected = ResolvedInternalTarget(
      Root / "0.42" / "tree-2" / "doc-5.html",
      RelativePath.parse("../tree-2/doc-5.html")
    )
    assertEquals(versionedRef.translate(input), expected)
  }

  test("versioned to unversioned document") {
    val input    = ResolvedInternalTarget(Root / "doc-2.md", RelativePath.parse("../doc-2.md"))
    val expected =
      ResolvedInternalTarget(Root / "doc-2.html", RelativePath.parse("../../doc-2.html"))
    assertEquals(versionedRef.translate(input), expected)
  }

  test("unversioned to versioned document") {
    val input    =
      ResolvedInternalTarget(Root / "tree-2" / "doc-5.md", RelativePath.parse("tree-2/doc-5.md"))
    val expected = ResolvedInternalTarget(
      Root / "0.42" / "tree-2" / "doc-5.html",
      RelativePath.parse("0.42/tree-2/doc-5.html")
    )
    assertEquals(unversionedRef.translate(input), expected)
  }

  test("static unversioned document") {
    val input    = ResolvedInternalTarget(
      Root / "static-1" / "doc-7.txt",
      RelativePath.parse("../static-1/doc-7.txt")
    )
    val expected = ResolvedInternalTarget(
      Root / "static-1" / "doc-7.txt",
      RelativePath.parse("../../static-1/doc-7.txt")
    )
    assertEquals(versionedRef.translate(input), expected)
  }

  test("static versioned document") {
    val input    = ResolvedInternalTarget(
      Root / "static-2" / "doc-8.txt",
      RelativePath.parse("../static-2/doc-8.txt")
    )
    val expected = ResolvedInternalTarget(
      Root / "0.42" / "static-2" / "doc-8.txt",
      RelativePath.parse("../static-2/doc-8.txt")
    )
    assertEquals(versionedRef.translate(input), expected)
  }

  test("ignore versions when output format is not HTML") {
    val input    =
      ResolvedInternalTarget(Root / "tree-2" / "doc-5.md", RelativePath.parse("../tree-2/doc-5.md"))
    val expected = ResolvedInternalTarget(
      Root / "tree-2" / "doc-5.epub.xhtml",
      RelativePath.parse("../tree-2/doc-5.epub.xhtml")
    )
    assertEquals(epubRef.translate(input), expected)
  }

  test("apply versions when substituting an internal target with an external one") {
    val input    = ResolvedInternalTarget(
      Root / "tree-2" / "doc-5.md",
      RelativePath.parse("../tree-2/doc-5.md"),
      TargetFormats.Selected("html")
    )
    val expected = ExternalTarget("http://external.com/0.42/tree-2/doc-5.html")
    assertEquals(epubRef.translate(input), expected)
  }

}
