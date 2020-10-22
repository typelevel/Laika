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
import laika.ast.helper.ModelBuilder
import laika.ast._
import laika.config.{ConfigBuilder, LaikaKeys}
import laika.rewrite.nav.{ConfigurablePathTranslator, TargetLookup}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers


class PathTranslatorSpec extends AnyFunSuite with Matchers with ModelBuilder {

  private val rootCursor = {

    val versions = Versions(Version("0.42.x", "0.42"), Nil)
    val rootConfig = ConfigBuilder.empty.withValue(versions).build
    val versionedConfig = ConfigBuilder.withFallback(rootConfig).withValue(LaikaKeys.versioned, true).build
    val versionedDocConfig = ConfigBuilder.withFallback(versionedConfig).build
    
    def doc (num: Int): Document = {
      val basePath = if (num < 3) Root else if (num < 5) Root / "tree-1" else Root / "tree-2"
      val config = if (num < 3) rootConfig else versionedDocConfig
      val blocks = Seq(
        Header(1, "Title").withOptions(Id("ref")),
        Paragraph("text")
      )
      Document(basePath / ("doc-" + num + ".md"), RootElement(blocks), config = config)
    }

    val tree1 = DocumentTree(Root / "tree-1", Seq(doc(3), doc(4)), config = versionedConfig)
    val tree2 = DocumentTree(Root / "tree-2", Seq(doc(5), doc(6)), config = versionedConfig)
    val tree3 = DocumentTree(Root / "tree-3", Nil)
    val tree4 = DocumentTree(Root / "tree-4", Nil, config = versionedConfig)

    val root = DocumentTreeRoot(
      DocumentTree(Root, Seq(doc(1), doc(2), tree1, tree2, tree3, tree4), config = rootConfig),
      staticDocuments = Seq(
        StaticDocument(Root / "tree-3" / "doc-7.txt"),
        StaticDocument(Root / "tree-4" / "doc-8.txt")
      )
    )

    RootCursor(root)
  }
  
  val versionedRef = ConfigurablePathTranslator(rootCursor.config, "html", "html", Root / "tree-1" / "doc-3.md", new TargetLookup(rootCursor))
  val unversionedRef = ConfigurablePathTranslator(rootCursor.config, "html", "html", Root / "doc-1.md", new TargetLookup(rootCursor))
  val epubRef = ConfigurablePathTranslator(rootCursor.config, "epub.xhtml", "epub", Root / "tree-1" / "doc-3.md", new TargetLookup(rootCursor))
  
  
  test("between two unversioned documents") {
    val input    = ResolvedInternalTarget(Root / "doc-2.md", CurrentTree / "doc-2.md")
    val expected = ResolvedInternalTarget(Root / "doc-2.html", CurrentTree / "doc-2.html")
    unversionedRef.translate(input) shouldBe expected
  }

  test("between two versioned documents") {
    val input    = ResolvedInternalTarget(Root / "tree-2" / "doc-5.md", RelativePath.parse("../tree-2/doc-5.md"))
    val expected = ResolvedInternalTarget(Root / "0.42" / "tree-2" / "doc-5.html", RelativePath.parse("../tree-2/doc-5.html"))
    versionedRef.translate(input) shouldBe expected
  }

  test("versioned to unversioned document") {
    val input    = ResolvedInternalTarget(Root / "doc-2.md", RelativePath.parse("../doc-2.md"))
    val expected = ResolvedInternalTarget(Root / "doc-2.html", RelativePath.parse("../../doc-2.html"))
    versionedRef.translate(input) shouldBe expected
  }

  test("unversioned to versioned document") {
    val input    = ResolvedInternalTarget(Root / "tree-2" / "doc-5.md", RelativePath.parse("tree-2/doc-5.md"))
    val expected = ResolvedInternalTarget(Root / "0.42" / "tree-2" / "doc-5.html", RelativePath.parse("0.42/tree-2/doc-5.html"))
    unversionedRef.translate(input) shouldBe expected
  }

  test("static unversioned document") {
    val input    = ResolvedInternalTarget(Root / "tree-3" / "doc-7.txt", RelativePath.parse("../tree-3/doc-7.txt"))
    val expected = ResolvedInternalTarget(Root / "tree-3" / "doc-7.txt", RelativePath.parse("../../tree-3/doc-7.txt"))
    versionedRef.translate(input) shouldBe expected
  }

  test("static versioned document") {
    val input    = ResolvedInternalTarget(Root / "tree-4" / "doc-8.txt", RelativePath.parse("../tree-4/doc-8.txt"))
    val expected = ResolvedInternalTarget(Root / "0.42" / "tree-4" / "doc-8.txt", RelativePath.parse("../tree-4/doc-8.txt"))
    versionedRef.translate(input) shouldBe expected
  }
  
  test("ignore versions when output format is not HTML") {
    val input    = ResolvedInternalTarget(Root / "tree-2" / "doc-5.md", RelativePath.parse("../tree-2/doc-5.md"))
    val expected = ResolvedInternalTarget(Root / "tree-2" / "doc-5.epub.xhtml", RelativePath.parse("../tree-2/doc-5.epub.xhtml"))
    epubRef.translate(input) shouldBe expected
  }
  
}
