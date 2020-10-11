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

import laika.ast.{/, Document, DocumentTree, DocumentTreeRoot, Header, Id, Image, InternalTarget, InvalidSpan, Paragraph, Path, RelativePath, ResolvedInternalTarget, RootCursor, RootElement, SpanLink, StaticDocument, Target, Text}
import laika.ast.Path.Root
import laika.ast.helper.ModelBuilder
import laika.config.{Config, ConfigBuilder, LaikaKeys}
import laika.rewrite.link.{InvalidTarget, RecoveredTarget, ValidTarget}
import laika.rewrite.nav.TargetFormats
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/**
  * @author Jens Halm
  */
class LinkValidatorSpec extends AnyFunSuite with Matchers with ModelBuilder {

  
  private val testCursor = {

    val rootConfig = ConfigBuilder.empty.withValue(LaikaKeys.siteBaseURL, "https://external/").build
    val docConfig = ConfigBuilder.withFallback(rootConfig).build
    val targetConfig = ConfigBuilder.withFallback(rootConfig).withValue(LaikaKeys.targetFormats, Seq("html")).build
    
    def doc (num: Int, config: Config = docConfig): Document = {
      val basePath = if (num < 3) Root else if (num < 5) Root / "tree-1" else Root / "tree-2"
      val blocks = Seq(
        Header(1, "Title").withOptions(Id("ref")),
        Paragraph("text")
      )
      Document(basePath / ("doc-" + num + ".md"), RootElement(blocks), config = config)
    }
    
    val tree1 = DocumentTree(Root / "tree-1", Seq(doc(3), doc(4, targetConfig)))
    val tree2 = DocumentTree(Root / "tree-2", Seq(doc(5), doc(6)))
    val tree3 = DocumentTree(Root / "tree-3", Nil)
    val tree4 = DocumentTree(Root / "tree-4", Nil, config = targetConfig)
    
    val root = DocumentTreeRoot(
      DocumentTree(Root, Seq(doc(1), doc(2), tree1, tree2, tree3, tree4), config = rootConfig),
      staticDocuments = Seq(
        StaticDocument(Root / "tree-3" / "doc-7.txt"),
        StaticDocument(Root / "tree-4" / "doc-8.txt", TargetFormats.Selected("html"))
      )
    )
    
    RootCursor(root).allDocuments.find(_.path == Root / "tree-2" / "doc-6.md").get
    
  }
  
  def testTarget (path: String): InternalTarget = InternalTarget(RelativePath.parse(path))
  
  def msg (target: String): String = 
    s"document for all output formats cannot reference document '$target' with restricted output formats"
  
  
  test("valid markup link target") {
    testCursor.validate(testTarget("../tree-1/doc-3.md")) shouldBe ValidTarget
  }

  test("valid markup link target with fragment") {
    testCursor.validate(testTarget("../tree-1/doc-3.md#ref")) shouldBe ValidTarget
  }

  test("valid static link target") {
    testCursor.validate(testTarget("../tree-3/doc-7.txt")) shouldBe ValidTarget
  }
  
  test("invalid link target") {
    testCursor.validate(testTarget("../tree-1/doc-9.md")) shouldBe InvalidTarget("unresolved internal reference: ../tree-1/doc-9.md")
  }
  
  test("recoverable markup link target") {
    val absPath = Path.parse("/tree-1/doc-4.md")
    val relPath = absPath.relativeTo(Root / "tree-2" / "doc-6.md")
    val expected = RecoveredTarget(msg(relPath.toString), ResolvedInternalTarget(absPath, relPath, TargetFormats.Selected("html")))
    testCursor.validate(testTarget("../tree-1/doc-4.md")) shouldBe expected
  }

  test("recoverable static link target") {
    val absPath = Path.parse("/tree-4/doc-8.txt")
    val relPath = absPath.relativeTo(Root / "tree-2" / "doc-6.md")
    val expected = RecoveredTarget(msg(relPath.toString), ResolvedInternalTarget(absPath, relPath, TargetFormats.Selected("html")))
    testCursor.validate(testTarget("../tree-4/doc-8.txt")) shouldBe expected
  }

  test("valid span link") {
    val link = SpanLink(Seq(Text("text")), Target.parse("../tree-1/doc-3.md"))
    testCursor.validate(link) shouldBe Right(link)
  }

  test("invalid span link") {
    val link = SpanLink(Seq(Text("text")), Target.parse("../tree-1/doc-9.md"))
    testCursor.validate(link) shouldBe Left("unresolved internal reference: ../tree-1/doc-9.md")
  }

  test("recovered span link") {
    val absPath = Path.parse("/tree-1/doc-4.md")
    val relPath = absPath.relativeTo(Root / "tree-2" / "doc-6.md")
    val link = SpanLink(Seq(Text("text")), InternalTarget(relPath))
    val recoveredTarget = ResolvedInternalTarget(absPath, relPath, TargetFormats.Selected("html"))
    testCursor.validate(link) shouldBe Right(link.copy(target = recoveredTarget))
  }

  test("unrecoverable image link") {
    val absPath = Path.parse("/tree-1/doc-4.md")
    val relPath = absPath.relativeTo(Root / "tree-2" / "doc-6.md")
    val link = Image(InternalTarget(relPath))
    testCursor.validate(link) shouldBe Left(msg("../tree-1/doc-4.md"))
  }
  
}
