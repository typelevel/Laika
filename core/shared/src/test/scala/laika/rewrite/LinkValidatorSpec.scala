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
import laika.ast.sample.{ BuilderKey, SampleTrees }
import laika.ast._
import laika.rewrite.link.{ InvalidTarget, RecoveredTarget, ValidTarget }
import laika.rewrite.nav.TargetFormats
import munit.FunSuite

/** @author Jens Halm
  */
class LinkValidatorSpec extends FunSuite {

  private val testCursor: DocumentCursor = {
    import laika.ast.sample.SampleConfig._

    def doc2(key: BuilderKey): Seq[Block] = Seq(
      Header(1, "Title").withOptions(Id("ref")),
      Paragraph("text")
    )

    SampleTrees.sixDocuments
      .root.config(siteBaseURL("https://external/"))
      .doc4.config(targetFormats("html"))
      .static2.config(targetFormats("html"))
      .staticDoc(Root / "static-1" / "doc-7.txt")
      .staticDoc(Root / "static-2" / "doc-8.txt", "html")
      .docContent(doc2 _)
      .suffix("md")
      .buildCursor // TODO - buildCursor should be available on doc6
      .toOption
      .flatMap(_.allDocuments.find(_.path == Root / "tree-2" / "doc-6.md"))
      .get
  }

  def testTarget(path: String): InternalTarget = InternalTarget(RelativePath.parse(path))

  def msg(target: String): String =
    s"document for all output formats cannot reference document '$target' with restricted output formats"

  test("valid markup link target") {
    assertEquals(testCursor.validate(testTarget("../tree-1/doc-3.md")), ValidTarget)
  }

  test("valid markup link target with fragment") {
    assertEquals(testCursor.validate(testTarget("../tree-1/doc-3.md#ref")), ValidTarget)
  }

  test("valid static link target") {
    assertEquals(testCursor.validate(testTarget("../static-1/doc-7.txt")), ValidTarget)
  }

  test("invalid link target") {
    assertEquals(
      testCursor.validate(testTarget("../tree-1/doc-9.md")),
      InvalidTarget("unresolved internal reference: ../tree-1/doc-9.md")
    )
  }

  test("recoverable markup link target") {
    val absPath  = Path.parse("/tree-1/doc-4.md")
    val relPath  = absPath.relativeTo(Root / "tree-2" / "doc-6.md")
    val expected = RecoveredTarget(
      msg(relPath.toString),
      ResolvedInternalTarget(absPath, relPath, TargetFormats.Selected("html"))
    )
    assertEquals(testCursor.validate(testTarget("../tree-1/doc-4.md")), expected)
  }

  test("recoverable static link target") {
    val absPath  = Path.parse("/static-2/doc-8.txt")
    val relPath  = absPath.relativeTo(Root / "tree-2" / "doc-6.md")
    val expected = RecoveredTarget(
      msg(relPath.toString),
      ResolvedInternalTarget(absPath, relPath, TargetFormats.Selected("html"))
    )
    assertEquals(testCursor.validate(testTarget("../static-2/doc-8.txt")), expected)
  }

  test("valid span link") {
    val link = SpanLink.internal("../tree-1/doc-3.md")("text")
    assertEquals(testCursor.validate(link), Right(link))
  }

  test("invalid span link") {
    val link = SpanLink.internal("../tree-1/doc-9.md")("text")
    assertEquals(
      testCursor.validate(link),
      Left("unresolved internal reference: ../tree-1/doc-9.md")
    )
  }

  test("recovered span link") {
    val absPath         = Path.parse("/tree-1/doc-4.md")
    val relPath         = absPath.relativeTo(Root / "tree-2" / "doc-6.md")
    val link            = SpanLink.internal(relPath)("text")
    val recoveredTarget = ResolvedInternalTarget(absPath, relPath, TargetFormats.Selected("html"))
    assertEquals(testCursor.validate(link), Right(link.copy(target = recoveredTarget)))
  }

  test("unrecoverable image link") {
    val absPath = Path.parse("/tree-1/doc-4.md")
    val relPath = absPath.relativeTo(Root / "tree-2" / "doc-6.md")
    val link    = Image(InternalTarget(relPath))
    assertEquals(testCursor.validate(link), Left(msg("../tree-1/doc-4.md")))
  }

}
