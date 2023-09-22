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

import laika.ast._
import laika.ast.CellType.BodyCell
import laika.ast.RewriteAction.Replace
import laika.ast.sample.ParagraphCompanionShortcuts
import munit.FunSuite

class RewriteSpec extends FunSuite with ParagraphCompanionShortcuts {

  test("replace the first element of the children in a container") {
    val input    = RootElement(p("a"), p("b"), p("c"))
    val expected = RootElement(p("x"), p("b"), p("c"))
    val actual   = input.rewriteBlocks { case Paragraph(Seq(Text("a", _)), _) => Replace(p("x")) }
    assertEquals(actual, expected)
  }

  test("replace an element in the middle of the list of children in a container") {
    val input    = RootElement(p("a"), p("b"), p("c"))
    val expected = RootElement(p("a"), p("x"), p("c"))
    val actual   = input.rewriteBlocks { case Paragraph(Seq(Text("b", _)), _) => Replace(p("x")) }
    assertEquals(actual, expected)
  }

  test("replace the last element of the children in a container") {
    val input    = RootElement(p("a"), p("b"), p("c"))
    val expected = RootElement(p("a"), p("b"), p("x"))
    val actual   = input.rewriteBlocks { case Paragraph(Seq(Text("c", _)), _) => Replace(p("x")) }
    assertEquals(actual, expected)
  }

  test("remove the first element of the children in a container") {
    val input    = RootElement(p("a"), p("b"), p("c"))
    val expected = RootElement(p("b"), p("c"))
    val actual   = input.rewriteBlocks { case Paragraph(Seq(Text("a", _)), _) =>
      RewriteAction.Remove
    }
    assertEquals(actual, expected)
  }

  test("remove an element in the middle of the list of children in a container") {
    val input    = RootElement(p("a"), p("b"), p("c"))
    val expected = RootElement(p("a"), p("c"))
    val actual   = input.rewriteBlocks { case Paragraph(Seq(Text("b", _)), _) =>
      RewriteAction.Remove
    }
    assertEquals(actual, expected)
  }

  test("remove the last element of the children in a container") {
    val input    = RootElement(p("a"), p("b"), p("c"))
    val expected = RootElement(p("a"), p("b"))
    val actual   = input.rewriteBlocks { case Paragraph(Seq(Text("c", _)), _) =>
      RewriteAction.Remove
    }
    assertEquals(actual, expected)
  }

  test("replace the content of the header of a section, which is not part of the content list") {
    val input    = RootElement(Section(Header(1, Text("Title")), List(p("Text"))))
    val expected = RootElement(Section(Header(1, Text("New")), List(p("Text"))))
    val actual   = input.rewriteSpans { case Text("Title", _) => Replace(Text("New")) }
    assertEquals(actual, expected)
  }

  test(
    "return a new instance for a branch in the document tree that contains one or more modified children"
  ) {
    val before = RootElement(QuotedBlock("a"), QuotedBlock("b"), QuotedBlock("c"))
    val after  = before.rewriteBlocks { case Paragraph(Seq(Text("a", _)), _) => Replace(p("x")) }
    assert(before.content.head ne after.content.head)
  }

  test("rewrite a span container") {
    val before   = p(Text("a"), Emphasized("b"), Text("c"))
    val expected = p(Text("a"), Strong("x"), Text("c"))
    val after    = before.rewriteSpans { case Emphasized(Seq(Text("b", _)), _) =>
      Replace(Strong("x"))
    }
    assertEquals(after, expected)
  }

  test("rewrite a nested span container") {
    val before   = p(Text("a"), Emphasized("b"), Text("c"))
    val expected = p(Text("a"), Emphasized("x"), Text("c"))
    val after    = before.rewriteSpans { case Text("b", _) => Replace(Text("x")) }
    assertEquals(after, expected)
  }

  test("rewrite main content and attribution in a QuotedBlock") {
    val before   = RootElement(QuotedBlock(Seq(p("a"), p("b")), Seq(Text("a"), Text("c"))))
    val expected = RootElement(QuotedBlock(Seq(Paragraph.empty, p("b")), Seq(Text("c"))))
    val after    = before.rewriteSpans { case Text("a", _) => RewriteAction.Remove }
    assertEquals(after, expected)
  }

  test("rewrite text in bullet list items") {
    val before   = RootElement(BulletList("a", "b", "c"))
    val expected = RootElement(BulletList("a", "x", "c"))
    val after    = before.rewriteSpans { case Text("b", _) => Replace(Text("x")) }
    assertEquals(after, expected)
  }

  test("rewrite text in enum list items") {
    val before   = RootElement(EnumList("a", "b", "c"))
    val expected = RootElement(EnumList("a", "x", "c"))
    val after    = before.rewriteSpans { case Text("b", _) => Replace(Text("x")) }
    assertEquals(after, expected)
  }

  test("rewrite text in a template element") {
    val before   = TemplateSpanSequence(TemplateElement(Text("a")))
    val expected = TemplateSpanSequence(TemplateElement(Text("x")))
    val after    = before.rewriteSpans { case Text("a", _) => Replace(Text("x")) }
    assertEquals(after, expected)
  }

  test("rewrite text in table cells") {
    val before   =
      RootElement(Table(Row(BodyCell("a"), BodyCell("b")), Row(BodyCell("a"), BodyCell("c"))))
    val expected =
      RootElement(Table(Row(BodyCell("x"), BodyCell("b")), Row(BodyCell("x"), BodyCell("c"))))
    val after    = before.rewriteSpans { case Text("a", _) => Replace(Text("x")) }
    assertEquals(after, expected)
  }

}
