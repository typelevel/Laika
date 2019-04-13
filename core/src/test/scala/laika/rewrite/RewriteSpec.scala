/*
 * Copyright 2013-2016 the original author or authors.
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
import laika.ast.helper.ModelBuilder
import org.scalatest.{FlatSpec, Matchers}
 
class RewriteSpec extends FlatSpec 
                  with Matchers
                  with ModelBuilder {

  
  "The rewriter" should "replace the first element of the children in a container" in {
    val rootElem = root(p("a"), p("b"), p("c"))
    rootElem rewriteBlocks { case Paragraph(Seq(Text("a",_)),_) => Replace(p("x")) } should be (root(p("x"), p("b"), p("c")))
  }
  
  it should "replace an element in the middle of the list of children in a container" in {
    val rootElem = root(p("a"), p("b"), p("c"))
    rootElem rewriteBlocks { case Paragraph(Seq(Text("b",_)),_) => Replace(p("x")) } should be (root(p("a"), p("x"), p("c")))
  }
  
  it should "replace the last element of the children in a container" in {
    val rootElem = root(p("a"), p("b"), p("c"))
    rootElem rewriteBlocks { case Paragraph(Seq(Text("c",_)),_) => Replace(p("x")) } should be (root(p("a"), p("b"), p("x")))
  }
  
  it should "remove the first element of the children in a container" in {
    val rootElem = root(p("a"), p("b"), p("c"))
    rootElem rewriteBlocks { case Paragraph(Seq(Text("a",_)),_) => Remove } should be (root(p("b"), p("c")))
  }
  
  it should "remove an element in the middle of the list of children in a container" in {
    val rootElem = root(p("a"), p("b"), p("c"))
    rootElem rewriteBlocks { case Paragraph(Seq(Text("b",_)),_) => Remove } should be (root(p("a"), p("c")))
  }
  
  it should "remove the last element of the children in a container" in {
    val rootElem = root(p("a"), p("b"), p("c"))
    rootElem rewriteBlocks { case Paragraph(Seq(Text("c",_)),_) => Remove } should be (root(p("a"), p("b")))
  }
  
  it should "replace the content of the header of a section, which is not part of the content list" in {
    val rootElem = root(Section(h(1, txt("Title")), List(p("Text"))))
    rootElem rewriteSpans { case Text("Title", _) => Replace(Text("New")) } should be (root(Section(h(1, txt("New")), List(p("Text")))))
  }
  
  it should "return the same instance if no rewrite rule matches" in {
    val rootElem = root(p("a"), p("b"), p("c"))
    rootElem rewriteBlocks { case Paragraph(Seq(Text("f",_)),_) => Remove } should be theSameInstanceAs (rootElem)
  }
  
  it should "return a new instance for a branch in the document tree that contains one or more modified children" in {
    val before = root(quote(p("a")), quote(p("b")), quote(p("c")))
    val after = before rewriteBlocks { case Paragraph(Seq(Text("a",_)),_) => Replace(p("x")) }
    before.content(0) should not be theSameInstanceAs (after.content(0))
  }
  
  it should "return the same instance for a branch in the document tree that does not contain any modified children" in {
    val before = root(quote(p("a")), quote(p("b")), quote(p("c")))
    val after = before rewriteBlocks { case Paragraph(Seq(Text("a",_)),_) => Replace(p("x")) }
    before.content(1) should be theSameInstanceAs (after.content(1))
  }

  it should "rewrite a span container" in {
    val before = p(txt("a"), em("b"), txt("c"))
    before rewriteSpans { case Emphasized(Seq(Text("b",_)),_) => Replace(str("x")) } should be (p(txt("a"), str("x"), txt("c")))
  }

  it should "rewrite a nested span container" in {
    val before = p(txt("a"), em("b"), txt("c"))
    before rewriteSpans { case Text("b",_) => Replace(txt("x")) } should be (p(txt("a"), em("x"), txt("c")))
  }
  
  it should "rewrite main content and attribution in a QuotedBlock" in {
    val before = root(QuotedBlock(Seq(p("a"), p("b")), Seq(txt("a"), txt("c"))))
    before.rewriteSpans { case Text("a", _) => Remove } should be (root(QuotedBlock(Seq(Paragraph(Nil), p("b")), Seq(txt("c")))))
  }

  it should "rewrite text in bullet list items" in {
    val before = root(bulletList() + "a" + "b" + "c")
    before.rewriteSpans { case Text("b", _) => Replace(Text("x")) } should be (root(bulletList() + "a" + "x" + "c"))
  }

  it should "rewrite text in enum list items" in {
    val before = root(enumList() + "a" + "b" + "c")
    before.rewriteSpans { case Text("b", _) => Replace(Text("x")) } should be (root(enumList() + "a" + "x" + "c"))
  }

  it should "rewrite text in a template element" in {
    val before = TemplateSpanSequence(Seq(TemplateElement(txt("a"))))
    before.rewriteSpans { case Text("a", _) => Replace(Text("x")) } should be (TemplateSpanSequence(Seq(TemplateElement(txt("x")))))
  }

  it should "rewrite text in table cells" in {
    val before = root(table(row(cell("a"), cell("b")), row(cell("a"), cell("c"))))
    before.rewriteSpans { case Text("a", _) => Replace(Text("x")) } should be (root(table(row(cell("x"), cell("b")), row(cell("x"), cell("c")))))
  }
   
}
