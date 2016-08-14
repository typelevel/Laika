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

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import laika.tree.Elements._
import laika.tree.helper.ModelBuilder
 
class RewriteSpec extends FlatSpec 
                  with Matchers
                  with ModelBuilder {

  
  "The rewriter" should "replace the first element of the children in a container" in {
    val rootElem = root(p("a"), p("b"), p("c"))
    rootElem rewrite { case Paragraph(Seq(Text("a",_)),_) => Some(p("x")) } should be (root(p("x"), p("b"), p("c")))
  }
  
  it should "replace an element in the middle of the list of children in a container" in {
    val rootElem = root(p("a"), p("b"), p("c"))
    rootElem rewrite { case Paragraph(Seq(Text("b",_)),_) => Some(p("x")) } should be (root(p("a"), p("x"), p("c")))
  }
  
  it should "replace the last element of the children in a container" in {
    val rootElem = root(p("a"), p("b"), p("c"))
    rootElem rewrite { case Paragraph(Seq(Text("c",_)),_) => Some(p("x")) } should be (root(p("a"), p("b"), p("x")))
  }
  
  it should "remove the first element of the children in a container" in {
    val rootElem = root(p("a"), p("b"), p("c"))
    rootElem rewrite { case Paragraph(Seq(Text("a",_)),_) => None } should be (root(p("b"), p("c")))
  }
  
  it should "remove an element in the middle of the list of children in a container" in {
    val rootElem = root(p("a"), p("b"), p("c"))
    rootElem rewrite { case Paragraph(Seq(Text("b",_)),_) => None } should be (root(p("a"), p("c")))
  }
  
  it should "remove the last element of the children in a container" in {
    val rootElem = root(p("a"), p("b"), p("c"))
    rootElem rewrite { case Paragraph(Seq(Text("c",_)),_) => None } should be (root(p("a"), p("b")))
  }
  
  it should "replace the header of a section, which is not part of the content list" in {
    val rootElem = root(Section(h(1, txt("Title")), List(p("Text"))))
    rootElem rewrite { case Header(1, content, _) => Some(Header(2, content)) } should be (root(Section(h(2, txt("Title")), List(p("Text")))))
  }
  
  it should "return the same instance if no rewrite rule matches" in {
    val rootElem = root(p("a"), p("b"), p("c"))
    rootElem rewrite { case Paragraph(Seq(Text("f",_)),_) => None } should be theSameInstanceAs (rootElem)
  }
  
  it should "return the same instance if the rewrite rule always returns the same instance" in {
    val rootElem = root(p("a"), p("b"), p("c"))
    rootElem rewrite { case element => Some(element) } should be theSameInstanceAs (rootElem)
  }
  
  it should "return a new instance for a branch in the document tree that contains one or more modified children" in {
    val before = root(quote(p("a")), quote(p("b")), quote(p("c")))
    val after = before rewrite { case Paragraph(Seq(Text("a",_)),_) => Some(p("x")) }
    before.content(0) should not be theSameInstanceAs (after.content(0))
  }
  
  it should "return the same instance for a branch in the document tree that does not contain any modified children" in {
    val before = root(quote(p("a")), quote(p("b")), quote(p("c")))
    val after = before rewrite { case Paragraph(Seq(Text("a",_)),_) => Some(p("x")) }
    before.content(1) should be theSameInstanceAs (after.content(1))
  }
  
  it should "throw an exception when a rewrite rule produces a new element that violates the contract of its parent element" in {
    val rootElem = root(Section(h(1,"Title"), Nil)) 
    an [rootElem.RewriteException] should be thrownBy { rootElem rewrite { case Header(_,_,_) => Some(em("x")) }} 
  }
  
  
}
