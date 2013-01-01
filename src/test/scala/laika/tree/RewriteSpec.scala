/*
 * Copyright 2013 the original author or authors.
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

package laika.tree

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

import laika.tree.Elements._
import laika.tree.helper.ModelBuilder
 
@RunWith(classOf[JUnitRunner])
class RewriteSpec extends FlatSpec 
									with ShouldMatchers
									with ModelBuilder {

  
  "The rewriter" should "replace the first element of the children in a container" in {
    val document = doc(p("a"), p("b"), p("c"))
    document rewrite { case Paragraph(List(Text("a"))) => Some(p("x")) } should be (doc(p("x"), p("b"), p("c")))
  }
  
  it should "replace an element in the middle of the list of children in a container" in {
    val document = doc(p("a"), p("b"), p("c"))
    document rewrite { case Paragraph(List(Text("b"))) => Some(p("x")) } should be (doc(p("a"), p("x"), p("c")))
  }
  
  it should "replace the last element of the children in a container" in {
    val document = doc(p("a"), p("b"), p("c"))
    document rewrite { case Paragraph(List(Text("c"))) => Some(p("x")) } should be (doc(p("a"), p("b"), p("x")))
  }
  
  it should "remove the first element of the children in a container" in {
    val document = doc(p("a"), p("b"), p("c"))
    document rewrite { case Paragraph(List(Text("a"))) => None } should be (doc(p("b"), p("c")))
  }
  
  it should "remove an element in the middle of the list of children in a container" in {
    val document = doc(p("a"), p("b"), p("c"))
    document rewrite { case Paragraph(List(Text("b"))) => None } should be (doc(p("a"), p("c")))
  }
  
  it should "remove the last element of the children in a container" in {
    val document = doc(p("a"), p("b"), p("c"))
    document rewrite { case Paragraph(List(Text("c"))) => None } should be (doc(p("a"), p("b")))
  }
  
  it should "replace the header of a section, which is not part of the content list" in {
    val document = doc(Section(h(1, txt("Title")), List(p("Text"))))
    document rewrite { case Header(1, content) => Some(Header(2, content)) } should be (doc(Section(h(2, txt("Title")), List(p("Text")))))
  }
  
  it should "return the same instance if no rewrite rule matches" in {
    val document = doc(p("a"), p("b"), p("c"))
    document rewrite { case Paragraph(List(Text("f"))) => None } should be theSameInstanceAs (document)
  }
  
  it should "return the same instance if the rewrite rule always returns the same instance" in {
    val document = doc(p("a"), p("b"), p("c"))
    document rewrite { case element => Some(element) } should be theSameInstanceAs (document)
  }
  
  it should "return a new instance for a branch in the document tree that contains one or more modified children" in {
    val before = doc(quote(p("a")), quote(p("b")), quote(p("c")))
    val after = before rewrite { case Paragraph(List(Text("a"))) => Some(p("x")) }
    before.content(0) should not be theSameInstanceAs (after.content(0))
  }
  
  it should "return the same instance for a branch in the document tree that does not contain any modified children" in {
    val before = doc(quote(p("a")), quote(p("b")), quote(p("c")))
    val after = before rewrite { case Paragraph(List(Text("a"))) => Some(p("x")) }
    before.content(1) should be theSameInstanceAs (after.content(1))
  }
  
  it should "throw an exception when a rewrite rule produces a new element that violates the contract of its parent element" in {
    val document = doc(Section(h(1,"Title"), Nil)) 
    evaluating { document rewrite { case Header(_,_) => Some(em("x")) }} should produce [document.RewriteException]
  }
  
  
}