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

package laika.ast

import laika.ast.sample.ParagraphCompanionShortcuts
import munit.FunSuite
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
 
class ElementTraversalSpec extends FunSuite with ParagraphCompanionShortcuts {

  
  test("select all elements that satisfy the predicate") {
    val rootElem = RootElement(p("a"), p("b"), p("c"), QuotedBlock("d"))
    assertEquals(rootElem.select { case Paragraph(_,_) => true; case _ => false }, List(p("a"), p("b"), p("c"), p("d")))
  }
  
  test("select the elements in depth-first order") {
    val rootElem = RootElement(QuotedBlock(QuotedBlock("a")), QuotedBlock("b"))
    assertEquals(rootElem.select { case QuotedBlock(_,_,_) => true; case _ => false }, List(
      QuotedBlock("a"), QuotedBlock(QuotedBlock("a")), QuotedBlock("b")
    ))
  }
  
  test("select elements which are not part of the content collection of a container") {
    val rootElem = RootElement(Section(Header(1,"Title"), Nil))
    assertEquals(rootElem.select { case Header(_,_,_) => true; case _ => false }, List(Header(1,"Title")))
  }
  
  test("return an empty list if no element satisfies the predicate") {
    val rootElem = RootElement(p("a"), p("b"), p("c"), QuotedBlock("d"))
    assertEquals(rootElem.select { case Header(_,_,_) => true; case _ => false }, Nil)
  }
  
  test("collect values based on a partial function applied to all elements") {
    val rootElem = RootElement(p("a"), p("b"), p("c"), QuotedBlock("d"))
    assertEquals(rootElem.collect { case Text(text,_) => text }, List("a", "b", "c", "d"))
  }
   
  
}
