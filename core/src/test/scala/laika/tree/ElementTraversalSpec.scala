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

package laika.tree

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers

import laika.tree.Elements._
import laika.tree.helper.ModelBuilder
 
class ElementTraversalSpec extends FlatSpec 
                           with Matchers
                           with ModelBuilder {

  
  "The select method" should "select all elements that satisfy the predicate" in {
    val rootElem = root(p("a"), p("b"), p("c"), quote(p("d")))
    rootElem select { case Paragraph(_,_) => true; case _ => false } should be (List(p("a"), p("b"), p("c"), p("d")))
  }
  
  it should "select the elements in depth-first order" in {
    val rootElem = root(quote(quote(p("a"))), quote(p("b")))
    rootElem select { case QuotedBlock(_,_,_) => true; case _ => false } should be (List(quote(p("a")), quote(quote(p("a"))), quote(p("b"))))
  }
  
  it should "select elements which are not part of the content collection of a container" in {
    val rootElem = root(Section(h(1,"Title"), Nil))
    rootElem select { case Header(_,_,_) => true; case _ => false } should be (List(h(1,"Title")))
  }
  
  it should "return an empty list if no element satisfies the predicate" in {
    val rootElem = root(p("a"), p("b"), p("c"), quote(p("d")))
    rootElem select { case Header(_,_,_) => true; case _ => false } should be (Nil)
  }
  
  it should "collect values based on a partial function applied to all elements" in {
    val rootElem = root(p("a"), p("b"), p("c"), quote(p("d")))
    rootElem collect { case Text(text,_) => text } should be (List("a", "b", "c", "d"))
  }
   
  
}
