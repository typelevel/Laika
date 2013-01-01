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
class ElementTraversalSpec extends FlatSpec 
									with ShouldMatchers
									with ModelBuilder {

  
  "The select method" should "select all elements that satisfy the predicate" in {
    val document = doc(p("a"), p("b"), p("c"), quote(p("d")))
    document select { case Paragraph(_) => true; case _ => false } should be (List(p("a"), p("b"), p("c"), p("d")))
  }
  
  it should "select the elements in depth-first order" in {
    val document = doc(quote(quote(p("a"))), quote(p("b")))
    document select { case QuotedBlock(_) => true; case _ => false } should be (List(quote(p("a")), quote(quote(p("a"))), quote(p("b"))))
  }
  
  it should "select elements which are not part of the content collection of a container" in {
    val document = doc(Section(h(1,"Title"), Nil))
    document select { case Header(_,_) => true; case _ => false } should be (List(h(1,"Title")))
  }
  
  it should "return an empty list if no element satisfies the predicate" in {
    val document = doc(p("a"), p("b"), p("c"), quote(p("d")))
    document select { case Header(_,_) => true; case _ => false } should be (Nil)
  }
   
  
  
  
}