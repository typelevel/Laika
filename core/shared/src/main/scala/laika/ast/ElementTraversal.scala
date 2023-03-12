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

import scala.collection.mutable.ListBuffer

/** Provides means to traverse and select elements of a document tree.
  *  This trait can get mixed in by any element node, not just the root Document class,
  *  therefore providing traversal for any kind of sub-tree, too.
  *
  *  @author Jens Halm
  */
trait ElementTraversal { this: Element =>

  /** Invokes the specified function for each child of this element
    *  container, including children of children, and this element itself,
    *  in depth-first traversal.
    */
  def foreach(f: Element => Unit): Unit = {

    def foreachInElement(element: Element, f: Element => Unit): Unit = {
      foreachInIterable(element.productIterator.toSeq, f)
      f(element)
    }

    def foreachInIterable(t: Iterable[_], f: Element => Unit): Unit = {
      t.foreach {
        case e: Element     => foreachInElement(e, f)
        case t: Iterable[_] => foreachInIterable(t, f)
        case _              => ()
      }
    }

    foreachInElement(this, f)
  }

  /** Selects all elements satisfying the specified predicate, collecting
    *  in depth-first traversal, including this element itself.
    */
  def select(p: Element => Boolean): List[Element] = {
    val buffer = new ListBuffer[Element]
    foreach { e =>
      if (p(e)) buffer += e
    }
    buffer.toList
  }

  /** Collects elements by applying the partial function to all elements
    *  it is defined for, in depth-first traversal, including this element
    *  itself.
    */
  def collect[B](pf: PartialFunction[Element, B]): List[B] = {
    val buffer = new ListBuffer[B]
    foreach { e =>
      if (pf.isDefinedAt(e)) buffer += pf(e)
    }
    buffer.toList
  }

}
