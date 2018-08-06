/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.collection

/** Very minimal replacement for the SDK Stack implementation that
  * was deprecated in Scala 2.12
  *
  * @author Jens Halm
  */
class Stack[T] {

  private var underlying: List[T] = Nil

  /** All current elements in this stack,
    * with the top element returned as the
    * first element.
    *
    * This is an immutable List.
    */
  def elements: List[T] = underlying

  /** Indicates whether this stack is empty.
    */
  def isEmpty: Boolean = underlying.isEmpty

  /** Indicates whether this stack contains at least one element.
    */
  def nonEmpty: Boolean = underlying.nonEmpty

  /** Returns the top element without modifying the Stack.
    */
  def top: T = underlying.head

  /** Returns the top element and removes it from the Stack.
    */
  def pop: T = {
    val elem = underlying.head
    underlying = underlying.tail
    elem
  }

  /** Pushes the specified element to the top the Stack.
    */
  def push (elem: T): Unit = underlying = elem +: underlying

}
