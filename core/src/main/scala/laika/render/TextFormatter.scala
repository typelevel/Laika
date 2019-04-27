/*
 * Copyright 2012-2019 the original author or authors.
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

package laika.render

import laika.ast._

/** API for renderers that produce character output.
 *  
 *  @param renderChild the function to use for rendering child elements
 *  @param elementStack the stack of parent elements of this formatter in recursive rendering
 *  @param indentLevel the level of indentation for this formatter
 *  @param indentItem the string to write for a single level of indentation
 *  @param newLine the new line character sequence 
 * 
 *  @author Jens Halm
 */
abstract class TextFormatter[Rep <: TextFormatter[Rep]] (renderChild: (Rep, Element) => String,
                                                         elementStack: Seq[Element],
                                                         indentLevel: Int = 0,
                                                         indentItem: String = "  ",
                                                         newLineChar: String = "\n") { this: Rep =>
  
  /** A newline character followed by whitespace matching the indentation level of this instance. 
    */
  val newLine: String = newLineChar + (" " * indentLevel)
  
  val parents: Seq[Element] = elementStack.tail
  
  private def renderCurrentElement: String = renderChild(this, elementStack.last)
  
  def withMinIndentation (minIndent: Int): Rep
  
  def withChild (element: Element): Rep
  
  /** Renders the specified elements, all on the same line.
   */
  def children (elements: Seq[Element]): String = elements.map(child).mkString

  /** Renders the specified element on the same line.
   */
  def child (element: Element): String = withChild(element).renderCurrentElement
  
  /** Writes the specified elements to the output, 
   *  each of them on a new line using the current level of indentation.
   */
  def childPerLine (elements: Seq[Element]): String = elements.map(child).mkString(newLine)
  
  /** Renders the specified elements, 
   *  each of them on a new line with the indentation increased one level to the right.
   */
  def indentedChildren (elements: Seq[Element]): String = {
    val formatter = withMinIndentation(indentLevel + 1)
    formatter.newLine + formatter.childPerLine(elements)
  }
  
}
