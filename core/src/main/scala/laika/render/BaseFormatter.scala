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
 *  @param indentation the level of indentation for this formatter
 *  @param messageLevel the minimum severity level for a system message to be rendered                   
 * 
 *  @author Jens Halm
 */
abstract class BaseFormatter[Rep <: BaseFormatter[Rep]] (renderChild: (Rep, Element) => String,
                                                         elementStack: List[Element],
                                                         indentation: Indentation,
                                                         messageLevel: MessageLevel) { this: Rep =>
  
  /** A newline character followed by whitespace matching the indentation level of this instance. 
    */
  val newLine: String = indentation.newLine
  
  val parents: Seq[Element] = elementStack.tail
  
  
  private def renderCurrentElement: String = renderChild(this, elementStack.head)
  
  private lazy val nextLevel: Rep = if (indentation == Indentation.none) this else withIndentation(indentation.nextLevel)

  
  protected def withChild (element: Element): Rep

  protected def withIndentation (newIndentation: Indentation): Rep
  
  
  def indented (f: Rep => String): String = f(nextLevel)
  
  def withoutIndentation (f: Rep => String): String = f(withIndentation(Indentation.none))
  
  def withMinIndentation (minIndent: Int)(f: Rep => String): String = {
    val newIndentation = if (indentation == Indentation.none || indentation.currentLevel >= minIndent) this 
    else withIndentation(indentation.copy(currentLevel = minIndent))
    f(newIndentation)
  }
  
  def forMessage (message: SystemMessage)(whenEnabled: String): String = 
    if (messageLevel <= message.level) whenEnabled else ""
  
  
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
    indented { fmt =>
      fmt.newLine + fmt.childPerLine(elements)
    }
  }
  
}

case class Indentation (currentLevel: Int, numSpaces: Int) {

  val newLine: String = "\n" + (" " * currentLevel)

  lazy val nextLevel: Indentation = if (numSpaces == 0) this else copy(currentLevel = currentLevel + numSpaces)

}

object Indentation {
  
  val none: Indentation = Indentation(0,0)
  
  val default: Indentation = Indentation(0,2)
  
}
