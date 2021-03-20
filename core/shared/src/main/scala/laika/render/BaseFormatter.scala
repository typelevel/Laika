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

package laika.render

import laika.ast._

/** API basis for renderers that produce character output.
 *  
 *  @param renderChild the function to use for rendering child elements
 *  @param currentElement the active element currently being rendered                   
 *  @param parents the stack of parent elements of this formatter in recursive rendering, 
 *                 with the root element being the last in the list
 *  @param indentation the indentation mechanism for this formatter
 *  @param messageFilter the filter to apply before rendering runtime messages                    
 * 
 *  @author Jens Halm
 */
abstract class BaseFormatter[Rep <: BaseFormatter[Rep]] (renderChild: (Rep, Element) => String,
                                                         currentElement: Element,
                                                         parents: List[Element],
                                                         indentation: Indentation,
                                                         messageFilter: MessageFilter) { this: Rep =>
  
  /** A newline character followed by whitespace matching the indentation level of this instance. 
    */
  val newLine: String = indentation.newLine

  
  private[BaseFormatter] def renderCurrentElement: String = renderChild(this, currentElement)
  
  private lazy val nextLevel: Rep = if (indentation == Indentation.none) this else withIndentation(indentation.nextLevel)

  
  protected def withChild (element: Element): Rep

  protected def withIndentation (newIndentation: Indentation): Rep


  /** Invokes the specified render function with a new formatter that is indented
    * one level to the right of this formatter.
    */
  def indented (f: Rep => String): String = f(nextLevel)

  /** Invokes the specified render function with a new formatter that has all indentation disabled.
    * 
    * This is usually only required when rendering literal elements or source code
    * where rendered whitespace would be significant.
    */
  def withoutIndentation (f: Rep => String): String = f(withIndentation(Indentation.none))

  /** Invokes the specified render function with a formatter that has at least the specified minimum
    * level of indentation. If this instance already has an indentation equal or greater
    * to this value, the current level of indentation will be kept.
    */
  def withMinIndentation (minIndent: Int)(f: Rep => String): String = {
    val newIndentation = if (indentation == Indentation.none || indentation.currentLevel >= minIndent) this 
    else withIndentation(indentation.copy(currentLevel = minIndent))
    f(newIndentation)
  }

  /** Renders the specified string only when the given message has at least the minimum
    * message level defined for this formatter instance.
    */
  def forMessage (message: RuntimeMessage)(whenEnabled: String): String = 
    if (messageFilter(message)) whenEnabled else ""
  
  
  /** Renders the specified elements, all on the same line, without any separators.
   */
  def children (elements: Seq[Element]): String = elements.map(child).mkString

  /** Renders the specified element on the current line.
   */
  def child (element: Element): String = withChild(element).renderCurrentElement
  
  /** Renders the specified elements, 
   *  each of them on a new line using the current level of indentation.
   */
  def childPerLine (elements: Seq[Element]): String = elements.map(child).mkString(newLine)
  
  /** Renders the specified elements, 
   *  each of them on a new line with the indentation increased one level to the right.
   */
  def indentedChildren (elements: Seq[Element]): String = {
    if (elements.isEmpty) ""
    else indented { fmt =>
      fmt.newLine + fmt.childPerLine(elements)
    }
  }
  
}

/** Represents the current indentation level of a formatter instance.
  * 
  * @param currentLevel the level of indentation (number of characters)
  * @param numSpaces the number of space characters to add when creating the next level of indentation from this instance
  * @param dotted indicates whether the indentation happens with a dot pattern or with just whitespace
  */
case class Indentation (currentLevel: Int, numSpaces: Int, dotted: Boolean = false) {

  /** A string container the newline character and all characters needed for rendering
    * the current level of indentation.
    */
  val newLine: String =
    if (dotted) "\n" + (". " * (currentLevel / 2)) + (if (currentLevel % 2 == 1) "." else "")
    else "\n" + (" " * currentLevel)

  /** Creates a new instance with the indentation level increased by `numCharacters`.
    */
  lazy val nextLevel: Indentation = if (numSpaces == 0) this else copy(currentLevel = currentLevel + numSpaces)

}

/** Default Indentation instances.
  */
object Indentation {

  /** An instance with indentation disabled.
    */
  val none: Indentation = Indentation(0,0)

  /** The default indentation mechanism, adding two spaces per indentation level.
    */
  val default: Indentation = Indentation(0,2)

  /** Renders indentation with a dot pattern.
    * 
    * Used internally for formatted output of ASTs.
    */
  val dotted: Indentation = Indentation(0, 2, dotted = true)
  
}
