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

package laika.api.format

import laika.api.bundle.PathTranslator
import laika.ast.*
import laika.ast.styles.StyleDeclarationSet

/** API basis for renderers that produce character output.
  *
  * @author Jens Halm
  */
abstract class Formatter protected {

  type Rep <: Formatter

  protected def self: Rep

  protected def context: RenderContext[Rep]

  protected def withChild(element: Element): Rep

  protected def withIndentation(newIndentation: Formatter.Indentation): Rep

  private def renderCurrentElement: String = context.renderChild(self, currentElement)

  private lazy val nextLevel: Rep =
    if (context.indentation == Formatter.Indentation.none) self
    else withIndentation(context.indentation.nextLevel)

  /** The active element currently being rendered. */
  def currentElement: Element = context.currentElement

  /** The stack of parent elements of this formatter in recursive rendering,
    * with the root element being the last in the list.
    * Does not include the current element.
    */
  def parents: List[Element] = context.parents

  /** The target path of the currently rendered document. */
  def path: Path = context.path

  /** Translates paths of input documents to the corresponding output path.
    *
    * This API needs to be used for rendering all internal links.
    */
  def pathTranslator: PathTranslator = context.pathTranslator

  /** The styles the new renderer should apply to the rendered elements.
    *
    * Only used for some special render formats like XSL-FO.
    * In case of HTML, for example, styles are only copied over to the output directory
    * and not processed by the formatter at all.
    * For all those formats this set is always empty.
    */
  def styles: StyleDeclarationSet = context.styles

  /** A newline character followed by whitespace matching the indentation level of this instance.
    */
  val newLine: String = context.indentation.newLine

  /** Invokes the specified render function with a new formatter that is indented
    * one level to the right of this formatter.
    */
  def indented(f: Rep => String): String = f(nextLevel)

  /** Invokes the specified render function with a new formatter that has all indentation disabled.
    *
    * This is usually only required when rendering literal elements or source code
    * where rendered whitespace would be significant.
    */
  def withoutIndentation(f: Rep => String): String = f(withIndentation(Formatter.Indentation.none))

  /** Invokes the specified render function with a formatter that has at least the specified minimum
    * level of indentation. If this instance already has an indentation equal or greater
    * to this value, the current level of indentation will be kept.
    */
  def withMinIndentation(minIndent: Int)(f: Rep => String): String = {
    val newIndentation =
      if (
        context.indentation == Formatter.Indentation.none || context.indentation.currentLevel >= minIndent
      )
        self
      else withIndentation(context.indentation.copy(currentLevel = minIndent))
    f(newIndentation)
  }

  /** Renders the specified string only when the given message has at least the minimum
    * message level defined for this formatter instance.
    */
  def forMessage(message: RuntimeMessage)(whenEnabled: String): String =
    if (context.messageFilter(message)) whenEnabled else ""

  /** Renders the specified elements, all on the same line, without any separators.
    */
  def children(elements: Seq[Element]): String = elements.map(child).mkString

  /** Renders the specified element on the current line.
    */
  def child(element: Element): String = withChild(element).renderCurrentElement

  /** Renders the specified elements,
    *  each of them on a new line using the current level of indentation.
    */
  def childPerLine(elements: Seq[Element]): String = elements.map(child).mkString(newLine)

  /** Renders the specified elements,
    *  each of them on a new line with the indentation increased one level to the right.
    */
  def indentedChildren(elements: Seq[Element]): String = {
    if (elements.isEmpty) ""
    else
      indented { fmt =>
        fmt.newLine + fmt.childPerLine(elements)
      }
  }

}

object Formatter {

  /** The most basic formatter implementation & API.
    *
    * Use `TagFormatter` for all outputs with angle brackets (e.g. HTML).
    */
  def defaultFactory: RenderContext[Formatter] => Formatter = ctx =>
    new Formatter {
      type Rep = Formatter
      protected def self: Rep                         = this
      protected def context: RenderContext[Formatter] = ctx

      protected def withChild(element: Element): Rep =
        defaultFactory(ctx.forChildElement(element))

      protected def withIndentation(newIndentation: Indentation): Rep =
        defaultFactory(ctx.withIndentation(newIndentation))

    }

  /** Represents the current indentation level of a formatter instance.
    *
    * @param currentLevel the level of indentation (number of characters)
    * @param numSpaces    the number of space characters to add when creating the next level of indentation from this instance
    * @param dotted       indicates whether the indentation happens with a dot pattern or with just whitespace
    */
  case class Indentation(currentLevel: Int, numSpaces: Int, dotted: Boolean = false) {

    /** A string container the newline character and all characters needed for rendering
      * the current level of indentation.
      */
    val newLine: String =
      if (dotted) "\n" + (". " * (currentLevel / 2)) + (if (currentLevel % 2 == 1) "." else "")
      else "\n" + (" " * currentLevel)

    /** Creates a new instance with the indentation level increased by `numCharacters`.
      */
    lazy val nextLevel: Indentation =
      if (numSpaces == 0) this else copy(currentLevel = currentLevel + numSpaces)

  }

  /** Default Indentation instances.
    */
  object Indentation {

    /** An instance with indentation disabled.
      */
    val none: Indentation = Indentation(0, 0)

    /** The default indentation mechanism, adding two spaces per indentation level.
      */
    val default: Indentation = Indentation(0, 2)

    /** Renders indentation with a dot pattern.
      *
      * Used internally for formatted output of ASTs.
      */
    val dotted: Indentation = Indentation(0, 2, dotted = true)

  }

}
