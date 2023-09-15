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

import laika.ast.{ Element, MessageFilter, Path, StyleDeclarationSet }
import laika.bundle.RenderOverrides
import laika.rewrite.nav.PathTranslator

/** Provides the context for a single render operation.
  *
  * @param renderChild a render function to use for rendering the children of an element
  * @param currentElement the element currently being rendered
  * @param parents the stack of parent elements of this formatter in recursive rendering,
  *                with the root element being the last in the list
  * @param styles the styles the new renderer should apply to the rendered elements
  * @param path the (virtual) path the output will be rendered to
  * @param pathTranslator translates paths of input documents to the corresponding output path
  * @param indentation the indentation mechanism to use for rendering
  * @param messageFilter the filter to apply before rendering runtime messages
  */
class RenderContext[FMT] private[laika] (
    val renderChild: (FMT, Element) => String,
    val currentElement: Element,
    val parents: List[Element],
    val styles: StyleDeclarationSet,
    val path: Path,
    val pathTranslator: PathTranslator,
    val indentation: Formatter.Indentation,
    val messageFilter: MessageFilter
) {

  def forChildElement(child: Element): RenderContext[FMT] =
    new RenderContext[FMT](
      renderChild,
      child,
      currentElement :: parents,
      styles,
      path,
      pathTranslator,
      indentation,
      messageFilter
    )

  def withIndentation(newValue: Formatter.Indentation): RenderContext[FMT] =
    new RenderContext[FMT](
      renderChild,
      currentElement,
      parents,
      styles,
      path,
      pathTranslator,
      newValue,
      messageFilter
    )

}

/** Responsible for creating renderer instances for a specific output format.
  *  A renderer is simply a function of type `(Formatter, Element) => String`. In addition
  *  to the actual renderer function, the factory method also produces
  *  an instance of the generic `FMT` type which is the formatter API to use
  *  for custom renderer functions and which is specific to the output format.
  *
  *  @author Jens Halm
  */
trait RenderFormat[FMT] extends Format {

  /** The file suffix to use when rendering the output
    *  to a file. When transforming entire directories
    *  the suffix of the markup file will be automatically
    *  replaced by the suffix for the output format.
    */
  def fileSuffix: String

  /** The default renderer function for this output format.
    * It may be overridden by extensions for individual nodes of the AST.
    *
    * The function takes both, a formatter instance
    * and the element to render and returns a String in the target format.
    */
  def defaultRenderer: (FMT, Element) => String

  /** The function for creating a new formatter for each render operation,
    * based on the specified context containing the root element, the indentation mechanism and
    * the delegate function for rendering child elements (that may contain user-specified extensions
    * this render format implementation is not aware of).
    *
    * The formatter created by this function (or copies created from it)
    * will be used when invoking the default renderer.
    */
  def formatterFactory: RenderContext[FMT] => FMT

  case class Overrides(value: PartialFunction[(FMT, Element), String] = PartialFunction.empty)
      extends RenderOverrides {

    type Formatter = FMT

    def withBase(base: Overrides): Overrides = Overrides(
      value.orElse(base.value)
    )

  }

}
