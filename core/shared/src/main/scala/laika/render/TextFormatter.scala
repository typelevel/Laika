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
import laika.factory.RenderContext

/** API for renderers that produce text output.
  *
  * @param renderChild  the function to use for rendering child elements
  * @param currentElement the active element currently being rendered
  * @param parents the stack of parent elements of this formatter in recursive rendering,
  *                with the root element being the last in the list
  * @param indentation the indentation mechanism for this formatter
  *
  * @author Jens Halm
  */
case class TextFormatter(
    renderChild: (TextFormatter, Element) => String,
    currentElement: Element,
    parents: List[Element],
    indentation: Indentation
) extends BaseFormatter[TextFormatter](
      renderChild,
      currentElement,
      parents,
      indentation,
      MessageFilter.Debug
    ) {

  protected def withChild(element: Element): TextFormatter =
    copy(parents = currentElement :: parents, currentElement = element)

  protected def withIndentation(newIndentation: Indentation): TextFormatter =
    copy(indentation = newIndentation)

}

/** Default factory for TextFormatters, based on a provided RenderContext.
  */
object TextFormatter extends (RenderContext[TextFormatter] => TextFormatter) {

  def apply(context: RenderContext[TextFormatter]): TextFormatter =
    TextFormatter(context.renderChild, context.root, Nil, context.indentation)

}

/** Default factory for ASTFormatters, based on a provided RenderContext.
  */
object ASTFormatter extends (RenderContext[TextFormatter] => TextFormatter) {

  def apply(context: RenderContext[TextFormatter]): TextFormatter =
    TextFormatter(context.renderChild, context.root, Nil, Indentation.dotted)

}
