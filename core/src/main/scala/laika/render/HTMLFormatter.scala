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
import laika.factory.RenderContext


/** API for renderers that produce HTML output.
 * 
 * @param renderChild the function to use for rendering child elements
 * @param elementStack the stack of parent elements of this formatter in recursive rendering, 
 *                     with the root element being the last in the list
 * @param indentation the indentation mechanism for this formatter
 * @param messageLevel the minimum severity level for a system message to be rendered  
 *                   
 *  @author Jens Halm
 */
case class HTMLFormatter (renderChild: (HTMLFormatter, Element) => String,
                          elementStack: List[Element],
                          indentation: Indentation,
                          messageLevel: MessageLevel) extends 
  TagFormatter[HTMLFormatter](renderChild, elementStack, indentation, messageLevel) {

  type StyleHint = Options
  
  protected def withChild (element: Element): HTMLFormatter = copy(elementStack = element :: elementStack)

  protected def withIndentation (newIndentation: Indentation): HTMLFormatter = copy(indentation = newIndentation)
  
  def attributes (tag: String, styleHint: StyleHint, attrs: Seq[(String, String)]): String = {
    val id = styleHint.id.map("id" -> _).toSeq
    val styles = if (styleHint.styles.isEmpty) Nil else Seq("class" -> styleHint.styles.mkString(" "))
    attributes(id ++ styles ++ attrs)
  }

  override def emptyElement (tagName: String, styleHint: StyleHint, attrs: (String, String)*): String =
    s"<$tagName${attributes(tagName,styleHint,attrs)}>"

  override def emptyElement (tagName: String): String = s"<$tagName>"
 
}

/** Default factory for HTMLFormatters, based on a provided RenderContext.
  */
object HTMLFormatter extends (RenderContext[HTMLFormatter] => HTMLFormatter) {
  def apply (context: RenderContext[HTMLFormatter]): HTMLFormatter =
    HTMLFormatter(context.renderChild, List(context.root), context.indentation, context.config.minMessageLevel)
}
