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


/** API for renderers that produce HTML output.
 * 
 * @param renderChild the function to use for rendering child elements
 * @param elementStack the stack of parent elements of this formatter in recursive rendering
 * @param indentation the level of indentation for this formatter
 *                   
 *  @author Jens Halm
 */
case class HTMLFormatter (renderChild: (HTMLFormatter, Element) => String,
                          elementStack: Seq[Element],
                          indentation: Indentation) extends TagFormatter[HTMLFormatter](renderChild, elementStack, indentation) {

  protected def withChild (element: Element): HTMLFormatter = copy(elementStack = elementStack :+ element)

  protected def withIndentation (newIndentation: Indentation): HTMLFormatter = copy(indentation = newIndentation)
  
  def attributes (tag: String, options: Options, attrs: Seq[(String,Any)]): String = {
    val id = options.id.map("id" -> _)
    val styles = if (options.styles.isEmpty) None else Some("class" -> options.styles.mkString(" "))
    val other = attrs map {
      case (name, Some(value)) => Some(name -> value.toString)
      case (_, None)           => None
      case (name, value)       => Some(name -> value.toString)
    }
    attributes((id +: styles +: other).flatten)
  }
 
}
