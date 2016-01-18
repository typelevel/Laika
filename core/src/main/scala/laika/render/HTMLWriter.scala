/*
 * Copyright 2013-2016 the original author or authors.
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

import laika.tree.Elements._
 

/** API for renderers that produce HTML output.
 * 
 *  @param out the render function to write string values to
 *  @param render the render function for writing elements
 *  @param newLine the newline character to use
 *  @param formatted whether the output is formatted (adding indentation and newlines)
 * 
 *  @author Jens Halm
 */
class HTMLWriter (out: String => Unit,  
                  render: Element => Unit, 
                  newLine: String = "\n",
                  formatted: Boolean = true) extends TagWriter(out, render, newLine, formatted) {

  
  protected def attributes (tag: String, options: Options, attrs: Seq[(String,Any)]): Seq[(String,Any)] = {
    val styles = if (options.styles.isEmpty) None else Some(options.styles.mkString(" "))
    ("id"->options.id) +: ("class"->styles) +: attrs
  }
  
  protected def attributes (tag: String, element: Element, attrs: Seq[(String,Any)]): Seq[(String,Any)] = {
    val options = element match {
      case c: Customizable => c.options
      case _ => NoOpt
    }
    attributes(tag, options, attrs)
  }
  
 
}
