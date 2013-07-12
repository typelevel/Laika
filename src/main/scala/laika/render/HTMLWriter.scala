/*
 * Copyright 2013 the original author or authors.
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

import scala.collection.mutable.StringBuilder
import laika.tree.Elements._
import scala.collection.immutable.ListMap
 

/** API for renderers that produce HTML output.
 *  Extends the base `TextWriter` and adds methods for writing text
 *  with special HTML characters escaped and for conveniently writing
 *  attributes.
 * 
 *  @param out
 *  @param render
 *  @param newLine
 * 
 *  @author Jens Halm
 */
class HTMLWriter (out: String => Unit,  
                  render: Element => Unit, 
                  newLine: String = "\n",
                  formatted: Boolean = true) extends TextWriter(out, render, 
                                                                newLine = if (formatted) newLine else "", 
                                                                indentItem = if (formatted) "  " else "") {

  
  
  /** Writes the specified string to the output, on the same line, 
   *  with all special HTML characters converted to HTML entities.
   */
  def <<& (str: String): this.type = { <<(escaped(str)); this }
  
  /** Writes the specified string to the output, 
   *  on a new line using the current level of indentation,
   *  with all special HTML characters converted to HTML entities.
   */
  def <<|& (str: String): this.type = { <<|(escaped(str)); this }

  /** Writes the specified string to the output, 
   *  on a new line and increasing indentation one level to the right,  
   *  with all special HTML characters converted to HTML entities.
   */
  def <<|>& (str: String): this.type = { <<|>(escaped(str)); this }
  
  /** Writes the specified string to the output, 
   *  without any indentation,  
   *  with all special HTML characters converted to HTML entities.
   *  This is needed for writing blocks like those enclosed in &lt;pre&rt;
   *  tags where whitespace is significant.
   */
  def <<<& (str: String): this.type = { <<(escaped(str, false)); this }
  
  /** Writes the specified name and value as an optional HTML attribute, including
   *  a preceding space character. In case the value is `None` nothing
   *  will be written to the output.
   */
  def <<@ (name: String, value: Option[String]): this.type = value match {
    case Some(value) => <<@(name,value)
    case None        => this
  }
  
  /** Writes the specified name and value as an HTML attribute, including
   *  a preceding space character.
   */
  def <<@ (name: String, value: String): this.type = this << " " << name << "=\"" << value << "\""
 
  /** Writes an opening tag with attributes derived from both
   *  the options parameter and the subsequent tuples. The latter
   *  also allow for overriding of `class` and `id` attributes
   *  derived from the `Options` instance.
   */
  def <<@ (tag: String, options: Options, attrs: (String,Any)*): this.type = {
    val styles = if (options.styles.isEmpty) None else Some(options.styles.mkString(" "))
    val atMap = ListMap("id"->options.id,"class"->styles) ++ attrs
    this << "<" << tag
    for ((name,value) <- atMap) {
      value match {
        case Some(value) => <<@ (name, value.toString)
        case None        => ()
        case _           => <<@ (name, value.toString)
      }
    }
    this << ">"
  }
    
  
  private def escaped (str: String, indented: Boolean = true) = {
    var i = 0
    val end = str.length
    val result = new StringBuilder()
    while (i < end) {
      str.charAt(i) match {
        case '<' => result append "&lt;"
        case '>' => result append "&gt;"
        case '"' => result append "&quot;"
        case '\''=> result append "&#39;"
        case '&' => result append "&amp;"
        case '\n' => if (indented) result append Indent.current else result append "\n"; 
        case c   => result append c
      }
      i += 1
    }
    result.toString
  }

  
}