/*
 * Copyright 2014 the original author or authors.
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

/** Base type for writers that produce tag-based output formats like XML 
 *  or HTML.
 *  Extends the base `TextWriter` and adds methods for writing text
 *  with special characters as entities and for conveniently writing
 *  tags with attributes.
 * 
 *  @param out the render function to write string values to
 *  @param render the render function for writing elements
 *  @param newLine the newline character to use
 *  @param formatted whether the output is formatted (adding indentation and newlines)
 * 
 *  @author Jens Halm
 */
abstract class TagWriter (out: String => Unit,  
                          render: Element => Unit, 
                          newLine: String = "\n",
                          formatted: Boolean = true) extends 
                              TextWriter(out, render, 
                                         newLine = if (formatted) newLine else "", 
                                         indentItem = if (formatted) "  " else "") {
  
  
  /** Writes the specified string to the output, on the same line, 
   *  with all special XML/HTML characters converted to entities.
   */
  def <<& (str: String): this.type = { <<(escaped(str)); this }
  
  /** Writes the specified string to the output, 
   *  on a new line using the current level of indentation,
   *  with all special XML/HTML characters converted to entities.
   */
  def <<|& (str: String): this.type = { <<|(escaped(str)); this }

  /** Writes the specified string to the output, 
   *  on a new line and increasing indentation one level to the right,  
   *  with all special XML/HTML characters converted to entities.
   */
  def <<|>& (str: String): this.type = { <<|>(escaped(str)); this }
  
  /** Writes the specified string to the output, 
   *  without any indentation,  
   *  with all special XML/HTML characters converted to entities.
   *  This is needed for writing blocks like those enclosed in &lt;pre&rt;
   *  tags where whitespace is significant.
   */
  def <<<& (str: String): this.type = { 
    withoutIndentation { <<(escaped(str)) }
    this 
  }
  
  /** Writes the specified span elements to the output,
   *  on the same line, while omitting indentation
   *  for all text spans written with one of the methods
   *  that convert special characters.  
   */
  def <<< (elements: Seq[Span]): this.type = {
    withoutIndentation { <<(elements) }
    this
  }
  
  private var indented = true
  
  /** Invokes the specified function after switching off
   *  the rendering of any indentation for escaped text elements.
   *  The old flag gets restored after invocation.
   */
  protected def withoutIndentation (f: => Any): this.type = {
    val oldFlag = indented
    indented = false
    f
    indented = oldFlag
    this
  }
  
  /** Writes the specified name and value as an optional attribute, including
   *  a preceding space character. In case the value is `None` nothing
   *  will be written to the output.
   */
  def <<@ (name: String, value: Option[String]): this.type = value match {
    case Some(value) => <<@(name,value)
    case None        => this
  }
  
  /** Writes the specified name and value as an attribute, including
   *  a preceding space character.
   */
  def <<@ (name: String, value: String): this.type = this << " " << name << "=\"" << value << "\""
 
  /** Writes the specified name and value as an optional attribute, including
   *  a preceding space character. In case the value is `None` nothing
   *  will be written to the output.
   */
  protected def <<@ (name: String, value: Any): this.type = value match {
    case Some(value) => <<@ (name, value.toString)
    case None        => this
    case _           => <<@ (name, value.toString)
  }
  
  /** Writes the specified attributes (passed as name-value tuples),
   *  including a preceding space character. In cases where the value 
   *  is `None` nothing will be written to the output.
   */
  def <<@ (attrs: (String,Any)*): this.type = {
    for ((name,value) <- attrs) <<@(name,value)
    this
  }
  
  /** Writes an opening tag with attributes derived from both
   *  the options parameter and the subsequent tuples.
   */
  def <<@ (tag: String, options: Options, attrs: (String,Any)*): this.type =
    this << "<" << tag <<@ (attributes(tag,options,attrs):_*) << ">"
  
  /** Writes an opening tag with attributes derived from both
   *  the element parameter and the subsequent tuples.
   */
  def <<@ (tag: String, element: Element, attrs: (String,Any)*): this.type =
    this << "<" << tag <<@ (attributes(tag,element,attrs):_*) << ">"
  
  /** Writes an empty tag with attributes derived from both
   *  the options parameter and the subsequent tuples.
   */
  def <<@/ (tag: String, options: Options, attrs: (String,Any)*): this.type =
    this << "<" << tag <<@ (attributes(tag,options,attrs):_*) << "/>"
    
  /** Writes an empty tag with attributes derived from both
   *  the element parameter and the subsequent tuples.
   */
  def <<@/ (tag: String, element: Element, attrs: (String,Any)*): this.type =
    this << "<" << tag <<@ (attributes(tag,element,attrs):_*) << "/>"
  
  /** Produces the complete sequence of attributes to write for the specified tag.
   */
  protected def attributes (tag: String, options: Options, attrs: Seq[(String,Any)]): Seq[(String,Any)]
  
  /** Produces the complete sequence of attributes to write for the specified tag.
   */
  protected def attributes (tag: String, element: Element, attrs: Seq[(String,Any)]): Seq[(String,Any)]
    
  /** Replaces all special XML/HTML characters
   *  with entities.
   */
  protected def escaped (str: String) = {
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
        case '\u00A0' => result append "&nbsp;"
        case '\n' => if (indented) result append Indent.current else result append "\n"; 
        case c   => result append c
      }
      i += 1
    }
    result.toString
  }

  
}