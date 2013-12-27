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

import laika.tree.Elements._

/** API for renderers that produce character output.
 *  
 *  @param out the function to use for writing character data 
 *  @param render the function to use for rendering child elements 
 *  @param indentItem the string to write for a single level of indentation
 *  @param newLine the new line character sequence 
 * 
 *  @author Jens Halm
 */
class TextWriter (out: String => Unit, 
                  render: Element => Unit, 
                  indentItem: String = "  ", 
                  newLine: String = "\n") {

  
  protected object Indent {
    
    var current = newLine
    
    def >>> = { current += indentItem; this }
    
    def <<< = { current = current.dropRight(indentItem.length); this }
    
    def write = { out(current); this }
    
    def indented (indent: Int, block: => Unit) = {
      if (indent > (current.length - 1) && indentItem.nonEmpty) {
        val oldIndent = current
        current = "\n" + (" " * indent)
        block
        current = oldIndent
      }
      else block
    }  
    
  }
  
  
  /** Executes the specified block while temporarily
   *  shifting the indentation level (if it is greater
   *  than the currently active one).
   */
  def indented (indent: Int)(block: => Unit) = Indent.indented(indent, block)
  
  /** Writes a new line character sequence.
   */
  def <| : this.type = { Indent.write; this }
  
  
  /** Writes the specified string to the output, on the same line.
   */
  def << (str: String): this.type = { out(str); this }
  
  /** Writes the specified elements to the output, 
   *  all on the same line.
   */
  def << (elements: Seq[Element]): this.type = { elements.foreach(render); this }

  /** Writes the specified element to the output, 
   *  on the same line.
   */
  def << (element: Element): this.type = { render(element); this }
  
  

  /** Writes the specified string to the output, 
   *  on a new line using the current level of indentation.
   */
  def <<| (str: String): this.type = { Indent.write; out(str); this }
  
  /** Writes the specified elements to the output, 
   *  each of them on a new line using the current level of indentation.
   */
  def <<| (elements: Seq[Element]): this.type = { elements.foreach(e => { Indent.write; render(e) }); this }
  
  /** Writes the specified element to the output, 
   *  on a new line using the current level of indentation.
   */
  def <<| (element: Element): this.type = { Indent.write; render(element); this }
  
   
  /** Writes the specified string to the output, 
   *  on a new line and increasing indentation one level to the right.  
   */
  def <<|> (str: String): this.type = {
    Indent.>>>.write
    out(str)
    Indent.<<<.write
    this
  }
  
  /** Writes the specified elements to the output, 
   *  each of them on a new line with the indentation increased one level to the right.
   */
  def <<|> (elements: Seq[Element]): this.type = {
    Indent.>>>
    elements.foreach { e => 
      Indent.write
      render(e)
    }
    Indent.<<<
    this
  }
  
  /** Writes the specified element to the output, 
   *  on a new line and increasing indentation one level to the right.  
   */
  def <<|> (element: Element): this.type = {
    Indent.>>>.write
    render(element)
    Indent.<<<.write
    this
  }
  
  
}

