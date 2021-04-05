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

package laika.parse.text

import laika.parse.SourceCursor
import laika.parse.markup.DocumentParser.DocumentInput

/** Processes whitespace, removing or replacing most whitespace characters except
 *  for newline and space.
 * 
 *  It modifies string input in the following ways:
 * 
 *  - Replaces all occurrences of tabs with the corresponding number of spaces,
 *    depending on the column the tab is placed in and the configured `tabStops` value.
 * 
 *  - Removes any return character.
 * 
 *  - Replaces form feed and vertical tab with spaces.
 * 
 *  The processor should run on text input before it is passed to the actual
 *  parsers as they would not be able to deal with tabs properly.
 * 
 * @author Jens Halm
 */
class WhitespacePreprocessor extends (String => String) {

  
  /** The number of columns between tab stops.
   */
  val tabStops = 4
  

  /** Processes whitespace, removing or replacing most whitespace characters except
   *  for newline and space.
   */
  def apply (input: String): String = {
    
    val end = input.length
    val buf = new StringBuilder

    var i = 0
    var col = 0
    var cur = 0
    
    def cut (add: String) = {
      buf ++= input.substring(cur, i) ++= add
      cur = i + 1
    }
    
    while (i < end) {
      input.charAt(i) match {
        case '\f' | '\u000b' => 
          cut(" ")
          col += 1
        case '\r' =>
          cut("")
        case '\n' =>
          col = 0
        case '\t' => 
          val spaces = tabStops - (col % tabStops)
          cut(" " * spaces)
          col += spaces
        case _    => 
          col += 1
      }
      i += 1
    }
    
    buf ++= input.substring(cur)
    buf.toString
  }
  
  
}

/** Companion for creating instances of WhitespacePreprocessor.
  */
object WhitespacePreprocessor {

  /** Processes all whitespace in the specified string.
    */
  val forString: String => String = new WhitespacePreprocessor

  /** Processes the specified Input instance and returns
    * a new instance with the same path, but all whitespace
    * pre-processed.
    */
  val forInput: DocumentInput => DocumentInput = { input =>
    val raw = input.source.input
    val preprocessed = forString(raw.toString)
    input.copy(source = SourceCursor(preprocessed, input.path))
  }

}
