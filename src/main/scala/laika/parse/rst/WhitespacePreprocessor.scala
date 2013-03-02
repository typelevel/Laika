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

package laika.parse.rst

import scala.annotation.tailrec

/**
 * @author Jens Halm
 */
trait WhitespacePreprocessor {

  
  val tabStops = 4
  
  
  def processWhitespace (input: String) = {
    
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