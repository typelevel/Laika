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

package laika.parse.code.common

import laika.parse.text.TextParsers._
import laika.parse.code.{CodeCategory, CodeSpanParser}

/** Configurable base parsers for comments in code blocks.
  * 
  * @author Jens Halm
  */
object Comment {
  
  /** Parses a single line comment from the specified start delimiter to the end of the line. */
  def singleLine (start: String): CodeSpanParser = {
    require(start.nonEmpty)
    CodeSpanParser(CodeCategory.Comment) {
      start ~> restOfLine ^^ { text => start + text + "\n" }
    }
    // TODO - 0.14 - create restOfLine variant that does not consume final \n or use anyBut('\n') directly
  }

  /** Parses a multi-line comment enclosed by the specified start and end delimiters. */
  def multiLine (start: String, end: String): CodeSpanParser = {
    require(start.nonEmpty)
    CodeSpanParser(CodeCategory.Comment) {
      start ~> delimitedBy(end) ^^ { text => start + text + end }
    }
  }
  
}
