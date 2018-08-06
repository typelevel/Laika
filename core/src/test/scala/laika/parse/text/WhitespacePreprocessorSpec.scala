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

package laika.parse.text

import org.scalatest.{FlatSpec, Matchers}

class WhitespacePreprocessorSpec extends FlatSpec
                                 with Matchers {

  
  "The whitespace preprocessor" should "replace form feeds and vertical tabs with single spaces" in {
    val vt = '\u000b'
    val ff = '\f'
    val input = "text\n"+vt+"text\n"+ff+" \ntext"
    (new WhitespacePreprocessor)(input) should be ("text\n text\n  \ntext")
  }
  
  it should "remove carriage return characters" in {
    (new WhitespacePreprocessor)("text\n\r text\n\r  \n\rtext") should be ("text\n text\n  \ntext")
  }
  
  it should "replace tabs with the corresponding number of whitespace characters depending on the column in occurs in" in {
    val input = "\t#\n \t#\n  \t#\n   \t#\n    \t#"
    (new WhitespacePreprocessor)(input) should be ("    #\n    #\n    #\n    #\n        #")
  }
  
  it should "allow the overriding of the tabStops value" in {
    val input = "\t#\n \t#\n  \t#\n   \t#\n    \t#"
    (new WhitespacePreprocessor{override val tabStops = 2})(input) should be ("  #\n  #\n    #\n    #\n      #")
  }
  

}
