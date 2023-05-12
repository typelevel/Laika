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

import munit.FunSuite

class WhitespacePreprocessorSpec extends FunSuite {

  private val processor = new WhitespacePreprocessor

  test("replace form feeds and vertical tabs with single spaces") {
    val vt    = '\u000b'
    val ff    = '\f'
    val input = "text\n" + vt + "text\n" + ff + " \ntext"
    assertEquals(processor(input), "text\n text\n  \ntext")
  }

  test("remove carriage return characters") {
    assertEquals(processor("text\n\r text\n\r  \n\rtext"), "text\n text\n  \ntext")
  }

  test(
    "replace tabs with the corresponding number of whitespace characters depending on the column in occurs in"
  ) {
    val input = "\t#\n \t#\n  \t#\n   \t#\n    \t#"
    assertEquals(processor(input), "    #\n    #\n    #\n    #\n        #")
  }

  test("allow the overriding of the tabStops value") {
    val input = "\t#\n \t#\n  \t#\n   \t#\n    \t#"
    assertEquals(
      (new WhitespacePreprocessor { override val tabStops = 2 })(input),
      "  #\n  #\n    #\n    #\n      #"
    )
  }

}
