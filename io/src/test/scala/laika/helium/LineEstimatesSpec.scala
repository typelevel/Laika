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

package laika.helium

import laika.api.MarkupParser
import laika.format.Markdown
import laika.helium.builder.HeliumRewriteRules
import laika.markdown.github.GitHubFlavor
import munit.FunSuite

/**
  * @author Jens Halm
  */
class LineEstimatesSpec extends FunSuite {

  private val parser = MarkupParser
    .of(Markdown)
    .using(GitHubFlavor)
    .build
  
  def run (input: String, expected: Int): Unit = {
    val actual = parser
      .parse(input)
      .fold(_ => 0, doc => HeliumRewriteRules.estimateLines(doc.content.content))
    assertEquals(actual, expected)
  }
  
  test("count paragraph lines") {
    val input = 
      """
        |1
        |2
        |3
        |
        |4
        |5
      """.stripMargin
    run(input, 5)
  }

  test("count list item lines") {
    val input =
      """
        |* 1
        |  2
        |  3
        |
        |  4
        |  5
        |  
        |* 6
        |  7
        |    * 8
        |    * 9
      """.stripMargin
    run(input, 9)
  }

  test("count table rows") {
    val input =
      """
        || 11 | 11 | 11 |
        || 22 | 22 | 22 |
        || 33 | 33 | 33 |
      """.stripMargin
    run(input, 3)
  }
  
  test("count sections and navigation list items") {
    val input = 
      """
        |# 1
        |
        |## 1.1
        |
        |## 1.2
        |
        |# 2
        |
        |## 2.1
        |
        |## 2.2
        |
        |@:navigationTree {
        |  entries = [ { target = "#", excludeRoot = true } ]
        |}
      """.stripMargin
    run(input, 12)
  }
  
}
