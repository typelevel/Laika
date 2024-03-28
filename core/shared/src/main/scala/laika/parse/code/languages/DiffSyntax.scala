/*
 * Copyright 2012-2024 the original author or authors.
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

package laika.parse.code.languages

import cats.data.NonEmptyList
import laika.api.bundle.SyntaxHighlighter
import laika.parse.builders.*
import laika.parse.code.{ CodeCategory, CodeSpanParser }

/** Simple highlighter for diff syntax where added and removed lines
  * are indicated with a '+' or '-' line prefix.
  *
  * @author Jens Halm
  */
object DiffSyntax extends SyntaxHighlighter {

  val language: NonEmptyList[String] = NonEmptyList.of("diff")

  private def prefixedLine(startChar: Char, category: CodeCategory): CodeSpanParser =
    CodeSpanParser.onLineStart(category) {
      (oneOf(startChar) ~ anyNot('\n')).source
    }

  val spanParsers: Seq[CodeSpanParser] = Seq(
    prefixedLine('-', CodeCategory.Diff.Removed),
    prefixedLine('+', CodeCategory.Diff.Added)
  )

}
