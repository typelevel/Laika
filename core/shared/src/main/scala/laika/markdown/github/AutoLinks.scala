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

package laika.markdown.github

import cats.data.NonEmptySet
import laika.parse.Parser
import laika.parse.builders._
import laika.parse.uri.AutoLinkParsers

/** Parsers for inline auto-links, which are urls or email addresses that are recognized and
  * inserted as links into the AST without any surrounding markup delimiters.
  *
  * In contrast to the rather informal description of the GitHub-Flavored-Markdown spec,
  * parsing of the http or email URIs is based on the corresponding RFCs.
  * See [[laika.parse.uri.URIParsers]] for details.
  *
  * @author Jens Halm
  */
object AutoLinks {

  private val startChars = NonEmptySet.of('*', '_', '~', '(', ' ', '\n')
  private val endChars   = NonEmptySet.of('*', '_', '~', ')', '?', '!', '.', ',', ':', ' ', '\n')

  private val reverseMarkupStart: Parser[Any] = lookAhead(eof | oneOf(startChars))
  private val afterMarkupEnd: Parser[Any] = oneOf(endChars)

  /** The parsers for http and email auto-links.
    */
  val parsers = new AutoLinkParsers(
    reverseMarkupStart,
    afterMarkupEnd,
    startChars.toSortedSet,
    endChars.toSortedSet
  )

}
