/*
 * Copyright 2013-2018 the original author or authors.
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

import laika.bundle.{ExtensionBundle, ParserBundle}

/** Extension bundle that enables GitHub-Flavored Markdown on top of standard Markdown.
  *
  * The extension can be added to a transformation like any other extension:
  *
  * {{{
  *   Transform
  *     .from(Markdown).to(HTML)
  *     .using(GitHubFlavor)
  *     .fromFile("hello.md").toFile("hello.html")
  * }}}
  *
  * These are the parsers this extension adds to standard Markdown:
  *
  * - strikethrough - (TODO)
  * - auto-links (urls and email addresses) - (TODO)
  * - fenced code blocks - (TODO)
  * - tables - (TODO)
  *
  * @author Jens Halm
  */
object GitHubFlavor extends ExtensionBundle {

  override def parsers: ParserBundle = ParserBundle(
    blockParsers = Nil,
    spanParsers = Nil
  )

}
