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

import laika.bundle.{ BundleOrigin, ExtensionBundle, ParserBundle }

/** Extension bundle that enables GitHub-Flavored Markdown on top of standard Markdown.
  *
  * The extension can be added to a transformer like any other extension:
  *
  * {{{
  *   val transformer = Transformer
  *     .from(Markdown)
  *     .to(HTML)
  *     .using(GitHubFlavor)
  *     .build
  * }}}
  *
  * These are the parsers this extension adds to standard Markdown:
  *
  * - strikethrough
  * - auto-links (urls and email addresses)
  * - fenced code blocks
  * - tables
  *
  * @author Jens Halm
  */
object GitHubFlavor extends ExtensionBundle {

  val description: String = "Github-flavored Markdown"

  override val origin: BundleOrigin = BundleOrigin.Parser

  override def parsers: ParserBundle = ParserBundle(
    blockParsers = Seq(
      Tables.parser
    ) ++ FencedCodeBlocks.parsers,
    spanParsers = Seq(
      Strikethrough.parser,
      AutoLinks.parsers.www,
      AutoLinks.parsers.http,
      AutoLinks.parsers.email
    )
  )

  override def forStrictMode: Option[ExtensionBundle] = None
}
