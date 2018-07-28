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

package laika.parse.markdown.html

import laika.api.ext.{ExtensionBundle, ParserConfig}
import laika.render.HTML

/**  Markdown extension that also parses verbatim HTML elements alongside
  *  the standard Markdown markup.
  *
  *  Since verbatim HTML is treated as an optional feature
  *  by this library as it aims to also support renderers for other formats than HTML,
  *  this extension is disabled by default.
  *
  *  You can enable it with the Transform API:
  *
  *  {{{
  *  val transform = Transform.from(Markdown).to(HTML).withRawContent
  *  }}}
  *
  *  @author Jens Halm
  */
object VerbatimHTML extends ExtensionBundle {

  override val useInStrictMode: Boolean = true
  override val acceptRawContent: Boolean = true

  override def parsers: ParserConfig = ParserConfig(
    blockParsers = Seq(HTMLParsers.htmlBlockFragment),
    spanParsers = Seq(HTMLParsers.htmlSpan, HTMLParsers.htmlCharRef)
  )

  override val themes = Seq(HTML.Theme(customRenderer = HTMLRenderer))

}
