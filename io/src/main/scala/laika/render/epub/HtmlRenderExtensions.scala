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

package laika.render.epub

import laika.ast._
import laika.render.HTMLFormatter

/** Customizations of the default HTML renderer for AST elements where attributes
  * specific to EPUB need to be rendered.
  *
  *  @author Jens Halm
  */
object HtmlRenderExtensions {

  val all: PartialFunction[(HTMLFormatter, Element), String] = {
    case (fmt, CitationLink(ref,label,opt)) => fmt.textElement("a", opt + Style.citation, "[" + label + "]", "href" -> ("#"+ref), "epub:type" -> "noteref")
    case (fmt, FootnoteLink(ref,label,opt)) => fmt.textElement("a", opt + Style.footnote, "[" + label + "]", "href" -> ("#"+ref), "epub:type" -> "noteref")
    case (fmt, Citation(_,content,opt)) => fmt.indentedElement("aside", opt + Style.citation, content, "epub:type" -> "footnote")
    case (fmt, Footnote(_,content,opt)) => fmt.indentedElement("aside", opt + Style.footnote, content, "epub:type" -> "footnote")
  }

}
