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

package laika.render.epub

import laika.ast._
import laika.render.HTMLWriter

/** Customizations of the default HTML renderer for AST elements where attributes
  * specific to EPUB need to be rendered.
  *
  *  @author Jens Halm
  */
object HtmlRenderExtensions {

  /** Function that can be added to any HTML-based `Theme` that
    * contains renderers for all AST elements where attributes
    * specific to EPUB need to be rendered.
    */
  val all: HTMLWriter => RenderFunction = out => {
    case CitationLink(ref,label,opt) => out <<@ ("a",opt + Styles("citation"),"href"->("#"+ref),"epub:type"->"noteref") << "[" << label << "]</a>"
    case FootnoteLink(ref,label,opt) => out <<@ ("a",opt + Styles("footnote"),"href"->("#"+ref),"epub:type"->"noteref") << "[" << label << "]</a>"
    case Citation(_,content,opt) => out <<@ ("aside",opt + Styles("citation"),"epub:type"->"footnote") <<|> content <<| "</aside>"
    case Footnote(_,content,opt) => out <<@ ("aside",opt + Styles("footnote"),"epub:type"->"footnote") <<|> content <<| "</aside>"
  }

}
