/*
 * Copyright 2013 the original author or authors.
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

package laika.render

import laika.tree.Elements._
import laika.io.Output
  
/** A renderer for HTML output. May be directly passed to the `Render` or `Transform` APIs:
 * 
 *  {{{
 *  Render as HTML from document toFile "hello.html"
 *  
 *  Transform from Markdown to HTML fromFile "hello.md" toFile "hello.html"
 *  }}}
 * 
 *  @author Jens Halm
 */
class HTML extends ((Output, Element => Unit) => (HTMLWriter, Element => Unit)) {
 
  
  /** The actual setup method for providing both the writer API for customized
   *  renderers as well as the actual default render function itself. The default render
   *  function always only renders a single element and then delegates to the composite
   *  renderer passed to this function as a parameter when rendering children. This way
   *  user customizations are possible on a per-element basis.
   *  
   *  @param output the output to write to
   *  @param render the composite render function to delegate to when elements need to render their children
   *  @return a tuple consisting of the writer API for customizing
   *  the renderer as well as the actual default render function itself
   */
  def apply (output: Output, render: Element => Unit) = {
    val out = new HTMLWriter(output asFunction, render)  
    (out, renderElement(out))
  }

  private def renderElement (out: HTMLWriter)(elem: Element): Unit = {
    
    elem match {
      case Document(content)          => out <<        "<div>" <<|> content <<| "</div>"       
      case QuotedBlock(content, _)    => out << "<blockquote>" <<|> content <<| "</blockquote>"
      case UnorderedList(content)     => out <<         "<ul>" <<|> content <<| "</ul>"
      case OrderedList(content,_,_,_,_) => out <<         "<ol>" <<|> content <<| "</ol>"
      case CodeBlock(content)         => out <<  "<code><pre>" <<<& content <<  "</pre></code>"
      case Section(header, content)   => out <<         header <<|  content
      case Paragraph(content)         => out <<          "<p>" <<   content <<  "</p>"  
      case ListItem(content)          => out <<         "<li>" <<   content <<  "</li>"
      case Emphasized(content)        => out <<         "<em>" <<   content <<  "</em>" 
      case Strong(content)            => out <<     "<strong>" <<   content <<   "</strong>" 
      case CodeSpan(content)          => out <<       "<code>" <<<& content <<   "</code>" 
      case Text(content)              => out                   <<&  content
      case FlowContent(content)       => out                   <<   content
      case Rule                       => out << "<hr>"
      case LineBreak                  => out << "<br>"
      case Header(level, content)     => out <<| "<h" << level.toString << ">" << content << "</h" << level.toString << ">"
      case Link(content, url, title)  => out << "<a"   <<@ ("href",url) <<@ ("title",title) << ">" << content << "</a>"
      case Image(text, url, title)    => out << "<img" <<@ ("src", url) <<@ ("alt",text) <<@ ("title",title) << ">"

      case LinkReference(content, id, inputPrefix, inputPostfix)  => out << inputPrefix << content << inputPostfix 
      case ImageReference(text, id, inputPrefix, inputPostfix)    => out << inputPrefix << text    << inputPostfix

      case sc: SpanContainer[_]       => out << "<span>" <<  sc.content << "</span>"
      case bc: BlockContainer[_]      => out << "<div>" <<|> bc.content << "</div>"
      case unknown                    => ()  
    }  
  } 
}

/** The default instance of the HTML renderer.
 */
object HTML extends HTML
