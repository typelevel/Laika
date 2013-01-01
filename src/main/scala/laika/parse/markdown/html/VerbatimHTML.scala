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

package laika.parse.markdown.html

import laika.render.HTMLWriter
import laika.tree.Elements.Element
import laika.parse.markdown.html.VerbatimHTMLElements._
   

/** Renderer for verbatim HTML elements. Since verbatim HTML is treated as an optional feature
 *  by this library as it aims to also support renderers for other formats than HTML,
 *  the nodes in the document tree produced by the verbatim HTML parsers are not known by the standard
 *  renderers. This partial renderer complements the regular HTML renderer and simply writes
 *  the HTML elements out as they were read. Of course, in contrast to regular text, without 
 *  escaping any of the special HTML characters. 
 *  
 *  It must be applied explicitly when enabling verbatim HTML:
 *  
 *  {{{
 *  val transform = Transform from (Markdown withVerbatimHTML) to HTML rendering VerbatimHTML
 *  }}}
 * 
 *  @author Jens Halm
 */
class VerbatimHTML extends (HTMLWriter => PartialFunction[Element, Unit]) {

  
  def apply (out: HTMLWriter): PartialFunction[Element, Unit] = {
    
    def tagStart (name: String, attributes: List[HTMLAttribute]) = {
      out << "<" << name
      attributes.foreach { at =>
        out << " " << at.name
        at match {
          case HTMLAttribute(_, value, Some(char)) 	=> out << "=" << char.toString << value << char.toString 
          case HTMLAttribute(_, Nil, None) 					=> () 
          case HTMLAttribute(_, value, None) 			  => out << "=" << value 
        }
      }
    }
    
    val pf: PartialFunction[Element, Unit] = {
	      
	    case HTMLElement(startTag, content) 		=> out << startTag << content << "</" << startTag.name << ">" 
	    case HTMLStartTag(name, attributes) 		=> tagStart(name, attributes); out << ">"
	    case HTMLEmptyElement(name, attributes) => tagStart(name, attributes); out << "/>"
	    case HTMLEndTag(name) 									=> out << "</" << name << ">"
	    case HTMLComment(content) 							=> out << "<!--" << content << "-->"
	    case HTMLCharacterReference(ref) 				=> out << ref
	    case HTMLBlock(root) 										=> out << root 
    } 
    
    pf
      
  }
  
  
  
}

object VerbatimHTML extends VerbatimHTML