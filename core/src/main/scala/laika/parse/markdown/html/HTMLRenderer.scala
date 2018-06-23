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

import laika.parse.markdown.html.HTMLElements._
import laika.render.HTMLWriter
import laika.tree.Elements.{RenderFunction, Text, TextContainer}

/**  Renderer for verbatim HTML elements. Since verbatim HTML is treated as an optional feature
  *  by this library as it aims to also support renderers for other formats than HTML,
  *  the nodes in the document tree produced by the verbatim HTML parsers are not known by the standard
  *  renderers. This partial renderer complements the regular HTML renderer and simply writes
  *  the HTML elements out as they were read. Of course, in contrast to regular text, without
  *  escaping any of the special HTML characters.
  *
  *  It must be applied explicitly as part of the `VerbatimHTML` bundle when enabling verbatim HTML:
  *
  *  {{{
  *  val transform = Transform from Markdown to HTML using VerbatimHTML
  *  }}}
  *
  *  @author Jens Halm
  */
object HTMLRenderer extends (HTMLWriter => RenderFunction) {


  def apply (out: HTMLWriter): RenderFunction = {

    def prepareAttributeValue (spans: List[TextContainer]): String =
      ("" /: spans) {
        case (acc, Text(content,_)) => acc + content.replace("&","&amp;").replace("\"","&quot;").replace("'","$#39;")
        case (acc, span) => acc + span.content
      }

    def tagStart (name: String, attributes: List[HTMLAttribute]): Unit = {
      out << "<" << name
      attributes.foreach { at =>
        out << " " << at.name
        at match {
          case HTMLAttribute(_, value, Some(char))  => out << "=" << char.toString << prepareAttributeValue(value) << char.toString
          case HTMLAttribute(_, Nil, None)          => ()
          case HTMLAttribute(_, value, None)        => out << "=" << prepareAttributeValue(value)
        }
      }
    }

    val pf: RenderFunction = {

      case HTMLElement(st @ HTMLStartTag("pre", _,_), content, _) => out << st <<< content << "</" << st.name << ">"
      case HTMLElement(startTag, content,_)     => out << startTag << content << "</" << startTag.name << ">"
      case HTMLStartTag(name, attributes,_)     => tagStart(name, attributes); out << ">"
      case HTMLEmptyElement(name, attributes,_) => tagStart(name, attributes); out << "/>"
      case HTMLEndTag(name,_)                   => out << "</" << name << ">"
      case HTMLComment(content,_)               => out << "<!--" << content << "-->"
      case HTMLCharacterReference(ref,_)        => out << ref
      case HTMLBlock(root,_)                    => out << root
    }

    pf

  }


}
