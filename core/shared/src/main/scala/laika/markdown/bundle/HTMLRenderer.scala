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

package laika.markdown.bundle

import laika.api.format.TagFormatter
import laika.ast.{ Element, TextContainer }
import laika.markdown.ast.*

/** Renderer for verbatim HTML elements.
  * Since verbatim HTML is treated as an optional feature by this library
  * as it aims to also support renderers for other formats than HTML,
  * the nodes in the document tree produced by the verbatim HTML parsers are not known
  * by the standard renderers.
  * This partial renderer complements the regular HTML renderer and simply writes
  * the HTML elements out as they were read.
  * Of course, in contrast to regular text, without escaping any of the special HTML characters.
  *
  *  @author Jens Halm
  */
private[bundle] object HTMLRenderer {

  private def prepareAttributeValue(spans: List[TextContainer]): String =
    spans.foldLeft("") {
      case (acc, span: HTMLCharacterReference) => acc + span.content
      case (acc, text)                         =>
        acc + text.content.replace("&", "&amp;").replace("\"", "&quot;").replace("'", "$#39;")
    }

  private def tagStart(tagName: String, attributes: List[HTMLAttribute]): String = {

    val renderedAttrs = attributes.map { at =>
      val name  = " " + at.name
      val value = at match {
        case HTMLAttribute(_, value, Some(char)) => s"=$char${prepareAttributeValue(value)}$char"
        case HTMLAttribute(_, Nil, None)         => ""
        case HTMLAttribute(_, value, None)       => "=" + prepareAttributeValue(value)
      }
      name + value
    }.mkString
    s"<$tagName$renderedAttrs"
  }

  val custom: PartialFunction[(TagFormatter, Element), String] = {

    case (fmt, HTMLElement(st @ HTMLStartTag("pre", _, _), content, _)) =>
      fmt.child(st) + fmt.withoutIndentation(_.children(content)) + s"</${st.name}>"
    case (fmt, HTMLElement(startTag, content, _))                       =>
      fmt.child(startTag) + fmt.children(content) + s"</${startTag.name}>"
    case (_, HTMLStartTag(name, attributes, _))     => tagStart(name, attributes) + ">"
    case (_, HTMLEmptyElement(name, attributes, _)) => tagStart(name, attributes) + "/>"
    case (_, HTMLEndTag(name, _))                   => s"</$name>"
    case (_, HTMLComment(content, _))               => s"<!--$content-->"
    case (_, HTMLScriptElement(attr, content, _))   =>
      tagStart("script", attr) + s">$content</script>"
    case (_, HTMLCharacterReference(ref, _))        => ref
    case (fmt, HTMLBlock(root, _))                  => fmt.child(root)
  }

}
