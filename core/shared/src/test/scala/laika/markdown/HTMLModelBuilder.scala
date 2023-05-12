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

package laika.markdown

import laika.ast.{ Span, TextContainer }
import laika.markdown.ast._

trait HTMLModelBuilder {

  def toAttributes(attributes: (String, Span with TextContainer)*): List[HTMLAttribute] =
    attributes.toList map (a => HTMLAttribute(a._1, List(a._2), Some('\"')))

  def emptyTag(name: String, attributes: (String, Span with TextContainer)*): HTMLEmptyElement =
    HTMLEmptyElement(name, toAttributes(attributes: _*))

  def startTag(name: String, attributes: (String, Span with TextContainer)*): HTMLStartTag =
    HTMLStartTag(name, toAttributes(attributes: _*))

  def startTag(name: String, attribute: HTMLAttribute): HTMLStartTag =
    HTMLStartTag(name, List(attribute))

  def endTag(name: String): HTMLEndTag = HTMLEndTag(name)

  def element(name: String, content: Span*): HTMLElement =
    HTMLElement(HTMLStartTag(name, Nil), content.toList)

  def element(startTag: HTMLStartTag, content: Span*): HTMLElement =
    HTMLElement(startTag, content.toList)

  def charRef(str: String): HTMLCharacterReference = HTMLCharacterReference(str)

  def comment(str: String): HTMLComment = HTMLComment(str)

}
