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

package laika.render

import laika.ast.*

import scala.collection.mutable

/** Base type for formatters that produce tag-based output formats like XML or HTML.
  *
  * Extends the base `Formatter` type and adds methods for writing text
  * with special characters as entities and for conveniently writing tags with attributes.
  *
  * @author Jens Halm
  */
abstract class TagFormatter extends Formatter {

  type Rep = TagFormatter

  protected def self: Rep = this

  /** Renders the specified string on the same line,
    * with all special XML/HTML characters converted to entities.
    */
  def text(str: String): String = TagFormatter.escape(str, newLine)

  /** Renders an HTML/XML comment.
    */
  def comment(content: String): String = s"<!-- $content -->"

  /** Renders an element with the specified tag name, attributes derived from the container options
    * and nested content consisting of the children of the container, all rendered on the same line.
    */
  def element(
      tagName: String,
      container: ElementContainer[_ <: Element],
      attrs: (String, String)*
  ): String =
    s"<$tagName${attributes(tagName, container, attrs)}>${children(container.content)}</$tagName>"

  /** Renders an element with the specified tag name, attributes derived from the container options
    * and indented content consisting of the children of the container.
    */
  def indentedElement(
      tagName: String,
      container: ElementContainer[_ <: Element],
      attrs: (String, String)*
  ): String =
    s"<$tagName${attributes(tagName, container, attrs)}>${indentedChildren(container.content)}$newLine</$tagName>"

  /** Renders an element with the specified tag name, attributes derived from the style hint
    * and content based on the provided string that is interpreted as already rendered in the target format.
    * That means that no character escaping will be performed on the provided content.
    */
  def rawElement(
      tagName: String,
      styleHint: Element,
      content: String,
      attrs: (String, String)*
  ): String =
    s"<$tagName${attributes(tagName, styleHint, attrs)}>$content</$tagName>"

  /** Renders a text element with the specified tag name, attributes derived from the container options
    * and content based on the container's content that gets rendered with all special XML/HTML
    * characters converted to entities.
    */
  def textElement(
      tagName: String,
      container: TextContainer,
      attrs: (String, String)*
  ): String =
    s"<$tagName${attributes(tagName, container, attrs)}>${text(container.content)}</$tagName>"

  /** Renders an empty element with the specified tag name and attributes derived from the style hint.
    */
  def emptyElement(tagName: String, styleHint: Element, attrs: (String, String)*): String =
    s"<$tagName${attributes(tagName, styleHint, attrs)}/>"

  /** Renders an empty element with the specified tag name.
    */
  def emptyElement(tagName: String): String = s"<$tagName/>"

  /** Renders all attributes derived from the style hint and the explicitly provided attributes.
    */
  def attributes(tag: String, styleHint: Element, attrs: Seq[(String, String)]): String

  /** Renders the specified attributes (passed as name-value tuples),
    * including a preceding space character.
    */
  def attributes(attrs: Seq[(String, String)]): String =
    attrs.map(t => attribute(t._1, t._2)).mkString

  /** Filters empty values from the provided list of name-value pairs.
    */
  def optAttributes(attrs: (String, Option[String])*): Seq[(String, String)] = attrs.collect {
    case (name, Some(value)) => (name, value)
  }

  /** Renders the specified attribute including a preceding space character.
    */
  def attribute(name: String, value: String): String = s""" $name="$value""""

}

private[laika] object TagFormatter {

  /** Replaces all special XML/HTML characters with entities. */
  def escape(str: String, newLine: String = "\n"): String = {
    var i      = 0
    val end    = str.length
    val result = new mutable.StringBuilder
    while (i < end) {
      str.charAt(i) match {
        case '<'      => result.append("&lt;")
        case '>'      => result.append("&gt;")
        case '"'      => result.append("&quot;")
        case '\''     => result.append("&#39;")
        case '&'      => result.append("&amp;")
        case '\u00A0' => result.append("&nbsp;")
        case '\n'     => result.append(newLine)
        case c        => result.append(c)
      }
      i += 1
    }
    result.toString
  }

}
