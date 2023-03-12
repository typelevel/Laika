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

package laika.markdown.ast

import laika.ast._

/** A top level HTML block as defined by the Markdown syntaxt description. It is surrounded by blank lines
  *  and has a block-level element (one that is not classified as "phrasing content" in the HTML specification)
  *  as its root element. It may contain other nested HTML elements and tags, but no spans produced by standard
  *  Markdown markup.
  */
case class HTMLBlock(root: HTMLElement, options: Options = NoOpt) extends Block {
  type Self = HTMLBlock
  def withOptions(options: Options): HTMLBlock = copy(options = options)
}

/** Base class for all verbatim HTML span elements.
  */
abstract class HTMLSpan extends Span

/** Represents a full HTML element with matching start and end tags. The content of this span container
  *  may contain further nested HTML elements and tags as well as simple text elements.
  */
case class HTMLElement(startTag: HTMLStartTag, content: Seq[Span], options: Options = NoOpt)
    extends HTMLSpan with SpanContainer {
  type Self = HTMLElement
  def withContent(newContent: Seq[Span]): HTMLElement = copy(content = newContent)
  def withOptions(options: Options): HTMLElement      = copy(options = options)
}

/** Represent a start tag. When this element is part of a final document tree, it represents
  *  an orphaned start tag without matching end tag. In HTML this may be legal (some tags like the p
  *  tag are defined as "auto-closing" under certain circumstances). This library however does not
  *  implement the full logic of a proper HTML parser to distinguish between legal and faulty
  *  occurrences of unmatched start tags.
  */
case class HTMLStartTag(name: String, attributes: List[HTMLAttribute], options: Options = NoOpt)
    extends HTMLSpan with Block {
  type Self = HTMLStartTag
  def withOptions(options: Options): HTMLStartTag = copy(options = options)
}

/** Represents an empty element (like `&lt;br/&gt;` or `&lt;hr/&gt;`) in case it contains the explicit
  *  slash to mark it as closed. Otherwise it will be classified as a start tag.
  */
case class HTMLEmptyElement(name: String, attributes: List[HTMLAttribute], options: Options = NoOpt)
    extends HTMLSpan with Block {
  type Self = HTMLEmptyElement
  def withOptions(options: Options): HTMLEmptyElement = copy(options = options)
}

/** Represents an orphaned end tag without matching start tag.
  */
case class HTMLEndTag(name: String, options: Options = NoOpt) extends HTMLSpan {
  type Self = HTMLEndTag
  def withOptions(options: Options): HTMLEndTag = copy(options = options)
}

/** Represents a standard HTML comment.
  */
case class HTMLComment(content: String, options: Options = NoOpt) extends HTMLSpan with Block
    with TextContainer {
  type Self = HTMLComment
  def withOptions(options: Options): HTMLComment = copy(options = options)
}

/** Represents a script element.
  */
case class HTMLScriptElement(
    attributes: List[HTMLAttribute],
    content: String,
    options: Options = NoOpt
) extends HTMLSpan with TextContainer {
  type Self = HTMLScriptElement
  def withOptions(options: Options): HTMLScriptElement = copy(options = options)
}

/** Represents a numerical or named character reference.
  */
case class HTMLCharacterReference(content: String, options: Options = NoOpt) extends HTMLSpan
    with TextContainer {
  type Self = HTMLCharacterReference
  def withOptions(options: Options): HTMLCharacterReference = copy(options = options)
}

/** Represents a single HTML attribute. The value is provided as a list of TextContainers
  * as it may contain HTML character references alongside regular Text elements.
  */
case class HTMLAttribute(name: String, value: List[TextContainer], quotedWith: Option[Char])
