/*
 * Copyright 2012-2023 the original author or authors.
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

package laika.internal.render

import laika.api.format.{ Formatter, TagFormatter }
import laika.ast.*

private[laika] class HTMLFormatter(
    closeEmptyTags: Boolean,
    protected val context: Formatter.Context[TagFormatter]
) extends TagFormatter {

  protected def withChild(element: Element): Rep =
    new HTMLFormatter(closeEmptyTags, context.forChildElement(element))

  protected def withIndentation(newIndentation: Formatter.Indentation): Rep = new HTMLFormatter(
    closeEmptyTags,
    context.withIndentation(newIndentation)
  )

  private val emptyTagClosingChar: String = if (closeEmptyTags) "/" else ""

  def attributes(tag: String, styleHint: Element, attrs: Seq[(String, String)]): String = {
    val id     = styleHint.options.id.map("id" -> _).toSeq
    val styles =
      if (styleHint.options.styles.isEmpty) Nil
      else Seq("class" -> styleHint.options.styles.mkString(" "))
    attributes((id ++ styles ++ attrs) *)
  }

  override def emptyElement(
      tagName: String,
      styleHint: Element,
      attrs: (String, String)*
  ): String =
    s"<$tagName${attributes(tagName, styleHint, attrs)}$emptyTagClosingChar>"

}
