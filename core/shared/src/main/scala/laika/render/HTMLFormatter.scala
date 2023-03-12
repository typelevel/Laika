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

import laika.ast._
import laika.factory.RenderContext
import laika.rewrite.nav.PathTranslator

/** API for renderers that produce HTML output.
  *
  * @param renderChild the function to use for rendering child elements
  * @param currentElement the active element currently being rendered
  * @param parents the stack of parent elements of this formatter in recursive rendering,
  *                with the root element being the last in the list
  * @param pathTranslator translates paths of input documents to the corresponding output path
  * @param path the virtual (translated) path of the document getting rendered
  * @param indentation the indentation mechanism for this formatter
  * @param messageFilter the filter to apply before rendering runtime messages
  *
  * @author Jens Halm
  */
case class HTMLFormatter(
    renderChild: (HTMLFormatter, Element) => String,
    currentElement: Element,
    parents: List[Element],
    pathTranslator: PathTranslator,
    path: Path,
    indentation: Indentation,
    messageFilter: MessageFilter,
    closeEmptyTags: Boolean
) extends TagFormatter[HTMLFormatter](
      renderChild,
      currentElement,
      parents,
      pathTranslator,
      indentation,
      messageFilter
    ) {

  val emptyTagClosingChar: String = if (closeEmptyTags) "/" else ""

  type StyleHint = Options

  protected def withChild(element: Element): HTMLFormatter =
    copy(parents = currentElement :: parents, currentElement = element)

  protected def withIndentation(newIndentation: Indentation): HTMLFormatter =
    copy(indentation = newIndentation)

  def attributes(tag: String, styleHint: StyleHint, attrs: Seq[(String, String)]): String = {
    val id     = styleHint.id.map("id" -> _).toSeq
    val styles =
      if (styleHint.styles.isEmpty) Nil else Seq("class" -> styleHint.styles.mkString(" "))
    attributes(id ++ styles ++ attrs)
  }

  override def emptyElement(
      tagName: String,
      styleHint: StyleHint,
      attrs: (String, String)*
  ): String =
    s"<$tagName${attributes(tagName, styleHint, attrs)}$emptyTagClosingChar>"

  override def emptyElement(tagName: String): String = s"<$tagName$emptyTagClosingChar>"

}

/** Default factory for HTMLFormatters, based on a provided RenderContext.
  */
object HTMLFormatter extends (RenderContext[HTMLFormatter] => HTMLFormatter) {

  def apply(context: RenderContext[HTMLFormatter]): HTMLFormatter =
    HTMLFormatter(
      context.renderChild,
      context.root,
      Nil,
      context.pathTranslator,
      context.path,
      context.indentation,
      context.config.renderMessages,
      closeEmptyTags = false
    )

}

/** Default factory for XHTMLFormatters, based on a provided RenderContext.
  * It differs from an standard HTMLFormatter solely in that it close empty
  * tags. Therefore it offers the same API and shares the type `HTMLFormatter`
  * so that shared custom renderers can be built for HTML and XHTML.
  */
object XHTMLFormatter extends (RenderContext[HTMLFormatter] => HTMLFormatter) {

  def apply(context: RenderContext[HTMLFormatter]): HTMLFormatter =
    HTMLFormatter(
      context.renderChild,
      context.root,
      Nil,
      context.pathTranslator,
      context.path,
      context.indentation,
      context.config.renderMessages,
      closeEmptyTags = true
    )

}
