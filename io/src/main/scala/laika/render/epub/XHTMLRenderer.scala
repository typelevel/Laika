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

package laika.render.epub

import laika.ast.*
import laika.render.{ HTMLRenderer, TagFormatter }

/** Customizations of the default HTML renderer for AST elements where attributes specific to EPUB need to be rendered.
  *
  *  @author Jens Halm
  */
private[laika] object XHTMLRenderer extends HTMLRenderer(format = "epub") {

  def renderChoices(
      fmt: TagFormatter,
      choices: Seq[Choice],
      options: Options
  ): String = {
    val content = choices.flatMap { choice =>
      Paragraph(Strong(Text(choice.label))) +: choice.content
    }
    fmt.child(BlockSequence(content, options))
  }

  override def apply(fmt: TagFormatter, element: Element): String = element match {

    case CitationLink(ref, label, opt) =>
      val text = Text(s"[$label]").withOptions(opt + Style.citation)
      fmt.textElement("a", text, "href" -> ("#" + ref), "epub:type" -> "noteref")
    case FootnoteLink(ref, label, opt) =>
      val text = Text(s"[$label]").withOptions(opt + Style.footnote)
      fmt.textElement("a", text, "href" -> ("#" + ref), "epub:type" -> "noteref")

    case c: Citation =>
      fmt.indentedElement("aside", c.mergeOptions(Style.citation), "epub:type" -> "footnote")

    case f: Footnote =>
      fmt.indentedElement("aside", f.mergeOptions(Style.footnote), "epub:type" -> "footnote")

    case Selection(_, choices, opt) =>
      renderChoices(fmt, choices, opt)

    case _ =>
      super.apply(fmt, element)
  }

}
