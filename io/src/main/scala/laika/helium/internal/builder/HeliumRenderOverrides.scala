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

package laika.helium.internal.builder

import laika.api.format.TagFormatter
import laika.ast.RelativePath.CurrentDocument
import laika.ast.*
import laika.helium.config.{ AnchorPlacement, HeliumIcon }
import laika.format.XSLFO.formatterSyntax.*

/** @author Jens Halm
  */
private[helium] object HeliumRenderOverrides {

  case class Tabs(tabs: Seq[Tab], options: Options = Options.empty) extends Block {
    type Self = Tabs
    def withOptions(options: Options): Tabs = copy(options = options)
  }

  case class Tab(name: String, label: String, options: Options = Options.empty) extends Span {
    type Self = Tab
    def withOptions(options: Options): Tab = copy(options = options)
  }

  case class TabContent(name: String, content: Seq[Block], options: Options = Options.empty)
      extends Block
      with BlockContainer {
    type Self = TabContent
    def withOptions(options: Options): TabContent       = copy(options = options)
    def withContent(newContent: Seq[Block]): TabContent = copy(content = newContent)
  }

  def renderChoices(
      fmt: TagFormatter,
      name: String,
      choices: List[Choice],
      options: Options
  ): String = {
    choices match {
      case Nil           => ""
      case first :: rest =>
        val tabs       = Tabs(
          Tab(first.name, first.label, Style.active) +: rest.map(c => Tab(c.name, c.label))
        )
        val content    = TabContent(first.name, first.content, Style.active) +: rest.map(c =>
          TabContent(c.name, c.content)
        )
        val allContent = BlockSequence(tabs +: content)
          .withOptions(options + Styles("tab-container"))
        fmt.indentedElement("div", allContent, "data-tab-group" -> name)
    }
  }

  private val messageLevels = Set("warning", "error", "info")

  def icon(opt: Options): Option[Icon] = (opt.styles.intersect(messageLevels)).headOption match {
    case Some("warning") => Some(HeliumIcon.warning)
    case Some("error")   => Some(HeliumIcon.error)
    case Some("info")    => Some(HeliumIcon.info)
    case _               => None
  }

  def renderCallout(fmt: TagFormatter, opt: Options, content: Seq[Block]): String = {
    val callout = icon(opt).map(SpanSequence(_)).toSeq ++ content
    fmt.indentedElement("div", BlockSequence(callout).withOptions(opt))
  }

  def htmlCalloutOptions(b: BlockSequence): Options =
    Options(b.options.id, b.options.styles - "pdf" - "epub" - "keep-together")

  def forHTML(
      anchorPlacement: AnchorPlacement
  ): PartialFunction[(TagFormatter, Element), String] = {
    case (fmt, h: Header) =>
      def link(style: String) = h.options.id.map(id =>
        SpanLink.internal(CurrentDocument(id))(HeliumIcon.link).withOptions(
          Styles("anchor-link", style)
        )
      )
      val linkedContent       = anchorPlacement match {
        case AnchorPlacement.None  => h.content
        case AnchorPlacement.Left  => link("left").toSeq ++ h.content
        case AnchorPlacement.Right => h.content ++ link("right").toSeq
      }
      fmt.newLine + fmt.element("h" + h.level.toString, h.withContent(linkedContent))

    case (fmt, InvalidSpan(msg, _, fallback, opt)) =>
      val content = SpanSequence(msg, fallback)
        .withOptions(opt + Styles("inline", msg.level.toString))
      fmt.forMessage(msg)(fmt.element("span", content))

    case (fmt, InvalidBlock(msg, _, fallback, opt)) =>
      fmt.forMessage(msg)(
        renderCallout(
          fmt,
          opt + Styles("callout", msg.level.toString),
          Seq(Paragraph(msg), fallback)
        )
      )

    case (fmt, b: BlockSequence) if b.hasStyle("callout")      =>
      renderCallout(fmt, htmlCalloutOptions(b), b.content)
    case (fmt, b: BlockSequence) if b.hasStyle("menu-content") =>
      fmt.indentedElement("nav", b)
    case (fmt, Selection(name, choices, opt)) => renderChoices(fmt, name, choices.toList, opt)

    case (fmt, tabs: Tabs)      =>
      fmt.indentedElement("ul", SpanSequence(tabs.tabs).withStyle("tab-group"))
    case (fmt, tab: TabContent) =>
      fmt.indentedElement(
        "div",
        tab.withOptions(Styles("tab-content") + tab.options),
        "data-choice-name" -> tab.name
      )
    case (fmt, tab: Tab)        =>
      val link    = SpanLink.internal(CurrentDocument())(tab.label)
      val content = SpanSequence(link).withOptions(Styles("tab") + tab.options)
      fmt.element("li", content, "data-choice-name" -> tab.name)

    case (fmt, CodeBlock("mermaid", content, _, _)) =>
      fmt.withoutIndentation(
        _.element("pre", SpanSequence(content).withStyle("mermaid"))
      )
  }

  def forPDF: PartialFunction[(TagFormatter, Element), String] = {
    case (fmt, b @ BlockSequence(content, opt)) if opt.styles.contains("callout") =>
      fmt.blockContainer(b, SpanSequence(icon(opt).toSeq, Styles("icon")) +: content)
    case (fmt, CodeBlock("mermaid", _, _, _))                                     =>
      fmt.child(
        RuntimeMessage(MessageLevel.Warning, "Mermaid diagrams are not supported for PDF output")
      )
  }

  def forEPUB: PartialFunction[(TagFormatter, Element), String] = {
    case (fmt, BlockSequence(content, opt)) if opt.styles.contains("callout") =>
      val callout = icon(opt).map(SpanSequence(_)).toSeq ++ content
      fmt.indentedElement("div", BlockSequence(callout).withOptions(opt))
    case (fmt, CodeBlock("mermaid", _, _, _))                                 =>
      fmt.child(
        RuntimeMessage(MessageLevel.Warning, "Mermaid diagrams are not supported for EPUB output")
      )
  }

}
