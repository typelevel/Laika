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

package laika.helium.config

import laika.ast.Path.Root
import laika.ast.RelativePath.CurrentDocument
import laika.ast._
import laika.render.HTMLFormatter

/**
  * @author Jens Halm
  */
object HeliumRenderOverrides {

  case class Tabs (tabs: Seq[Tab], options: Options = NoOpt) extends Block {
    type Self = Tabs
    def withOptions(options: Options): Tabs = copy(options = options)
  }
  case class Tab (name: String, label: String, options: Options = NoOpt) extends Span {
    type Self = Tab
    def withOptions(options: Options): Tab = copy(options = options)
  }
  case class TabContent (name: String, content: Seq[Block], options: Options = NoOpt) extends Block {
    type Self = TabContent
    def withOptions(options: Options): TabContent = copy(options = options)
  }

  def renderChoices (fmt:HTMLFormatter, name: String, choices: Seq[Choice], options: Options): String = {
    val tabs = Tabs(choices.map(c => Tab(c.name, c.label)))
    val content = choices.map(c => TabContent(c.name, c.content))
    fmt.indentedElement("div", options + Styles("tab-container"), tabs +: content, "data-tab-group" -> name)
  }
  
  def create (anchorPlacement: AnchorPlacement): PartialFunction[(HTMLFormatter, Element), String] = {
    case (fmt, Header(level, content, opt)) =>
      def link = opt.id.map(id => SpanLink(Seq(SpanSequence(Nil, Styles("anchor"))), InternalTarget(Root, CurrentDocument(id)), options = Styles("anchor-link")))
      val linkedContent = anchorPlacement match {
        case AnchorPlacement.None => content
        case AnchorPlacement.Left => link.toSeq ++ content
        case AnchorPlacement.Right => content ++ link.toSeq
      }
      fmt.newLine + fmt.element("h"+level.toString, opt, link.toSeq ++ linkedContent)
    case (fmt, BlockSequence(content, opt)) if opt.styles.contains("callout") =>
      val iconStyle = (opt.styles - "callout").headOption match {
        case Some("warning") => Some("icofont-close-circled")
        case Some("error") => Some("icofont-close-circled")
        case Some("info") => Some("icofont-close-circled")
        case _ => None
      }
      val icon = iconStyle.map(st => SpanSequence(Nil, Styles("icon", "icofont-xlg", st)))
      fmt.indentedElement("div", opt, icon.toSeq ++ content)
    case (fmt, Selection(name, choices, opt)) => renderChoices(fmt, name, choices, opt)
      
    case (fmt, tabs: Tabs)      => fmt.indentedElement("ul", Styles("tab-group"), tabs.tabs)
    case (fmt, tab: TabContent) => fmt.indentedElement("div", Styles("tab-content"), tab.content, "data-choice-name" -> tab.name)
    case (fmt, tab: Tab)        => fmt.element("li", Styles("tab"), Seq(Text(tab.label)), "data-choice-name" -> tab.name)
  }
  
}
