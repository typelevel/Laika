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

/** Default renderer implementation for the Formatted AST output format.
  *
  * @author Jens Halm
  */
object ASTRenderer extends ((TextFormatter, Element) => String) {

  /**  The maximum width of a single text element.
    *  For any text that exceeds this limit only the beginning
    *  and end of the line will be displayed up to the maximum
    *  number of characters allowed. This increases readability
    *  for the majority of cases where primarily the document
    *  structure is relevant.
    */
  val maxTextWidth = 50

  private case class Content (content: Seq[Element], desc: String) extends Element with ElementContainer[Element]

  def apply (fmt: TextFormatter, element: Element): String = {

    object NoRef

    def options (opt: Options): String = {
      List(
        opt.id map ("Id("+_+")"),
        if (opt.styles.isEmpty) None else Some(opt.styles mkString ("Styles(",",",")"))
      ) filter (_.isDefined) map (_.get) mkString " + "
    }

    def attributes (attr: Iterator[Any], exclude: AnyRef = NoRef): String = {
      def prep (value: Any) = value match { case opt: Options => options(opt); case other => other }
      val it = attr.asInstanceOf[Iterator[AnyRef]]
      val res = it filter (_ ne exclude) filter (_ != NoOpt) map prep mkString ("(", ",", ")")
      if (res == "()") "" else res
    }

    def elementContainerDesc (con: ElementContainer[Element], elementType: String): String = {
      val (elements, rest) = con.productIterator partition (_.isInstanceOf[Element])
      
      val prefix = con.productPrefix + attributes(rest, con.content)
      
      val contentDesc = s" - $elementType: ${con.content.length.toString}"
      if (elements.nonEmpty) prefix + fmt.indentedChildren(elements.toList.asInstanceOf[Seq[Element]] ++ List(Content(con.content, "Content" + contentDesc)))
      else prefix + contentDesc + fmt.indentedChildren(con.content)
    }

    def textContainerDesc (con: TextContainer): String = {
      val start = con.productPrefix + attributes(con.productIterator, con.content) + " - '"

      val text = con.content.replaceAllLiterally("\n", "|")
      val len = text.length

      if (len <= maxTextWidth) start + text + "'"
      else start + text.substring(0, maxTextWidth / 2) + " [...] " + text.substring(len - maxTextWidth / 2) + "'"
    }

    def renderElement (e: Element): String = {
      val (elements, rest) = e.productIterator partition (_.isInstanceOf[Element])
      e.productPrefix + attributes(rest) + fmt.indented(_.childPerLine(elements.toList.asInstanceOf[Seq[Element]]))
    }

    def lists (desc: String, lists: (Seq[Element], String)*): String =
      desc + fmt.indented(_.childPerLine(lists map {case (elems,d) => Content(elems, d + elems.length)}))

    element match {
      case QuotedBlock(content,attr,_)    => lists("QuotedBlock", (content, "Content - Blocks: "), (attr, "Attribution - Spans: "))
      case DefinitionListItem(term,defn,_)=> lists("Item", (term, "Term - Spans: "), (defn, "Definition - Blocks: "))
      case SectionNumber(pos, opt)        => "SectionNumber" + attributes(Seq(pos.mkString("."), opt).iterator)
      case bc: BlockContainer             => elementContainerDesc(bc, "Blocks")
      case sc: SpanContainer              => elementContainerDesc(sc, "Spans")
      case tsc: TemplateSpanContainer     => elementContainerDesc(tsc, "TemplateSpans")
      case tc: TextContainer              => textContainerDesc(tc)
      case Content(content,desc)          => desc + fmt.indented(_.childPerLine(content))
      case ec: ElementContainer[_]        => elementContainerDesc(ec, "Elements")
      case e                              => renderElement(e)
    }

  }

}
