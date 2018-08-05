/*
 * Copyright 2013-2018 the original author or authors.
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
class ASTRenderer (out: TextWriter) {

  /**  The maximum width of a single text element.
    *  For any text that exceeds this limit only the beginnig
    *  and end of the line will be displayed up to the maximum
    *  number of characters allowed. This increases readability
    *  for the majority of cases where primarily the document
    *  structure is relevant.
    */
  val maxTextWidth = 50

  private case class Content (content: Seq[Element], desc: String) extends Element with ElementContainer[Element,Content]

  def render (element: Element): Unit = {

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

    def elementContainerDesc (con: ElementContainer[Element,_], elementType: String): Unit = {
      val (elements, rest) = con.productIterator partition (_.isInstanceOf[Element])
      out << con.productPrefix << attributes(rest, con.content)

      val contentDesc = s" - $elementType: ${con.content.length.toString}"
      if (elements.nonEmpty) out <<|> (elements.toList.asInstanceOf[Seq[Element]] ++ List(Content(con.content, "Content" + contentDesc)))
      else out << contentDesc <<|> con.content
    }

    def textContainerDesc (con: TextContainer): Unit = {
      out << con.productPrefix << attributes(con.productIterator, con.content) << " - '"

      val text = con.content.replaceAllLiterally("\n", "|")
      val len = text.length

      if (len <= maxTextWidth) out << text << "'"
      else out << text.substring(0, maxTextWidth / 2) << " [...] " << text.substring(len - maxTextWidth / 2) << "'"
    }

    def renderElement (e: Element): Unit = {
      val (elements, rest) = e.productIterator partition (_.isInstanceOf[Element])
      out << e.productPrefix << attributes(rest)
      if (elements.nonEmpty) out <<|> elements.toList.asInstanceOf[Seq[Element]]
    }

    def lists (desc: String, lists: (Seq[Element], String)*): Unit =
      out << desc <<|> (lists map {case (elems,d) => Content(elems, d + elems.length)})

    element match {
      case QuotedBlock(content,attr,_)    => lists("QuotedBlock", (content, "Content - Blocks: "), (attr, "Attribution - Spans: "))
      case DefinitionListItem(term,defn,_)=> lists("Item", (term, "Term - Spans: "), (defn, "Definition - Blocks: "))
      case SectionNumber(pos, opt)        => out << "SectionNumber" << attributes(Seq(pos.mkString("."), opt).toIterator)
      case bc: BlockContainer[_]          => elementContainerDesc(bc, "Blocks")
      case sc: SpanContainer[_]           => elementContainerDesc(sc, "Spans")
      case tc: TextContainer              => textContainerDesc(tc)
      case Content(content,desc)          => out << desc <<|> content
      case ec: ElementContainer[_,_]      => elementContainerDesc(ec, "Elements")
      case e                              => renderElement(e)
    }

  }

}
