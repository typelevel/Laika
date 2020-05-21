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
import laika.rst.ast.RstStyle

/** Default renderer implementation for the HTML output format.
  *
  * @author Jens Halm
  */
class HTMLRenderer (fileSuffix: String) extends ((HTMLFormatter, Element) => String) {


  def apply (fmt: HTMLFormatter, element: Element): String = {

    def noneIfDefault [T](actual: T, default: T): Option[String] = if (actual == default) None else Some(actual.toString)

    def renderBlocks (tagName: String, options: Options, content: Seq[Block], attrs: (String, String)*): String = content match {
      case Seq(ss: SpanSequence)     => fmt.element(tagName, options, Seq(ss), attrs: _*)
      case Seq(Paragraph(spans,opt)) => fmt.element(tagName, options, Seq(SpanSequence(spans,opt)), attrs: _*)
      case other                     => fmt.indentedElement(tagName, options, other, attrs: _*)
    }

    def renderTable (table: Table): String = {
      val children = List(table.caption,table.columns,table.head,table.body) filterNot (_.content.isEmpty)
      fmt.indentedElement("table", table.options, children)
    }
    
    def navigationToBulletList (navList: NavigationList): BulletList = {
      
      val bullet = StringBullet("*")
      
      def transformItems (items: Seq[NavigationItem]): Seq[BulletListItem] = {
        items.map { item =>
          val target: Paragraph = item match {
            case nh: NavigationHeader => Paragraph(nh.title.content, nh.options + Style.navHeader)
            case nl: NavigationLink   => 
              val styles = if (nl.selfLink) Style.active else NoOpt
              Paragraph(Seq(SpanLink(nl.title.content, nl.target)), nl.options + styles)
          }
          val children = if (item.content.isEmpty) Nil
          else Seq(BulletList(transformItems(item.content), bullet))
          BulletListItem(target +: children, bullet)
        }
      }
      
      BulletList(transformItems(navList.content), bullet)
    }

    object WithFallback {
      def unapply (value: Element): Option[Element] = value match {
        case f: Fallback => Some(f.fallback)
        case _ => None
      }
    }

    def renderBlockContainer (con: BlockContainer): String = {

      def toTable (label: String, content: Seq[Block], options: Options): Table = {
        val left = BodyCell(SpanSequence(s"[$label]"))
        val right = BodyCell(content)
        val row = Row(List(left,right))
        Table(TableHead(Nil), TableBody(List(row)), Caption(),
          Columns.options(Style.label,NoOpt), options)
      }

      def quotedBlockContent (content: Seq[Block], attr: Seq[Span]): Seq[Block] =
        if (attr.isEmpty) content
        else content :+ Paragraph(attr, Style.attribution)

      def figureContent (img: Span, caption: Seq[Span], legend: Seq[Block]): List[Block] =
        List(SpanSequence(img), Paragraph(caption, Style.caption), BlockSequence(legend, Style.legend))

      con match {
        case RootElement(content, _)          => fmt.childPerLine(content)
        case EmbeddedRoot(content,indent,_)   => fmt.withMinIndentation(indent)(_.childPerLine(content))
        case Section(header, content,_)       => fmt.childPerLine(header +: content)
        case TitledBlock(title, content, opt) => fmt.indentedElement("div", opt, Paragraph(title, Style.title) +: content)
        case QuotedBlock(content,attr,opt)    => renderBlocks("blockquote", opt, quotedBlockContent(content,attr))
        case BulletListItem(content,_,opt)    => renderBlocks("li", opt, content)
        case EnumListItem(content,_,_,opt)    => renderBlocks("li", opt, content)
        case DefinitionListItem(term,defn,_)  => fmt.element("dt", NoOpt, term) + fmt.newLine + renderBlocks("dd", NoOpt, defn)
        case Figure(img,caption,legend,opt)   => fmt.indentedElement("div", opt + Style.figure, figureContent(img,caption,legend))

        case Footnote(label,content,opt)      => renderTable(toTable(label,content,opt + Style.footnote))
        case Citation(label,content,opt)      => renderTable(toTable(label,content,opt + Style.citation))

        case WithFallback(fallback)           => fmt.child(fallback)
        case c: Customizable                  => c match {
          case BlockSequence(content, NoOpt)  => fmt.childPerLine(content) // this case could be standalone above, but triggers a compiler bug then
          case _                              => fmt.indentedElement("div", c.options, c.content)
        }
        case unknown                          => fmt.indentedElement("div", NoOpt, unknown.content)
      }
    }

    def renderSpanContainer (con: SpanContainer): String = {
      
      def codeStyles (language: String, hasHighlighting: Boolean) = 
        if (hasHighlighting) Style.noHighlight else Styles(language)
      
      def linkAttributes (target: Target, title: Option[String]): Seq[(String, String)] = {
        val href = target match {
          case InternalTarget(_, relPath) =>
            // TODO - 0.15 - generalize suffix check
            if (relPath.suffix.contains("md") || relPath.suffix.contains("rst")) fmt.internalLink(relPath)
            else relPath.toString
          case ExternalTarget(url) => url
        }
        fmt.optAttributes(
          "href" -> Some(href),
          "title" -> title.map(fmt.text)
        )
      }

      con match {

        case Paragraph(content,opt)         => fmt.element("p", opt, content)
        case Emphasized(content,opt)        => fmt.element("em", opt, content)
        case Strong(content,opt)            => fmt.element("strong", opt, content)
        case Deleted(content,opt)           => fmt.element("del", opt, content)
        case Inserted(content,opt)          => fmt.element("ins", opt, content)
        case ParsedLiteralBlock(content,opt)=> fmt.rawElement("pre", opt, fmt.withoutIndentation(_.element("code", NoOpt, content)))
        case cb@CodeBlock(lang,content,opt) => fmt.rawElement("pre", opt, fmt.withoutIndentation(_.element("code", codeStyles(lang, cb.hasSyntaxHighlighting), content)))
        case InlineCode(lang,content,opt)   => fmt.withoutIndentation(_.element("code", opt + codeStyles(lang, false), content))
        case Line(content,opt)              => fmt.element("div", opt + RstStyle.line, content)
        case Title(content, opt)            => fmt.element("h1", opt, content)
        case Header(level, content, opt)    => fmt.newLine + fmt.element("h"+level.toString, opt,content)

        case SpanLink(content, target, title, opt)  => fmt.element("a", opt, content, linkAttributes(target, title):_*)

        case WithFallback(fallback)         => fmt.child(fallback)
        case c: Customizable                => c match {
          case SpanSequence(content, NoOpt) => fmt.children(content) // this case could be standalone above, but triggers a compiler bug then
          case CodeSpanSequence(content, NoOpt) => fmt.children(content)
          case _                            => fmt.element("span", c.options, c.content)
        }
        case unknown                        => fmt.element("span", NoOpt, unknown.content)
      }
    }

    def renderTemplateSpanContainer (con: TemplateSpanContainer): String = {
      con match {
        case TemplateRoot(content, NoOpt)         => fmt.children(content)
        case TemplateSpanSequence(content, NoOpt) => fmt.children(content)
        case c: Customizable                      => fmt.element("span", c.options, c.content)
        case unknown                              => fmt.element("span", NoOpt, unknown.content)
      }
    }

    def renderListContainer (con: ListContainer): String = con match {
      case EnumList(content,format,start,opt) =>
        fmt.indentedElement("ol", opt, content, fmt.optAttributes("class" -> Some(format.enumType.toString.toLowerCase), "start" -> noneIfDefault(start,1)):_*)
      case BulletList(content,_,opt)   => fmt.indentedElement("ul", opt, content)
      case DefinitionList(content,opt) => fmt.indentedElement("dl", opt, content)
      case nl: NavigationList          => fmt.child(navigationToBulletList(nl))

      case WithFallback(fallback)      => fmt.child(fallback)
      case c: Customizable             => fmt.indentedElement("div", c.options, c.content)
      case unknown                     => fmt.indentedElement("div", NoOpt, unknown.content)
    }

    def renderTextContainer (con: TextContainer): String = con match {
      case Text(content,opt)           => opt match {
        case NoOpt                     => fmt.text(content)
        case _                         => fmt.textElement("span", opt, content)
      }
      case TemplateString(content,opt) => opt match {
        case NoOpt                     => content
        case _                         => fmt.rawElement("span", opt, content)
      }
      case RawContent(formats, content, opt) => if (formats.contains("html") || formats.contains("html")) { opt match {
        case NoOpt                     => content
        case _                         => fmt.rawElement("span", opt, content)
      }} else ""
      case CodeSpan(content, categories, opt) => fmt.textElement("span", opt + Styles(categories.map(_.name).toSeq:_*), content) 
      case Literal(content,opt)        => fmt.withoutIndentation(_.textElement("code", opt, content))
      case LiteralBlock(content,opt)   => fmt.element("pre", opt, Seq(Literal(content)))
      case Comment(content,_)          => fmt.comment(content)
      case sn@ SectionNumber(_, opt)   => fmt.child(Text(sn.content, opt + Style.sectionNumber))

      case WithFallback(fallback)      => fmt.child(fallback)
      case c: Customizable             => fmt.textElement("span", c.options, c.content)
      case unknown                     => fmt.text(unknown.content)
    }

    def renderSimpleBlock (block: Block): String = block match {
      case Rule(opt)                   => fmt.emptyElement("hr", opt)
      case InternalLinkTarget(opt)     => fmt.textElement("a", opt, "")
      case LineBlock(content,opt)      => fmt.indentedElement("div", opt + RstStyle.lineBlock, content)
      case TargetFormat("html",e,_)    => fmt.child(e)
      case TargetFormat("xhtml",e,_)   => fmt.child(e)

      case WithFallback(fallback)      => fmt.child(fallback)
      case _                           => ""
    }

    def renderSimpleSpan (span: Span): String = span match {
      case CitationLink(ref,label,opt) => fmt.textElement("a", opt + Style.citation, s"[$label]", "href"->("#"+ref))
      case FootnoteLink(ref,label,opt) => fmt.textElement("a", opt + Style.footnote, s"[$label]", "href"->("#"+ref))

      case Image(text,target,width,height,title,opt) =>
        def sizeAttr (size: Option[Size], styleName: String): (Option[String],Option[String]) = size map {
          case Size(amount, "px") => (Some(amount.toInt.toString), None)
          case Size(amount, unit) => (None, Some(s"$styleName:$amount$unit"))
        } getOrElse (None, None)
        val (widthAttr, wStyle) = sizeAttr(width, "width")
        val (heightAttr, hStyle) = sizeAttr(height, "height")
        val styleAttr = (wStyle ++ hStyle).reduceLeftOption((a,b) => s"$a;$b")
        val uri = target match {
          case it: InternalTarget => it.relativePath.toString
          case et: ExternalTarget => et.url
        }
        val allAttr = fmt.optAttributes("src" -> Some(uri), "alt" -> Some(text), "title" -> title,
          "width" -> widthAttr, "height" -> heightAttr, "style" -> styleAttr)
        fmt.emptyElement("img", opt, allAttr:_*)

      case LineBreak(_)                   => fmt.emptyElement("br")
      case TemplateElement(elem,indent,_) => fmt.withMinIndentation(indent)(_.child(elem))

      case WithFallback(fallback)         => fmt.child(fallback)
      case _                              => ""
    }

    def renderTableElement (elem: TableElement): String = elem match {
      case TableHead(rows,opt)         => fmt.indentedElement("thead", opt, rows)
      case TableBody(rows,opt)         => fmt.indentedElement("tbody", opt, rows)
      case Columns(columns,opt)        => fmt.indentedElement("colgroup", opt, columns)
      case Row(cells,opt)              => fmt.indentedElement("tr", opt, cells)
      case Caption(content, opt)       => fmt.element("caption", opt, content)
      case Column(opt)                 => fmt.textElement("col", opt, "")
      case Cell(HeadCell, content, colspan, rowspan, opt) =>
        renderBlocks("th", opt, content, fmt.optAttributes("colspan" -> noneIfDefault(colspan,1), "rowspan" -> noneIfDefault(rowspan,1)):_*)
      case Cell(BodyCell, content, colspan, rowspan, opt) =>
        renderBlocks("td", opt, content, fmt.optAttributes("colspan" -> noneIfDefault(colspan,1), "rowspan" -> noneIfDefault(rowspan,1)):_*)
    }

    def renderUnresolvedReference (ref: Reference): String =
      fmt.child(InvalidElement(s"unresolved reference: $ref", ref.source).asSpan)

    def renderInvalidElement (elem: Invalid[_ <: Element]): String = elem match {
      case InvalidBlock(msg, fallback, opt) => 
        fmt.forMessage(msg)(fmt.child(Paragraph(List(msg), opt))) + fmt.child(fallback)
      case e =>
        fmt.forMessage(e.message)(fmt.child(e.message) + " ") + fmt.child(e.fallback)
    }

    def renderRuntimeMessage (message: RuntimeMessage): String = {
      fmt.forMessage(message) {
        fmt.textElement("span", message.options + Style.runtimeMessage + Styles(message.level.toString.toLowerCase), message.content)
      }
    }

    element match {
      case e: RuntimeMessage        => renderRuntimeMessage(e)
      case e: Table                 => renderTable(e)
      case e: TableElement          => renderTableElement(e)
      case e: Reference             => renderUnresolvedReference(e)
      case e: Invalid[_]            => renderInvalidElement(e)
      case e: BlockContainer        => renderBlockContainer(e)
      case e: SpanContainer         => renderSpanContainer(e)
      case e: ListContainer         => renderListContainer(e)
      case e: TextContainer         => renderTextContainer(e)
      case e: TemplateSpanContainer => renderTemplateSpanContainer(e)
      case e: Block                 => renderSimpleBlock(e)
      case e: Span                  => renderSimpleSpan(e)

      case _                        => ""
    }
  }

}

object HTMLRenderer extends HTMLRenderer(fileSuffix = "html")

object XHTMLRenderer extends HTMLRenderer(fileSuffix = "epub.xhtml")
