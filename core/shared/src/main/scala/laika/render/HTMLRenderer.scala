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

/** Default renderer implementation for the HTML output format.
  *
  * @author Jens Halm
  */
private[laika] class HTMLRenderer(format: String)
    extends ((HTMLFormatter, Element) => String) {

  def apply(fmt: HTMLFormatter, element: Element): String = {

    def noneIfDefault[T](actual: T, default: T): Option[String] =
      if (actual == default) None else Some(actual.toString)

    def renderBlocks(
        tagName: String,
        options: Options,
        content: Seq[Block],
        attrs: (String, String)*
    ): String = content match {
      case Seq(ss: SpanSequence)      => fmt.element(tagName, options, Seq(ss), attrs: _*)
      case Seq(Paragraph(spans, opt)) =>
        fmt.element(tagName, options, Seq(SpanSequence(spans, opt)), attrs: _*)
      case other                      => fmt.indentedElement(tagName, options, other, attrs: _*)
    }

    def renderTable(table: Table): String = {
      val children =
        List(table.caption, table.columns, table.head, table.body) filterNot (_.content.isEmpty)
      fmt.indentedElement("table", table.options, children)
    }

    def navigationToBulletList(navList: NavigationList): BulletList = {

      val bullet = StringBullet("*")

      def transformItems(items: Seq[NavigationItem]): Seq[BulletListItem] = {
        items.flatMap { item =>
          val target: BulletListItem = {
            val linkStyles = if (item.link.exists(_.selfLink)) Style.active else NoOpt
            val typeStyles =
              if (item.link.isEmpty) Style.navHeader
              else if (item.content.nonEmpty) Style.navNode
              else Style.navLeaf
            val content    = item.link.fold(item.title) { link =>
              SpanSequence(SpanLink(item.title.content, link.target))
            }
            BulletListItem(Seq(content), bullet, item.options + linkStyles + typeStyles)
          }
          val children = if (item.content.isEmpty) Nil else transformItems(item.content)
          target +: children
        }
      }

      BulletList(transformItems(navList.content), bullet, navList.options + Style.navList)
    }

    object WithFallback {
      def unapply(value: Element): Option[Element] = value match {
        case f: Fallback => Some(f.fallback)
        case _           => None
      }
    }

    def renderBlockContainer(con: BlockContainer): String = {

      def toTable(label: String, content: Seq[Block], options: Options): Table = {
        val left  = BodyCell(SpanSequence(s"[$label]"))
        val right = BodyCell(content)
        val row   = Row(List(left, right))
        Table(
          TableHead(Nil),
          TableBody(List(row)),
          Caption(),
          Columns.options(Style.label, NoOpt),
          options
        )
      }

      def quotedBlockContent(content: Seq[Block], attr: Seq[Span]): Seq[Block] =
        if (attr.isEmpty) content
        else content :+ Paragraph(attr, Style.attribution)

      def figureContent(img: Span, caption: Seq[Span], legend: Seq[Block]): List[Block] =
        List(
          SpanSequence(img),
          Paragraph(caption, Style.caption),
          BlockSequence(legend, Style.legend)
        )

      con match {
        case RootElement(content, _)           => fmt.childPerLine(content)
        case EmbeddedRoot(content, indent, _)  =>
          fmt.withMinIndentation(indent)(_.childPerLine(content))
        case Section(header, content, _)       => fmt.childPerLine(header +: content)
        case TitledBlock(title, content, opt)  =>
          fmt.indentedElement("div", opt, Paragraph(title, Style.title) +: content)
        case QuotedBlock(content, attr, opt)   =>
          renderBlocks("blockquote", opt, quotedBlockContent(content, attr))
        case BulletListItem(content, _, opt)   => renderBlocks("li", opt, content)
        case EnumListItem(content, _, _, opt)  => renderBlocks("li", opt, content)
        case DefinitionListItem(term, defn, _) =>
          fmt.element("dt", NoOpt, term) + fmt.newLine + renderBlocks("dd", NoOpt, defn)
        case Figure(img, caption, legend, opt) =>
          fmt.indentedElement("div", opt + Style.figure, figureContent(img, caption, legend))

        case Footnote(label, content, opt) =>
          renderTable(toTable(label, content, opt + Style.footnote))
        case Citation(label, content, opt) =>
          renderTable(toTable(label, content, opt + Style.citation))

        case WithFallback(fallback)        => fmt.child(fallback)
        case BlockSequence(content, NoOpt) => fmt.childPerLine(content)

        case unknown => fmt.indentedElement("div", unknown.options, unknown.content)
      }
    }

    def renderTarget(target: Target): String = fmt.pathTranslator.translate(target) match {
      case ext: ExternalTarget => ext.url
      case int: InternalTarget =>
        val relPath = int.relativeTo(fmt.path).relativePath
        if (relPath.withoutFragment.toString.endsWith("/index.html"))
          relPath.withBasename("").withoutSuffix.toString
        else
          relPath.toString
    }

    def renderSpanContainer(con: SpanContainer): String = {

      def codeStyles(language: String, hasHighlighting: Boolean) =
        if (hasHighlighting) Style.noHighlight else Styles(language)

      def linkAttributes(target: Target, title: Option[String]): Seq[(String, String)] =
        fmt.optAttributes(
          "href"  -> Some(renderTarget(target)),
          "title" -> title.map(fmt.text)
        )

      con match {

        case Paragraph(content, opt)               => fmt.element("p", opt, content)
        case Emphasized(content, opt)              => fmt.element("em", opt, content)
        case Strong(content, opt)                  => fmt.element("strong", opt, content)
        case Deleted(content, opt)                 => fmt.element("del", opt, content)
        case Inserted(content, opt)                => fmt.element("ins", opt, content)
        case ParsedLiteralBlock(content, opt)      =>
          fmt.rawElement("pre", opt, fmt.withoutIndentation(_.element("code", NoOpt, content)))
        case cb @ CodeBlock(lang, content, _, opt) =>
          fmt.rawElement(
            "pre",
            opt,
            fmt.withoutIndentation(
              _.element("code", codeStyles(lang, cb.hasSyntaxHighlighting), content)
            )
          )
        case InlineCode(lang, content, opt)        =>
          fmt.withoutIndentation(
            _.element("code", opt + codeStyles(lang, hasHighlighting = false), content)
          )
        case Title(content, opt)                   => fmt.element("h1", opt, content)
        case Header(level, content, opt)           =>
          fmt.newLine + fmt.element("h" + level.toString, opt, content)

        case SpanLink(content, target, title, opt) =>
          fmt.element("a", opt, content, linkAttributes(target, title): _*)

        case WithFallback(fallback)           => fmt.child(fallback)
        case SpanSequence(content, NoOpt)     => fmt.children(content)
        case CodeSpanSequence(content, NoOpt) => fmt.children(content)

        case unknown => fmt.element("span", unknown.options, unknown.content)
      }
    }

    def renderTemplateSpanContainer(con: TemplateSpanContainer): String = {
      con match {
        case TemplateRoot(content, NoOpt)         => fmt.children(content)
        case TemplateSpanSequence(content, NoOpt) => fmt.children(content)
        case unknown => fmt.element("span", unknown.options, unknown.content)
      }
    }

    def renderListContainer(con: ListContainer): String = con match {
      case EnumList(content, enumFormat, start, opt) =>
        fmt.indentedElement(
          "ol",
          opt,
          content,
          fmt.optAttributes(
            "class" -> Some(enumFormat.enumType.toString.toLowerCase),
            "start" -> noneIfDefault(start, 1)
          ): _*
        )
      case BulletList(content, _, opt)               => fmt.indentedElement("ul", opt, content)
      case DefinitionList(content, opt)              => fmt.indentedElement("dl", opt, content)
      case nl: NavigationList                        => fmt.child(navigationToBulletList(nl))

      case WithFallback(fallback) => fmt.child(fallback)
      case unknown                => fmt.indentedElement("div", unknown.options, unknown.content)
    }

    def renderTextContainer(con: TextContainer): String = con match {
      case Text(content, opt)                 =>
        opt match {
          case NoOpt => fmt.text(content)
          case _     => fmt.textElement("span", opt, content)
        }
      case TemplateString(content, opt)       =>
        opt match {
          case NoOpt => content
          case _     => fmt.rawElement("span", opt, content)
        }
      case RawContent(f, content, opt)        =>
        if (f.contains(format)) {
          opt match {
            case NoOpt => content
            case _     => fmt.rawElement("span", opt, content)
          }
        }
        else ""
      case CodeSpan(content, categories, opt) =>
        fmt.textElement("span", opt + Styles(categories.map(_.name).toSeq: _*), content)
      case Literal(content, opt)      => fmt.withoutIndentation(_.textElement("code", opt, content))
      case LiteralBlock(content, opt) => fmt.element("pre", opt, Seq(Literal(content)))
      case Comment(content, _)        => fmt.comment(content)
      case sn @ SectionNumber(_, opt) => fmt.child(Text(sn.content, opt + Style.sectionNumber))

      case WithFallback(fallback) => fmt.child(fallback)
      case unknown                => fmt.textElement("span", unknown.options, unknown.content)
    }

    def renderChoices(choices: Seq[Choice], options: Options): String = {
      val content = choices.flatMap { choice =>
        Paragraph(Strong(Text(choice.label))) +: choice.content
      }
      fmt.child(BlockSequence(content, options))
    }

    def renderSimpleBlock(block: Block): String = block match {
      case Rule(opt)                                   => fmt.emptyElement("hr", opt)
      case InternalLinkTarget(opt)                     => fmt.textElement("a", opt, "")
      case Selection(_, choices, opt)                  => renderChoices(choices, opt)
      case TargetFormat(f, e, _) if f.contains(format) => fmt.child(e)

      case WithFallback(fallback) => fmt.child(fallback)
      case _                      => ""
    }

    def renderIcon(icon: Icon): String = {
      val (tagName, options, content) = icon match {
        case icon: IconGlyph     => ("i", icon.options, icon.codePointAsEntity)
        case icon: IconStyle     => ("i", Styles(icon.styleName) + icon.options, "")
        case icon: InlineSVGIcon => ("span", icon.options, icon.content)
        case icon: SVGSymbolIcon =>
          (
            "span",
            icon.options,
            s"""<svg class="svg-icon"><use class="svg-shape" href="${
                renderTarget(icon.target)
              }"/></svg>"""
          )
      }
      fmt.rawElement(tagName, options, content, fmt.optAttributes("title" -> icon.title): _*)
    }

    def renderSimpleSpan(span: Span): String = span match {
      case CitationLink(ref, label, opt) =>
        fmt.textElement("a", opt + Style.citation, s"[$label]", "href" -> ("#" + ref))
      case FootnoteLink(ref, label, opt) =>
        fmt.textElement("a", opt + Style.footnote, s"[$label]", "href" -> ("#" + ref))
      case RawLink(target, _)            => renderTarget(target)

      case Image(target, width, height, alt, title, opt) =>
        def sizeAttr(size: Option[Length], styleName: String): (Option[String], Option[String]) =
          size
            .map {
              case Length(amount, LengthUnit.px) => (Some(amount.toInt.toString), None)
              case s: Length                     => (None, Some(s"$styleName:${s.displayValue}"))
            }
            .getOrElse((None, None))
        val (widthAttr, wStyle)  = sizeAttr(width, "width")
        val (heightAttr, hStyle) = sizeAttr(height, "height")
        val styleAttr            = (wStyle ++ hStyle).reduceLeftOption((a, b) => s"$a;$b")
        val allAttr              = fmt.optAttributes(
          "src"    -> Some(renderTarget(target)),
          "alt"    -> alt,
          "title"  -> title,
          "width"  -> widthAttr,
          "height" -> heightAttr,
          "style"  -> styleAttr
        )
        fmt.emptyElement("img", opt, allAttr: _*)

      case icon: Icon                       => renderIcon(icon)
      case LineBreak(_)                     => fmt.emptyElement("br")
      case TemplateElement(elem, indent, _) => fmt.withMinIndentation(indent)(_.child(elem))

      case WithFallback(fallback) => fmt.child(fallback)
      case _                      => ""
    }

    def renderTableElement(elem: TableElement): String = elem match {
      case TableHead(rows, opt)  => fmt.indentedElement("thead", opt, rows)
      case TableBody(rows, opt)  => fmt.indentedElement("tbody", opt, rows)
      case Columns(columns, opt) => fmt.indentedElement("colgroup", opt, columns)
      case Row(cells, opt)       => fmt.indentedElement("tr", opt, cells)
      case Caption(content, opt) => fmt.element("caption", opt, content)
      case Column(opt)           => fmt.textElement("col", opt, "")
      case Cell(HeadCell, content, colspan, rowspan, opt) =>
        renderBlocks(
          "th",
          opt,
          content,
          fmt.optAttributes(
            "colspan" -> noneIfDefault(colspan, 1),
            "rowspan" -> noneIfDefault(rowspan, 1)
          ): _*
        )
      case Cell(BodyCell, content, colspan, rowspan, opt) =>
        renderBlocks(
          "td",
          opt,
          content,
          fmt.optAttributes(
            "colspan" -> noneIfDefault(colspan, 1),
            "rowspan" -> noneIfDefault(rowspan, 1)
          ): _*
        )
    }

    def renderUnresolvedReference(ref: Reference): String =
      fmt.child(InvalidSpan(s"unresolved reference: $ref", ref.source))

    def renderInvalidElement(elem: Invalid): String = elem match {
      case InvalidBlock(msg, _, fallback, opt) =>
        fmt.forMessage(msg)(fmt.child(Paragraph(List(msg), opt))) + fmt.child(fallback)
      case e                                   =>
        fmt.forMessage(e.message)(fmt.child(e.message) + " ") + fmt.child(e.fallback)
    }

    def renderRuntimeMessage(message: RuntimeMessage): String = {
      fmt.forMessage(message) {
        fmt.textElement(
          "span",
          message.options + Style.runtimeMessage + Styles(message.level.toString.toLowerCase),
          message.content
        )
      }
    }

    element match {
      case e: RuntimeMessage        => renderRuntimeMessage(e)
      case e: Table                 => renderTable(e)
      case e: TableElement          => renderTableElement(e)
      case e: Reference             => renderUnresolvedReference(e)
      case e: Invalid               => renderInvalidElement(e)
      case e: BlockContainer        => renderBlockContainer(e)
      case e: SpanContainer         => renderSpanContainer(e)
      case e: ListContainer         => renderListContainer(e)
      case e: TextContainer         => renderTextContainer(e)
      case e: TemplateSpanContainer => renderTemplateSpanContainer(e)
      case e: Block                 => renderSimpleBlock(e)
      case e: Span                  => renderSimpleSpan(e)

      case _ => ""
    }
  }

}

private[laika] object HTMLRenderer extends HTMLRenderer(format = "html")
