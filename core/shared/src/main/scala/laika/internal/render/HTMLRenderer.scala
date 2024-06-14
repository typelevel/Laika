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

package laika.internal.render

import laika.api.format.TagFormatter
import laika.ast.*

/** Default renderer implementation for the HTML output format.
  *
  * @author Jens Halm
  */
private[laika] class HTMLRenderer(format: String)
    extends ((TagFormatter, Element) => String) {

  case class ElementSequence(content: Seq[Element], options: Options = Options.empty)
      extends ElementContainer[Element] {
    type Self = ElementSequence
    def withOptions(options: Options): ElementSequence = copy(options = options)
  }

  def apply(fmt: TagFormatter, element: Element): String = {

    def noneIfDefault[T](actual: T, default: T): Option[String] =
      if (actual == default) None else Some(actual.toString)

    def renderBlocks(
        tagName: String,
        container: BlockContainer,
        attrs: (String, String)*
    ): String = container.content match {
      case Seq(ss: SpanSequence)      =>
        fmt.element(tagName, ss.withOptions(container.options), attrs: _*)
      case Seq(Paragraph(spans, opt)) =>
        fmt.element(tagName, SpanSequence(spans, opt), attrs: _*)
      case _                          => fmt.indentedElement(tagName, container, attrs: _*)
    }

    def renderTable(table: Table): String = {
      val children =
        List(table.caption, table.columns, table.head, table.body) filterNot (_.content.isEmpty)
      fmt.indentedElement("table", ElementSequence(children).withOptions(table.options))
    }

    def navigationToBulletList(navList: NavigationList): BulletList = {

      val bullet = BulletFormat.StringBullet("*")

      def transformItems(items: Seq[NavigationItem]): Seq[BulletListItem] = {
        items.flatMap { item =>
          val target: BulletListItem = {
            val linkStyles = if (item.link.exists(_.selfLink)) Style.active else Options.empty
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
        val left  = CellType.BodyCell(SpanSequence(s"[$label]"))
        val right = CellType.BodyCell(content)
        val row   = Row(List(left, right))
        Table(
          TableHead(Nil),
          TableBody(List(row)),
          Caption(),
          Columns.options(Style.label, Options.empty),
          options
        )
      }

      def quotedBlockContent(qb: QuotedBlock): BlockContainer =
        if (qb.attribution.isEmpty) qb
        else
          BlockSequence(qb.content :+ Paragraph(qb.attribution, Style.attribution))
            .withOptions(qb.options)

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
          fmt.indentedElement(
            "div",
            BlockSequence(Paragraph(title, Style.title) +: content).withOptions(opt)
          )
        case qb: QuotedBlock                   =>
          renderBlocks("blockquote", quotedBlockContent(qb))
        case bli: BulletListItem               => renderBlocks("li", bli)
        case eli: EnumListItem                 => renderBlocks("li", eli)
        case dli: DefinitionListItem           =>
          fmt.element("dt", SpanSequence(dli.term)) +
            fmt.newLine +
            renderBlocks("dd", dli.clearOptions)
        case Figure(img, caption, legend, opt) =>
          val content = BlockSequence(figureContent(img, caption, legend))
            .withOptions(opt + Style.figure)
          fmt.indentedElement("div", content)

        case Footnote(label, content, opt) =>
          renderTable(toTable(label, content, opt + Style.footnote))
        case Citation(label, content, opt) =>
          renderTable(toTable(label, content, opt + Style.citation))

        case WithFallback(fallback)                => fmt.child(fallback)
        case BlockSequence(content, Options.empty) => fmt.childPerLine(content)

        case unknown => fmt.indentedElement("div", unknown)
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

      def renderCode(spans: SpanContainer): String =
        fmt.withoutIndentation(_.element("code", spans))

      con match {

        case p: Paragraph  => fmt.element("p", p)
        case t: Title      => fmt.element("h1", t)
        case e: Emphasized => fmt.element("em", e)
        case s: Strong     => fmt.element("strong", s)
        case d: Deleted    => fmt.element("del", d)
        case i: Inserted   => fmt.element("ins", i)

        case sl: SpanLink =>
          fmt.element("a", sl, linkAttributes(sl.target, sl.title) *)
        case h: Header    =>
          fmt.newLine + fmt.element("h" + h.level.toString, h)

        case lb: ParsedLiteralBlock =>
          fmt.rawElement("pre", lb, renderCode(lb.clearOptions))
        case cb: CodeBlock          =>
          val opt = codeStyles(cb.language, cb.hasSyntaxHighlighting)
          fmt.rawElement("pre", cb, renderCode(cb.withOptions(opt)))
        case ic: InlineCode         =>
          val opt = codeStyles(ic.language, hasHighlighting = false)
          renderCode(ic.mergeOptions(opt))

        case WithFallback(fallback)                   => fmt.child(fallback)
        case SpanSequence(content, Options.empty)     => fmt.children(content)
        case CodeSpanSequence(content, Options.empty) => fmt.children(content)

        case unknown => fmt.element("span", unknown)
      }
    }

    def renderTemplateSpanContainer(con: TemplateSpanContainer): String = {
      con match {
        case TemplateRoot(content, Options.empty)         => fmt.children(content)
        case TemplateSpanSequence(content, Options.empty) => fmt.children(content)
        case unknown                                      => fmt.element("span", unknown)
      }
    }

    def renderListContainer(con: ListContainer): String = con match {
      case el @ EnumList(_, enumFormat, start, _) =>
        fmt.indentedElement(
          "ol",
          el,
          fmt.optAttributes(
            "class" -> Some(enumFormat.enumType.toString.toLowerCase),
            "start" -> noneIfDefault(start, 1)
          ) *
        )
      case bl: BulletList                         => fmt.indentedElement("ul", bl)
      case dl: DefinitionList                     => fmt.indentedElement("dl", dl)
      case nl: NavigationList                     => fmt.child(navigationToBulletList(nl))

      case WithFallback(fallback) => fmt.child(fallback)
      case unknown                => fmt.indentedElement("div", unknown)
    }

    def renderTextContainer(con: TextContainer): String = con match {
      case t: Text                          =>
        if (t.options.isEmpty) fmt.text(t.content)
        else fmt.textElement("span", t)
      case ts: TemplateString               =>
        if (ts.options.isEmpty) ts.content
        else fmt.rawElement("span", ts, ts.content)
      case rc @ RawContent(f, content, opt) =>
        if (f.contains(format)) {
          if (opt.isEmpty) content
          else fmt.rawElement("span", rc, content)
        }
        else ""
      case cs: CodeSpan                     =>
        fmt.textElement("span", cs.mergeOptions(Styles(cs.categories.map(_.name).toSeq *)))
      case l: Literal                       => fmt.withoutIndentation(_.textElement("code", l))
      case LiteralBlock(content, opt)       =>
        fmt.element("pre", SpanSequence(Literal(content)).withOptions(opt))
      case Comment(content, _)              => fmt.comment(content)
      case sn @ SectionNumber(_, opt) => fmt.child(Text(sn.content, opt + Style.sectionNumber))

      case WithFallback(fallback) => fmt.child(fallback)
      case unknown                => fmt.textElement("span", unknown)
    }

    def renderChoices(choices: Seq[Choice], options: Options): String = {
      val content = choices.flatMap { choice =>
        Paragraph(Strong(Text(choice.label))) +: choice.content
      }
      fmt.child(BlockSequence(content, options))
    }

    def renderSimpleBlock(block: Block): String = block match {
      case r: Rule                    => fmt.emptyElement("hr", r)
      case InternalLinkTarget(opt)    => fmt.textElement("a", Text("").withOptions(opt))
      case Selection(_, choices, opt) => renderChoices(choices, opt)
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
      fmt.rawElement(
        tagName,
        icon.withOptions(options),
        content,
        fmt.optAttributes("title" -> icon.title): _*
      )
    }

    def renderSimpleSpan(span: Span): String = span match {
      case CitationLink(ref, label, opt) =>
        val text = Text(s"[$label]").withOptions(opt + Style.citation)
        fmt.textElement("a", text, "href" -> ("#" + ref))
      case FootnoteLink(ref, label, opt) =>
        val text = Text(s"[$label]").withOptions(opt + Style.footnote)
        fmt.textElement("a", text, "href" -> ("#" + ref))
      case RawLink(target, _)            => renderTarget(target)

      case img @ Image(target, width, height, alt, title, _) =>
        def sizeAttr(size: Option[Length], styleName: String): (Option[String], Option[String]) =
          size
            .map {
              case Length(amount, LengthUnit.px) => (Some(amount.toInt.toString), None)
              case s: Length                     => (None, Some(s"$styleName:${s.displayValue}"))
            }
            .getOrElse((None, None))
        val (widthAttr, wStyle) = sizeAttr(width, "width")
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
        fmt.emptyElement("img", img, allAttr: _*)

      case icon: Icon                       => renderIcon(icon)
      case lb: LineBreak                    => fmt.emptyElement("br", lb)
      case TemplateElement(elem, indent, _) => fmt.withMinIndentation(indent)(_.child(elem))

      case WithFallback(fallback) => fmt.child(fallback)
      case _                      => ""
    }

    def renderTableElement(elem: TableElement): String = elem match {
      case th: TableHead => fmt.indentedElement("thead", th)
      case tb: TableBody => fmt.indentedElement("tbody", tb)
      case c: Columns    => fmt.indentedElement("colgroup", c)
      case r: Row        => fmt.indentedElement("tr", r)
      case c: Caption    => fmt.element("caption", c)
      case Column(opt)   => fmt.textElement("col", Text("").withOptions(opt))
      case c: Cell       =>
        val tagName    = c.cellType match {
          case CellType.HeadCell => "th"
          case CellType.BodyCell => "td"
        }
        val attributes = fmt.optAttributes(
          "colspan" -> noneIfDefault(c.colspan, 1),
          "rowspan" -> noneIfDefault(c.rowspan, 1)
        )
        renderBlocks(tagName, c, attributes *)
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
      val styles = Style.runtimeMessage + Styles(message.level.toString.toLowerCase)
      fmt.forMessage(message) {
        fmt.textElement("span", message.mergeOptions(styles))
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
