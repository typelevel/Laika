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

import cats.data.NonEmptySet
import laika.ast.*
import laika.rst.ast.{ Line, LineBlock }

/** Default renderer implementation for the XSL-FO output format.
  *
  * @author Jens Halm
  */
private[laika] object FORenderer extends ((TagFormatter, Element) => String) {

  import laika.format.XSLFO.formatterSyntax.*
  import FOFormatter.*

  private val formats: NonEmptySet[String] = NonEmptySet.of("pdf", "fo", "xslfo", "xsl-fo")

  def apply(fmt: TagFormatter, element: Element): String = {

    def rootElement: Element = fmt.parents.lastOption.getOrElse(fmt.currentElement)

    def findFootnoteById(ref: String): Option[Footnote] = rootElement match {
      case et: ElementTraversal =>
        et.collect {
          case f @ Footnote(_, _, Id(id)) if id == ref => f
        }.headOption
      case _                    => None
    }

    def findCitationById(ref: String): Option[Citation] = rootElement match {
      case et: ElementTraversal =>
        et.collect {
          case c @ Citation(_, _, Id(id)) if id == ref => c
        }.headOption
      case _                    => None
    }

    def noneIfDefault[T](actual: T, default: T): Option[String] =
      if (actual == default) None else Some(actual.toString)

    def renderTable(table: Table): String = {
      if (table.caption.content.nonEmpty) {
        // FOP does not support fo:table-caption
        fmt.child(TitledBlock(table.caption.content, List(table.copy(caption = Caption()))))
      }
      else {
        val children =
          table.columns.content ++ (List(table.head, table.body) filterNot (_.content.isEmpty))
        fmt.indentedElement("fo:table", table, children)
      }
    }

    object WithFallback {
      def unapply(value: Element): Option[Element] = value match {
        case f: Fallback => Some(f.fallback)
        case _           => None
      }
    }

    def renderBlockContainer(con: BlockContainer): String = {

      def quotedBlockContent(content: Seq[Block], attr: Seq[Span]): Seq[Block] =
        if (attr.isEmpty) content
        else content :+ SpanSequence(attr, Style.attribution)

      def figureContent(img: Span, caption: Seq[Span], legend: Seq[Block]): List[Block] =
        List(
          Paragraph(img),
          SpanSequence(caption, Style.caption),
          BlockSequence(legend, Style.legend)
        )

      def enumLabel(format: EnumFormat, num: Int): String = {
        import EnumType.*
        val pos = format.enumType match {
          case Arabic     => num.toString
          case LowerAlpha => ('a' + num - 1).toChar.toString
          case UpperAlpha => ('A' + num - 1).toChar.toString
          case LowerRoman => RomanNumerals.intToRoman(num).toLowerCase
          case UpperRoman => RomanNumerals.intToRoman(num).toUpperCase
        }
        format.prefix + pos + format.suffix
      }

      def bulletLabel(format: BulletFormat): Span = format match {
        case StringBullet(_) => RawContent(NonEmptySet.one("fo"), "&#x2022;")
        case other           => Text(other.toString)
      }

      def listItemBody(body: ListItemBody): String = {
        val content = body.content.map {
          case sc: SpanSequence => Paragraph(sc.content, sc.options)
          case other            => other
        }
        fmt.indentedElement(
          "fo:list-item-body",
          body,
          content,
          "start-indent" -> "body-start()"
        )
      }

      con match {
        case RootElement(content, _)            => fmt.childPerLine(content)
        case EmbeddedRoot(content, indent, _)   =>
          fmt.withMinIndentation(indent)(_.childPerLine(content))
        case Section(header, content, _)        => fmt.childPerLine(header +: content)
        case e @ TitledBlock(title, content, _) =>
          fmt.blockContainer(e, SpanSequence(title, Style.title) +: content)
        case e @ QuotedBlock(content, attr, _)  =>
          fmt.blockContainer(e, quotedBlockContent(content, attr))

        case e @ BulletListItem(content, format, _)    =>
          fmt.listItem(e, List(bulletLabel(format)), content)
        case e @ EnumListItem(content, format, num, _) =>
          fmt.listItem(e, List(Text(enumLabel(format, num))), content)
        case e @ DefinitionListItem(term, defn, _)     => fmt.listItem(e, term, defn)
        case e: ListItemBody                           => listItemBody(e)

        case e @ Figure(img, caption, legend, _) =>
          fmt.blockContainer(e, figureContent(img, caption, legend))

        case e: FootnoteBody => fmt.indentedElement("fo:footnote-body", e)
        case _: Footnote     => "" // rendered in link position
        case _: Citation     => "" // rendered in link position

        case WithFallback(fallback) => fmt.child(fallback)

        case BlockSequence(Seq(SpanSequence(Seq(img: Image), optSpan)), optBlock) =>
          fmt.child(
            SpanSequence(
              Seq(img.mergeOptions(optSpan + optBlock)),
              Styles("align-center", "default-space")
            )
          )
        case BlockSequence(content, NoOpt) => fmt.childPerLine(content)

        case unknown => fmt.blockContainer(unknown, unknown.content)
      }
    }

    def internalLinkTarget(elem: Element): String = {
      fmt.parents.head match {
        case _: BlockContainer => fmt.emptyElement("fo:block", elem)
        case _                 =>
          val attributes = fmt.attributes("fo:inline", elem, Nil)
          s"<fo:inline$attributes></fo:inline>"
      }
    }

    def renderLink(link: SpanLink): String = {
      fmt.pathTranslator.translate(link.target) match {
        case int: InternalTarget =>
          val target = int.relativeTo(fmt.path).absolutePath
          val id     = FOFormatter.globalId(target, fmt.pathTranslator)
          fmt.element("fo:basic-link", link, "internal-destination" -> id)
        case ext: ExternalTarget =>
          fmt.element("fo:basic-link", link, "external-destination" -> ext.url)
      }
    }

    def renderSpanContainer(con: SpanContainer): String = {
      def codeStyles(language: String): Option[String] =
        if (language.isEmpty) None else Some(language)

      con match {

        case Paragraph(Seq(img: Image), _) =>
          fmt.child(SpanSequence(Seq(img), Styles("align-center", "default-space")))
        case e: Paragraph                  => fmt.block(e)
        case e: ParsedLiteralBlock         => fmt.blockWithWS(e)
        case e: CodeBlock                  =>
          fmt.blockWithWS(e.withStyles(codeStyles(e.language).toSeq))
        case e: Header                     =>
          fmt.block(e.mergeOptions(Style.level(e.level)), "keep-with-next" -> "always")
        case e: Title                      => fmt.block(e, "keep-with-next" -> "always")

        case e: Emphasized => fmt.inline(e)
        case e: Strong     => fmt.inline(e)
        case e: Deleted    => fmt.inline(e)
        case e: Inserted   => fmt.inline(e)
        case e: InlineCode => fmt.inline(e.withStyles(codeStyles(e.language).toSeq))
        case e: Line       => fmt.block(e)

        case link: SpanLink => renderLink(link)

        case WithFallback(fallback)           => fmt.child(fallback)
        case SpanSequence(content, NoOpt)     => fmt.children(content)
        case CodeSpanSequence(content, NoOpt) => fmt.children(content)

        // TODO - needs to be inline if parent is not a block container
        case unknown: Block => fmt.block(unknown)

        case unknown => fmt.inline(unknown)
      }
    }

    def renderTemplateSpanContainer(con: TemplateSpanContainer): String = {
      con match {
        case TemplateRoot(content, NoOpt)         => fmt.children(content)
        case TemplateSpanSequence(content, NoOpt) => fmt.children(content)
        case unknown                              => fmt.inline(unknown)
      }
    }

    def renderListContainer(con: ListContainer): String = con match {
      case e: EnumList       => fmt.listBlock(e)
      case e: BulletList     => fmt.listBlock(e)
      case e: DefinitionList => fmt.listBlock(e)
      case e: NavigationList =>
        if (e.hasStyle("bookmark")) renderBookmarkTree(e) else fmt.childPerLine(e.content)

      case WithFallback(fallback) => fmt.child(fallback)
      case unknown                => fmt.listBlock(unknown)
    }

    def preserveWhitespace(content: String): String = fmt.withoutIndentation(_.text(content))

    def renderTextContainer(con: TextContainer): String = con match {
      case e: Text                        => fmt.optRawElement("fo:inline", e, fmt.text(e.content))
      case e @ TemplateString(content, _) => fmt.optRawElement("fo:inline", e, content)
      case e @ RawContent(f, content, _)  =>
        if (f.intersect(formats).nonEmpty) fmt.optRawElement("fo:inline", e, content) else ""
      case e @ CodeSpan(content, categories, _) =>
        fmt.optRawElement(
          "fo:inline",
          e.withStyles(categories.map(_.name)),
          preserveWhitespace(content)
        )
      case e @ Literal(content, _)              =>
        fmt.optRawElement("fo:inline", e, preserveWhitespace(content))
      case e @ LiteralBlock(content, _)         =>
        fmt.optRawElement("fo:block", e, preserveWhitespace(content))
      case e: BookmarkTitle                     => renderBookmarkTitle(e)
      case Comment(content, _)                  => fmt.comment(content)

      case WithFallback(fallback) => fmt.child(fallback)
      case unknown                => fmt.optRawElement("fo:inline", unknown, unknown.content)
    }

    def renderChoices(choices: Seq[Choice], options: Options): String = {
      val content = choices.flatMap { choice =>
        Paragraph(Strong(Text(choice.label))) +: choice.content
      }
      fmt.child(BlockSequence(content, options))
    }

    def renderListItemLabel(label: ListItemLabel): String =
      fmt.indentedElement(
        "fo:list-item-label",
        label,
        Seq(label.content),
        "end-indent" -> "label-end()"
      )

    def renderSimpleBlock(block: Block): String = block match {
      case e: ContentWrapper => renderContentWrapper(e)
      case e: Preamble       => renderPreamble(e)
      case e: ListItemLabel  => renderListItemLabel(e)
      case e: Rule           =>
        val attributes = fmt.attributes("fo:leader", e, Seq("leader-pattern" -> "rule"))
        val styleHints = BlockSequence.empty.withOptions(e.options + Styles("rule-block"))
        fmt.rawElement("fo:block", styleHints, s"<fo:leader$attributes></fo:leader>")

      case Selection(_, choices, opt)                             => renderChoices(choices, opt)
      case e: InternalLinkTarget                                  => internalLinkTarget(e)
      case e: PageBreak                                           => fmt.emptyElement("fo:block", e)
      case e @ LineBlock(content, _)                              => fmt.blockContainer(e, content)
      case TargetFormat(f, e, _) if f.intersect(formats).nonEmpty => fmt.child(e)

      case WithFallback(fallback) => fmt.child(fallback)
      case _                      => ""
    }

    def renderTarget(target: Target): String = fmt.pathTranslator.translate(target) match {
      case ext: ExternalTarget => ext.url
      case int: InternalTarget =>
        FOFormatter.globalId(int.relativeTo(fmt.path).absolutePath, fmt.pathTranslator)
    }

    def renderIcon(icon: Icon): String = icon match {
      case icon: IconGlyph     => fmt.rawElement("fo:inline", icon, icon.codePointAsEntity)
      case icon: InlineSVGIcon =>
        val styles =
          fmt.styles.collectStyles(SpanSequence.empty.withStyle("svg-shape"), fmt.parents).get(
            "color"
          )
        val svg    = styles.fold(icon.content) { color =>
          val parts =
            icon.content.split(">", 2) // inlining styles as FOP itself does not support CSS for SVG
          if (parts.length == 2)
            parts.head + s">\n  <style>.svg-shape { fill: $color; }</style>" + parts.last
          else icon.content
        }
        fmt.rawElement("fo:instream-foreign-object", icon, svg)
      case _                   => ""
    }

    def renderFootnote(
        styleHint: Element,
        label: String,
        body: Seq[Block],
        options: Options
    ): String = {
      val labelElement = Text(s"[$label]", Style.footnoteLabel)
      val bodyElements = body match {
        case Paragraph(spans, opts) +: rest =>
          Paragraph(labelElement +: Text(" ") +: spans, opts) +: rest
        case _                              => Paragraph(labelElement) +: body
      }
      val content      = List(labelElement, FootnoteBody(bodyElements, options))
      fmt.indentedElement("fo:footnote", styleHint, content)
    }

    def withFootnote(ref: String)(f: Footnote => String): String =
      findFootnoteById(ref).fold("")(f)

    def withCitation(ref: String)(f: Citation => String): String =
      findCitationById(ref).fold("")(f)

    def renderSimpleSpan(span: Span): String = span match {
      case e @ CitationLink(ref, label, _)  =>
        withCitation(ref)(c => renderFootnote(e, label, c.content, c.options))
      case e @ FootnoteLink(ref, label, _)  =>
        withFootnote(ref)(f => renderFootnote(e, label, f.content, f.options))
      case RawLink(target, _)               => renderTarget(target)
      case SectionNumber(pos, opt)          =>
        fmt.child(Text(pos.mkString(".") + " ", opt + Style.sectionNumber))
      case e @ Image(target, _, _, _, _, _) =>
        val uri = target match {
          case it: InternalTarget => it.relativeTo(fmt.path).absolutePath.toString
          case et: ExternalTarget => et.url
        }
        // ignore intrinsic size and rely on styles for sizing
        fmt.emptyElement("fo:external-graphic", e, "src" -> uri)
      case icon: Icon                       => renderIcon(icon)
      case e: Leader                        =>
        fmt.textElement(
          "fo:leader",
          Text("").withOptions(e.options),
          "leader-pattern" -> "dots",
          "padding-left"   -> "2mm",
          "padding-right"  -> "2mm"
        )
      case PageNumberCitation(target, _)    =>
        s"""<fo:page-number-citation ref-id="${
            FOFormatter.globalId(target.relativeTo(fmt.path).absolutePath, fmt.pathTranslator)
          }" />"""
      case LineBreak(_)                     => "&#x2028;"
      case TemplateElement(elem, indent, _) => fmt.withMinIndentation(indent)(_.child(elem))

      case WithFallback(fallback) => fmt.child(fallback)
      case _                      => ""
    }

    def addRowStyles(rows: Seq[Row]): Seq[Row] = rows.zipWithIndex.map { case (row, index) =>
      row.withStyle(if (index % 2 == 0) "cell-odd" else "cell-even") // switch to 1-base
    }

    def renderTableElement(elem: TableElement): String = elem match {
      case e: TableHead           => fmt.indentedElement("fo:table-header", e)
      case e @ TableBody(rows, _) => fmt.indentedElement("fo:table-body", e, addRowStyles(rows))
      case Caption(_, _)          => "" // replaced by Table renderer
      case Columns(_, _)          => "" // replaced by Table renderer
      case e: Column              => fmt.emptyElement("fo:table-column", e)
      case e: Row                 => fmt.indentedElement("fo:table-row", e)
      case e @ Cell(_, _, colspan, rowspan, _) =>
        fmt.indentedElement(
          "fo:table-cell",
          e,
          fmt.optAttributes(
            "number-columns-spanned" -> noneIfDefault(colspan, 1),
            "number-rows-spanned"    -> noneIfDefault(rowspan, 1)
          ): _*
        )
    }

    def renderNavigationItem(elem: NavigationItem): String = {

      val keepWithNext = Styles("keepWithNext")
      val keepWithPrev = Styles("keepWithPrevious")

      def avoidOrphan(content: Seq[NavigationItem]): Seq[NavigationItem] = content match {
        case init :+ last if last.content.isEmpty => init :+ last.mergeOptions(keepWithPrev)
        case empty                                => empty
      }

      elem match {
        case l: NavigationItem if l.hasStyle("bookmark")  => renderBookmark(l)
        case NavigationItem(
              title,
              content,
              Some(NavigationLink(target: InternalTarget, _, _)),
              _,
              opt
            ) =>
          val link = SpanLink(
            content = title.content :+ Leader() :+ PageNumberCitation(target),
            target = target
          )
          val keep = if (content.isEmpty) NoOpt else keepWithNext
          fmt.childPerLine(Paragraph(Seq(link), Style.nav + keep + opt) +: avoidOrphan(content))
        case NavigationItem(title, content, None, _, opt) =>
          fmt.childPerLine(
            Paragraph(title.content, Style.nav + keepWithNext + opt) +: avoidOrphan(content)
          )
        case _                                            => ""
      }
    }

    def renderUnresolvedReference(ref: Reference): String = {
      fmt.child(InvalidSpan(s"unresolved reference: $ref", ref.source))
    }

    def renderInvalidElement(elem: Invalid): String = elem match {
      case InvalidBlock(msg, _, fallback, opt) =>
        fmt.forMessage(msg)(fmt.child(Paragraph(List(msg), opt))) + fmt.child(fallback)
      case e                                   =>
        fmt.forMessage(e.message)(fmt.child(e.message) + " ") + fmt.child(e.fallback)
    }

    def renderRuntimeMessage(message: RuntimeMessage): String = {
      fmt.forMessage(message) {
        fmt.optRawElement(
          "fo:inline",
          message.withStyle(message.level.toString.toLowerCase),
          message.content
        )
      }
    }

    def renderContentWrapper(cw: ContentWrapper): String = {
      val inner = fmt.newLine + cw.content + fmt.newLine
      fmt.rawElement("fo:wrapper", cw, inner)
    }

    def renderPreamble(p: Preamble): String = {
      s"""
         |
         |<fo:block id="${
          FOFormatter.globalId(fmt.path, fmt.pathTranslator)
        }" page-break-before="always">
         |  <fo:marker marker-class-name="chapter"><fo:block>${
          fmt.text(p.title)
        }</fo:block></fo:marker>
         |</fo:block>""".stripMargin
    }

    def renderBookmarkTitle(title: BookmarkTitle): String =
      fmt.textElement("fo:bookmark-title", title)

    def renderBookmarkTree(tree: NavigationList): String =
      fmt.indentedElement("fo:bookmark-tree", tree)

    def renderBookmark(bookmark: NavigationItem): String = {
      def internalTarget(link: NavigationLink): Option[Path] = link.target match {
        case it: InternalTarget => Some(it.relativeTo(fmt.path).absolutePath)
        case _                  => None
      }

      val target = bookmark.link.orElse(bookmark.firstChildLink).flatMap(internalTarget)
      target.fold("") { targetPath =>
        val content = BookmarkTitle(bookmark.title.extractText) +: bookmark.content
        fmt.indentedElement(
          "fo:bookmark",
          bookmark,
          content,
          "internal-destination" -> globalId(targetPath, fmt.pathTranslator)
        )
      }
    }

    element match {
      case e: RuntimeMessage        => renderRuntimeMessage(e)
      case e: Table                 => renderTable(e)
      case e: TableElement          => renderTableElement(e)
      case e: NavigationItem        => renderNavigationItem(e)
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
