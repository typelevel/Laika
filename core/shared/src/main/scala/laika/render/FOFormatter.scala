/*
 * Copyright 2012-2023 the original author or authors.
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

import laika.ast.Path.Root
import laika.ast.{ Path, * }
import laika.factory.RenderContext
import laika.rewrite.nav.PathTranslator

private[laika] class FOFormatter private (
    protected val context: RenderContext[TagFormatter]
) extends TagFormatter with FOProperties {

  protected def withChild(element: Element): Rep =
    new FOFormatter(context.forChildElement(element))

  protected def withIndentation(newIndentation: Formatter.Indentation): Rep =
    new FOFormatter(context.withIndentation(newIndentation))

  def attributes(tag: String, element: Element, attrs: Seq[(String, String)]): String = {
    val fromCSS       = styles.collectStyles(element, parents)
    val combinedAttrs = (fromCSS ++ attrs).toSeq.sortBy(_._1)

    val idAttr = element.options.id.map(id => "id" -> buildLocalId(id)).toSeq

    attributes(filterAttributes(tag, idAttr ++ combinedAttrs))
  }

  /** Generates an id that is unique within the entire document tree for the
    * specified local reference.
    */
  private def buildLocalId(ref: String): String = {
    if (ref.isEmpty) FOFormatter.buildId(path, pathTranslator)
    else if (path == Root) FOFormatter.buildId(Path.parse(s"#$ref"), pathTranslator)
    else FOFormatter.buildId(path.withFragment(ref), pathTranslator)
  }

}

/** Companion providing tree elements specific to the XSL-FO renderer.
  * These are usually not part of the document AST produced by a parser,
  * but only inserted dynamically during the render process to drive features specific to FO output.
  */
object FOFormatter extends (RenderContext[TagFormatter] => TagFormatter) {

  /** Generates an id that is unique within the entire document tree for the
    * specified path of the target document and its local reference.
    */
  def buildId(path: Path, translator: PathTranslator): String = {
    if (path == Path.Root) ""
    else
      translator
        .translate(path)
        .withoutSuffix
        .toString
        .replace("/", "_")
        .replace("#", "_")
  }

  implicit class FOFormatterSyntax(val fmt: TagFormatter) {

    private lazy val (footnotes, citations) =
      fmt.parents.lastOption.getOrElse(fmt.currentElement) match {
        case et: ElementTraversal =>
          (
            et.collect { case f @ Footnote(_, _, Id(id)) => (id, f) }.toMap,
            et.collect { case c @ Citation(_, _, Id(id)) => (id, c) }.toMap
          )
        case _                    => (Map.empty[String, Footnote], Map.empty[String, Citation])
      }

    /** Renders an element with the specified tag name, attributes derived from the style hint
      * and content consisting of the provided child elements, all rendered on the same line.
      */
    def element(
        tagName: String,
        styleHint: Element,
        content: Seq[Element],
        attrs: (String, String)*
    ): String =
      s"<$tagName${fmt.attributes(tagName, styleHint, attrs)}>${fmt.children(content)}</$tagName>"

    /** Renders an element with the specified tag name, attributes derived from the style hint
      * and indented content consisting of the provided child elements.
      */
    def indentedElement(
        tagName: String,
        styleHint: Element,
        content: Seq[Element],
        attrs: (String, String)*
    ): String =
      s"<$tagName${fmt.attributes(tagName, styleHint, attrs)}>${fmt.indentedChildren(content)}${fmt.newLine}</$tagName>"

    /** Renders an FO `block` element and the specified nested spans on the same line.
      */
    def block(styleHint: Element, content: Seq[Span], attr: (String, String)*): String =
      fmt.element("fo:block", styleHint, content, attr *)

    /** Renders an empty FO `block` element.
      */
    def block(styleHint: Element, attr: (String, String)*): String =
      fmt.emptyElement("fo:block", styleHint, attr *)

    /** Renders an FO `block` element and the specified nested spans,
      * preserving all whitespace within the text elements of those spans.
      */
    def blockWithWS(styleHint: Element, content: Seq[Span], attr: (String, String)*): String =
      fmt.withoutIndentation(_.element("fo:block", styleHint, content, attr *))

    /** Renders an FO `inline` element and the specified nested spans on the same line.
      */
    def inline(styleHint: Element, content: Seq[Span], attr: (String, String)*): String =
      fmt.element("fo:inline", styleHint, content, attr *)

    /** Renders an FO `block` element, containing nested blocks.
      * The content will be rendered indented one level to the right.
      */
    def blockContainer(styleHint: Element, content: Seq[Block], attr: (String, String)*): String =
      fmt.indentedElement("fo:block", styleHint, content, attr: _*)

    /** Renders an FO `list-block` element, and the specified list items.
      * The content will be rendered indented one level to the right.
      */
    def listBlock(styleHint: Element, content: Seq[ListItem], attr: (String, String)*): String =
      fmt.indentedElement("fo:list-block", styleHint, content, attr: _*)

    /** Renders an FO `basic-link` element for an internal target.
      */
    def internalLink(
        styleHint: Element,
        target: Path,
        content: Seq[Span],
        attr: (String, String)*
    ): String =
      fmt.element(
        "fo:basic-link",
        styleHint,
        content,
        attr :+ ("internal-destination" -> FOFormatter.buildId(target, fmt.pathTranslator)): _*
      )

    /** Renders an FO `block` or `inline` element for this internal link
      * target, depending on whether it is inside a `BlockContainer`
      * or `SpanContainer`.
      */
    def internalLinkTarget(element: Element): String = {
      fmt.parents.head match {
        case _: BlockContainer => block(element)
        case _                 => this.inline(element, Nil)
      }
    }

    /** Renders an FO `basic-link` element for an external target.
      */
    def externalLink(
        styleHint: Element,
        url: String,
        content: Seq[Span],
        attr: (String, String)*
    ): String =
      fmt.element("fo:basic-link", styleHint, content, attr :+ ("external-destination" -> url): _*)

    /** Renders an FO `external-graphic` element.
      */
    def externalGraphic(
        styleHint: Element,
        src: String,
        width: Option[Length],
        height: Option[Length]
    ): String =
      fmt.emptyElement(
        "fo:external-graphic",
        styleHint,
        fmt.optAttributes(
          "src"    -> Some(src),
          "width"  -> width.map(_.displayValue),
          "height" -> height.map(_.displayValue)
        ): _*
      )

    /** Renders an FO `list-item` element with the specified label and body.
      * The content will be rendered indented one level to the right.
      */
    def listItem(
        styleHint: Element,
        label: Seq[Span],
        body: Seq[Block],
        attr: (String, String)*
    ): String = {
      val content = List(ListItemLabel(Paragraph(label)), ListItemBody(body))
      fmt.indentedElement("fo:list-item", styleHint, content, attr: _*)
    }

    /** Renders an FO `list-item-label` element, with the content indented one level to the right.
      */
    def listItemLabel(styleHint: Element, content: Block, attr: (String, String)*): String =
      fmt.indentedElement(
        "fo:list-item-label",
        styleHint,
        Seq(content),
        attr :+ ("end-indent" -> "label-end()"): _*
      )

    /** Renders an FO `list-item-body` element, with the content indented one level to the right.
      */
    def listItemBody(styleHint: Element, content: Seq[Block], attr: (String, String)*): String =
      fmt.indentedElement(
        "fo:list-item-body",
        styleHint,
        content,
        attr :+ ("start-indent" -> "body-start()"): _*
      )

    /** Renders an FO `footnote` element, with the body indented one level to the right.
      */
    def footnote(styleHint: Element, label: String, body: Seq[Block], options: Options): String = {
      val labelElement = Text(s"[$label]", Style.footnoteLabel)
      val bodyElements = body match {
        case Paragraph(spans, opts) +: rest =>
          Paragraph(labelElement +: Text(" ") +: spans, opts) +: rest
        case _                              => Paragraph(labelElement) +: body
      }
      val content      = List(labelElement, FootnoteBody(bodyElements, options))
      fmt.indentedElement("fo:footnote", styleHint, content)
    }

    /** Obtains a Footnote with the specified reference name and, if it exists,
      * passes it to the provided render function.
      */
    def withFootnote(ref: String)(f: Footnote => String): String = footnotes.get(ref).fold("")(f)

    /** Obtains a Citation with the specified reference name and, if it exists,
      * passes it to the provided render function.
      */
    def withCitation(ref: String)(f: Citation => String): String = citations.get(ref).fold("")(f)

    /** Renders an FO `inline` element and the specified text.
      * Renders only the text itself in case there are no
      * attributes associated with the text.
      */
    def text(styleHint: Element, content: String, attr: (String, String)*): String =
      optRawElement("fo:inline", styleHint, fmt.text(content), attr: _*)

    /** Renders an FO `inline` element and the specified text, preserving
      * all whitespace. Renders only the text itself in case there are no
      * attributes associated with the text.
      */
    def textWithWS(styleHint: Element, content: String, attr: (String, String)*): String =
      optRawElement("fo:inline", styleHint, fmt.withoutIndentation(_.text(content)), attr: _*)

    /** Renders an FO `block` element and the specified text, preserving
      * all whitespace. Renders only the text itself in case there are no
      * attributes associated with the text.
      */
    def textBlockWithWS(styleHint: Element, content: String, attr: (String, String)*): String =
      optRawElement("fo:block", styleHint, fmt.withoutIndentation(_.text(content)), attr: _*)

    /** Renders an FO `inline` element and the specified text, treating it as
      * "raw", pre-rendered XSL-FO output, so that no escaping of special character will be performed.
      * Renders only the text itself in case there are no attributes associated with the text.
      */
    def rawText(styleHint: Element, content: String, attr: (String, String)*): String =
      optRawElement("fo:inline", styleHint, content, attr: _*)

    private def optRawElement(
        tagName: String,
        styleHint: Element,
        content: String,
        attrs: (String, String)*
    ): String = {
      val renderedAttrs = fmt.attributes(tagName, styleHint, attrs)
      if (renderedAttrs.nonEmpty) s"<$tagName$renderedAttrs>$content</$tagName>"
      else content
    }

    /** Renders an FO `bookmark-tree` element and all of its nested bookmarks.
      */
    def bookmarkTree(tree: NavigationList): String =
      fmt.indentedElement("fo:bookmark-tree", tree, tree.content)

    /** Renders an FO `bookmark` element and all of its nested bookmarks.
      */
    def bookmark(bookmark: NavigationItem): String = {
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
          "internal-destination" -> buildId(targetPath, fmt.pathTranslator)
        )
      }
    }

    /** Renders an FO `bookmark-title` element.
      */
    def bookmarkTitle(title: BookmarkTitle): String =
      fmt.textElement("fo:bookmark-title", title)

  }

  /** A wrapper around pre-rendered content which can be used to set default
    * attributes that can be inherited by any node in the document.
    */
  case class ContentWrapper(content: String, options: Options = NoOpt) extends Block {
    type Self = ContentWrapper

    def withOptions(options: Options): ContentWrapper = copy(options = options)
  }

  /** A preamble for a document, only used in PDF output where multiple XSL-FO documents get concatenated
    * before being passed to the PDF renderer.
    */
  case class Preamble(title: String, options: Options = NoOpt) extends Block {
    type Self = Preamble
    def withOptions(options: Options): Preamble = copy(options = options)
  }

  /** A leader element.
    */
  case class Leader(options: Options = NoOpt) extends Span {
    type Self = Leader
    def withOptions(options: Options): Leader = copy(options = options)
  }

  /** An internal link to be rendered as a page number.
    *
    *  @param target the path of the target document containing the local reference
    *  @param options optional render hints
    */
  case class PageNumberCitation(target: InternalTarget, options: Options = NoOpt) extends Span {
    type Self = PageNumberCitation
    def withOptions(options: Options): PageNumberCitation = copy(options = options)
  }

  /** A label for a list item, represented by a single Block element.
    */
  case class ListItemLabel(content: Block, options: Options = NoOpt) extends Block {
    type Self = ListItemLabel
    def withOptions(options: Options): ListItemLabel = copy(options = options)
  }

  /** The body of a list item containing a sequence of block elements.
    */
  case class ListItemBody(content: Seq[Block], options: Options = NoOpt) extends Block
      with BlockContainer {
    type Self = ListItemBody
    def withContent(newContent: Seq[Block]): ListItemBody = copy(content = newContent)
    def withOptions(options: Options): ListItemBody       = copy(options = options)
  }

  /** The body of a footnote containing a sequence of block elements.
    */
  case class FootnoteBody(content: Seq[Block], options: Options = NoOpt) extends Block
      with BlockContainer {
    type Self = FootnoteBody
    def withContent(newContent: Seq[Block]): FootnoteBody = copy(content = newContent)
    def withOptions(options: Options): FootnoteBody       = copy(options = options)
  }

  /** A bookmark title.
    */
  case class BookmarkTitle(content: String, options: Options = NoOpt) extends Block
      with TextContainer {
    type Self = BookmarkTitle
    def withOptions(options: Options): BookmarkTitle = copy(options = options)
  }

  /** Creates a new formatter instance based on the specified render context.
    */
  def apply(context: RenderContext[TagFormatter]): TagFormatter =
    new FOFormatter(context)

}
