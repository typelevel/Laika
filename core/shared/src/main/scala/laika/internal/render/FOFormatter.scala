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

package laika.internal.render

import laika.api.bundle.PathTranslator
import laika.api.format.{ Formatter, TagFormatter }
import laika.ast.Path.Root
import laika.ast.*

private[laika] class FOFormatter private (
    protected val context: Formatter.Context[TagFormatter]
) extends TagFormatter with FOProperties {

  protected def withChild(element: Element): Rep =
    new FOFormatter(context.forChildElement(element))

  protected def withIndentation(newIndentation: Formatter.Indentation): Rep =
    new FOFormatter(context.withIndentation(newIndentation))

  def attributes(tag: String, element: Element, attrs: Seq[(String, String)]): String = {
    val fromCSS       = styles.collectStyles(element, parents)
    val combinedAttrs = (fromCSS ++ attrs).toSeq.sortBy(_._1)

    def localId(ref: String): String = FOFormatter.localId(path, ref, pathTranslator)
    val idAttr                       = element.options.id.map(id => "id" -> localId(id)).toSeq

    attributes(filterAttributes(tag, idAttr ++ combinedAttrs) *)
  }

}

/** Companion providing tree elements specific to the XSL-FO renderer.
  * These are usually not part of the document AST produced by a parser,
  * but only inserted dynamically during the render process to drive features specific to FO output.
  */
private[laika] object FOFormatter extends (Formatter.Context[TagFormatter] => TagFormatter) {

  def localId(path: Path, ref: String, translator: PathTranslator): String = {
    val basePath = {
      if (ref.isEmpty) path
      else if (path == Root) Path.parse(s"#$ref")
      else path.withFragment(ref)
    }
    FOFormatter.globalId(basePath, translator)
  }

  def globalId(path: Path, translator: PathTranslator): String = {
    if (path == Path.Root) ""
    else
      translator
        .translate(path)
        .withoutSuffix
        .toString
        .replace("/", "_")
        .replace("#", "_")
  }

  trait FormatterSyntax {

    implicit class FOFormatterSyntax(val fmt: TagFormatter) {

      /** Generates an id that is unique within the entire document tree for the
        * specified local reference.
        */
      def localId(ref: String): String = FOFormatter.localId(fmt.path, ref, fmt.pathTranslator)

      /** Generates an id that is unique within the entire document tree for the
        * specified path of the target document and its local reference.
        */
      def globalId(path: Path): String = FOFormatter.globalId(path, fmt.pathTranslator)

      /** Renders an element with the specified tag name, attributes derived from the style hint
        * and indented content consisting of the provided child elements.
        *
        * In contrast to the overload on the `TagFormatter` type, this method allows to keep
        * the element serving as a render hint separate from its rendered content.
        */
      def indentedElement(
          tagName: String,
          styleHint: Element,
          content: Seq[Element],
          attrs: (String, String)*
      ): String =
        s"<$tagName${fmt.attributes(tagName, styleHint, attrs)}>${fmt.indentedChildren(content)}${
            fmt.newLine
          }</$tagName>"

      /** Renders an element with the specified tag name, attributes derived from the style hint
        * and content based on the provided string that is interpreted as already rendered in the target format.
        * That means that no character escaping will be performed on the provided content.
        *
        * In contrast to the `rawElement` method, this method will omit the surrounding
        * element altogether if it would not contain any attributes.
        */
      def optRawElement(
          tagName: String,
          styleHint: Element,
          content: String,
          attrs: (String, String)*
      ): String = {
        val renderedAttrs = fmt.attributes(tagName, styleHint, attrs)
        if (renderedAttrs.nonEmpty) s"<$tagName$renderedAttrs>$content</$tagName>"
        else content
      }

      /** Renders an FO `block` element and its children on the same line.
        */
      def block(container: ElementContainer[_ <: Element], attr: (String, String)*): String =
        fmt.element("fo:block", container, attr *)

      /** Renders an FO `block` element and the specified nested spans,
        * preserving all whitespace within the text elements of those spans.
        */
      def blockWithWS(container: ElementContainer[_ <: Element], attr: (String, String)*): String =
        fmt.withoutIndentation(_.element("fo:block", container, attr *))

      /** Renders an FO `inline` element and its children on the same line.
        */
      def inline(container: ElementContainer[_ <: Element], attr: (String, String)*): String =
        fmt.element("fo:inline", container, attr *)

      /** Renders an FO `block` element, containing nested blocks.
        * The content will be rendered indented one level to the right.
        */
      def blockContainer(styleHint: Element, content: Seq[Block], attr: (String, String)*): String =
        fmt.indentedElement("fo:block", styleHint, content, attr *)

      /** Renders an FO `list-block` element, and the specified list items.
        * The content will be rendered indented one level to the right.
        */
      def listBlock(container: ListContainer, attr: (String, String)*): String =
        fmt.indentedElement("fo:list-block", container, attr *)

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

    }

  }

  /** A wrapper around pre-rendered content which can be used to set default
    * attributes that can be inherited by any node in the document.
    */
  case class ContentWrapper(content: String, options: Options = Options.empty) extends Block {
    type Self = ContentWrapper

    def withOptions(options: Options): ContentWrapper = copy(options = options)
  }

  /** A preamble for a document, only used in PDF output where multiple XSL-FO documents get concatenated
    * before being passed to the PDF renderer.
    */
  case class Preamble(title: String, options: Options = Options.empty) extends Block {
    type Self = Preamble
    def withOptions(options: Options): Preamble = copy(options = options)
  }

  /** A leader element.
    */
  case class Leader(options: Options = Options.empty) extends Span {
    type Self = Leader
    def withOptions(options: Options): Leader = copy(options = options)
  }

  /** An internal link to be rendered as a page number.
    *
    *  @param target the path of the target document containing the local reference
    *  @param options optional render hints
    */
  case class PageNumberCitation(target: InternalTarget, options: Options = Options.empty)
      extends Span {
    type Self = PageNumberCitation
    def withOptions(options: Options): PageNumberCitation = copy(options = options)
  }

  /** A label for a list item, represented by a single Block element.
    */
  case class ListItemLabel(content: Block, options: Options = Options.empty) extends Block {
    type Self = ListItemLabel
    def withOptions(options: Options): ListItemLabel = copy(options = options)
  }

  /** The body of a list item containing a sequence of block elements.
    */
  case class ListItemBody(content: Seq[Block], options: Options = Options.empty) extends Block
      with BlockContainer {
    type Self = ListItemBody
    def withContent(newContent: Seq[Block]): ListItemBody = copy(content = newContent)
    def withOptions(options: Options): ListItemBody       = copy(options = options)
  }

  /** The body of a footnote containing a sequence of block elements.
    */
  case class FootnoteBody(content: Seq[Block], options: Options = Options.empty) extends Block
      with BlockContainer {
    type Self = FootnoteBody
    def withContent(newContent: Seq[Block]): FootnoteBody = copy(content = newContent)
    def withOptions(options: Options): FootnoteBody       = copy(options = options)
  }

  /** A bookmark title.
    */
  case class BookmarkTitle(content: String, options: Options = Options.empty) extends Block
      with TextContainer {
    type Self = BookmarkTitle
    def withOptions(options: Options): BookmarkTitle = copy(options = options)
  }

  /** Creates a new formatter instance based on the specified render context.
    */
  def apply(context: Formatter.Context[TagFormatter]): TagFormatter =
    new FOFormatter(context)

}
