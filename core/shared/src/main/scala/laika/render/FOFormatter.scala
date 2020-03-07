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
import laika.factory.RenderContext

/** API for renderers that produce XSL-FO output.
 * 
 * @param renderChild the function to use for rendering child elements
 * @param currentElement the active element currently being rendered                             
 * @param parents the stack of parent elements of this formatter in recursive rendering, 
 *                with the root element being the last in the list
 * @param path        the virtual path of the document getting rendered, used for generating unique ids
 * @param styles      the styles to apply when writing the attributes of an element
 * @param indentation the indentation mechanism for this formatter
 * @param messageLevel the minimum severity level for a system message to be rendered                     
 * 
 * @author Jens Halm
 */
case class FOFormatter (renderChild: (FOFormatter, Element) => String,
                        currentElement: Element,
                        parents: List[Element],
                        path: Path,
                        styles: StyleDeclarationSet,
                        indentation: Indentation,
                        messageLevel: MessageLevel) extends 
  TagFormatter[FOFormatter](renderChild, currentElement, parents, indentation, messageLevel) with FOProperties {

  type StyleHint = Element

  protected def withChild (element: Element): FOFormatter = copy(parents = currentElement :: parents, currentElement = element)

  protected def withIndentation (newIndentation: Indentation): FOFormatter = copy(indentation = newIndentation)

  private lazy val (footnotes, citations) = parents.lastOption.getOrElse(currentElement) match {
    case et: ElementTraversal => (
      et collect { case f: Footnote if f.options.id.isDefined => (f.options.id.get, f) } toMap,
      et collect { case c: Citation if c.options.id.isDefined => (c.options.id.get, c) } toMap
    )
    case _ => (Map.empty[String,Footnote], Map.empty[String,Citation])
  }

  import FOFormatter._

  def attributes (tag: String, element: Element, attrs: Seq[(String, String)]): String = {
    val fromCSS = styles.collectStyles(element, parents)
    val combinedAttrs = (fromCSS ++ attrs).toSeq.sortBy(_._1)

    val options = element match {
      case c: Customizable => c.options
      case _ => NoOpt
    }
    val idAttr = options.id.map(id => "id"-> buildLocalId(id)).toSeq
    
    attributes(filterAttributes(tag, idAttr ++ combinedAttrs))
  }

  /** Obtains a Footnote with the specified reference name and, if it exists, 
    * passes it to the provided render function.
    */
  def withFootnote (ref: String)(f: Footnote => String): String = footnotes.get(ref).fold("")(f)

  /** Obtains a Citation with the specified reference name and, if it exists, 
    * passes it to the provided render function.
    */
  def withCitation (ref: String)(f: Citation => String): String = citations.get(ref).fold("")(f)


  /** Generates an id that is unique within the entire document tree for the 
    * specified path of the target document and its local reference.
   */
  def buildId (path: Path, ref: String): String = {
    val treePath = if (path.parent == Path.Root) "" else path.parent.toString.replaceAllLiterally("/", "_")
    val docPath = if (path == Path.Root) "" else treePath + "_" + path.basename
    docPath + "_" + ref
  }

  /** Generates an id that is unique within the entire document tree for the 
    * specified local reference.
    */
  def buildLocalId (ref: String): String = buildId(path, ref)

  /** Renders an FO `block` element, containing nested blocks.
   *  The content will be rendered indented one level to the right.
   */
  def blockContainer (styleHint: Element, content: Seq[Block], attr: (String,String)*): String = 
    indentedElement("fo:block", styleHint, content, attr: _*)

  /** Renders an FO `list-block` element, and the specified list items.
   *  The content will be rendered indented one level to the right.
   */
  def listBlock (styleHint: Element, content: Seq[ListItem], attr: (String,String)*): String =
    indentedElement("fo:list-block", styleHint, content, attr: _*)

  /** Renders an FO `block` element and the specified nested spans on the same line.
   */
  def block (styleHint: Element, content: Seq[Span], attr: (String,String)*): String = 
    element("fo:block", styleHint, content, attr: _*)

  /** Renders an empty FO `block` element.
   */
  def block (styleHint: Element, attr: (String,String)*): String = 
    emptyElement("fo:block", styleHint, attr: _*)

  /** Renders an FO `block` element and the specified nested spans, preserving
   *  all whitespace within the text elements of those spans.
   */
  def blockWithWS (styleHint: Element, content: Seq[Span], attr: (String,String)*): String = 
    withoutIndentation(_.element("fo:block", styleHint, content, attr: _*))

  /** Renders an FO `inline` element and the specified nested spans on the same line.
   */
  def inline (styleHint: Element, content: Seq[Span], attr: (String,String)*): String =
    element("fo:inline", styleHint, content, attr: _*)

  /** Renders an FO `basic-link` element for an internal target.
   */
  def internalLink (styleHint: Element, target: String, content: Seq[Span], attr: (String,String)*): String =
    element("fo:basic-link", styleHint, content, attr :+ ("internal-destination" -> target): _*)

  /** Renders an FO `block` or `inline` element for this internal link
   *  target, depending on whether it is inside a `BlockContainer`
   *  or `SpanContainer`.
   */
  def internalLinkTarget (element: Element): String = {
    parents.head match {
      case _: BlockContainer => block(element)
      case _ => inline(element, Nil)
    }
  }

  /** Renders an FO `basic-link` element for an external target.
   */
  def externalLink (styleHint: Element, url: String, content: Seq[Span], attr: (String,String)*): String =
    element("fo:basic-link", styleHint, content, attr :+ ("external-destination" -> url): _*)

  /** Renders an FO `external-graphic` element.
   */
  def externalGraphic (styleHint: Element, src: String, width: Option[Size], height: Option[Size]): String =
    emptyElement("fo:external-graphic", styleHint, optAttributes("src" -> Some(src),
                 "width" -> width.map(_.displayValue), "height" -> height.map(_.displayValue)):_*)

  /** Renders an FO `list-item` element with the specified label and body.
   *  The content will be rendered indented one level to the right.
   */
  def listItem (styleHint: Element, label: Seq[Span], body: Seq[Block], attr: (String,String)*): String = {
    val content = List(ListItemLabel(Paragraph(label)), ListItemBody(body))
    indentedElement("fo:list-item", styleHint, content, attr: _*)
  }

  /** Renders an FO `list-item-label` element, with the content indented one level to the right.
   */
  def listItemLabel (styleHint: Element, content: Block, attr: (String,String)*): String =
    indentedElement("fo:list-item-label", styleHint, Seq(content), attr :+ ("end-indent"->"label-end()"): _*)

  /** Renders an FO `list-item-body` element, with the content indented one level to the right.
   */
  def listItemBody (styleHint: Element, content: Seq[Block], attr: (String,String)*): String =
    indentedElement("fo:list-item-body", styleHint, content, attr :+ ("start-indent"->"body-start()"): _*)

  /** Renders an FO `footnote` element, with the body indented one level to the right.
   */
  def footnote (styleHint: Element, label: String, body: Seq[Block], options: Options): String = {
    val labelElement = Text(s"[$label]", Styles("footnote-label"))
    val bodyElements = body match {
      case Paragraph(spans, opts) +: rest => Paragraph(labelElement +: Text(" ") +: spans, opts) +: rest
      case _ => Paragraph(labelElement) +: body
    }
    val content = List(labelElement, FootnoteBody(bodyElements, options))
    indentedElement("fo:footnote", styleHint, content)
  }

  private def optRawElement (tagName: String, styleHint: Element, content: String, attrs: (String,String)*): String = {
    val renderedAttrs = attributes(tagName, styleHint, attrs)
    if (renderedAttrs.nonEmpty) s"<$tagName$renderedAttrs>$content</$tagName>"
    else content
  }

  /** Renders an FO `inline` element and the specified text. 
   *  Renders only the text itself in case there are no
   *  attributes associated with the text.
   */
  def text (styleHint: Element, content: String, attr: (String,String)*): String = 
    optRawElement("fo:inline", styleHint, text(content), attr: _*)

  /** Renders an FO `inline` element and the specified text, preserving
   *  all whitespace. Renders only the text itself in case there are no
   *  attributes associated with the text.
   */
  def textWithWS (styleHint: Element, content: String, attr: (String,String)*): String =
    optRawElement("fo:inline", styleHint, withoutIndentation(_.text(content)), attr: _*)

  /** Renders an FO `block` element and the specified text, preserving
   *  all whitespace. Renders only the text itself in case there are no
   *  attributes associated with the text.
   */
  def textBlockWithWS (styleHint: Element, content: String, attr: (String,String)*): String = 
    optRawElement("fo:block", styleHint, withoutIndentation(_.text(content)), attr: _*)

  /** Renders an FO `inline` element and the specified text, treating it as
   *  "raw", pre-rendered XSL-FO output, so that no escaping of special character will be performed. 
   *  Renders only the text itself in case there are no attributes associated with the text.
   */
  def rawText (styleHint: Element, content: String, attr: (String,String)*): String =
    optRawElement("fo:inline", styleHint, content, attr: _*)

  /** Renders an FO `bookmark-tree` element and all of its nested bookmarks.
   */
  def bookmarkTree (tree: BookmarkTree): String =
    indentedElement("fo:bookmark-tree", tree, tree.bookmarks)

  /** Renders an FO `bookmark` element and all of its nested bookmarks.
   */
  def bookmark (bookmark: Bookmark): String = {
    val content = BookmarkTitle(bookmark.title) +: bookmark.children
    indentedElement("fo:bookmark", bookmark, content, "internal-destination" -> buildId(bookmark.path.absolute, bookmark.ref))
  }

  /** Renders an FO `bookmark-title` element.
   */
  def bookmarkTitle (title: BookmarkTitle): String =
    textElement("fo:bookmark-title", title, title.content)

}

/** Companion providing tree elements specific to the XSL-FO renderer. 
  *  These are usually not part of the document AST produced by a parser,
  *  but only inserted dynamically during the render process to drive
  *  features specific to FO output. 
  */
object FOFormatter extends (RenderContext[FOFormatter] => FOFormatter) {

  /** A leader element.
    */
  case class Leader (options: Options = NoOpt) extends Span {
    type Self = Leader
    def withOptions (options: Options): Leader = copy(options = options)
  }

  /** An internal link to be rendered as a page number.
    *
    *  @param ref the local reference to the target element
    *  @param path the path of the target document containing the local reference
    *  @param options optional render hints
    */
  case class PageNumberCitation (ref: String, path: LinkPath, options: Options = NoOpt) extends Span {
    type Self = PageNumberCitation
    def withOptions (options: Options): PageNumberCitation = copy(options = options)
  }

  /** A label for a list item, represented by a single Block element.
    */
  case class ListItemLabel (content: Block, options: Options = NoOpt) extends Block {
    type Self = ListItemLabel
    def withOptions (options: Options): ListItemLabel = copy(options = options)
  }

  /** The body of a list item containing a sequence of block elements.
    */
  case class ListItemBody (content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer {
    type Self = ListItemBody
    def withContent (newContent: Seq[Block]): ListItemBody = copy(content = content)
    def withOptions (options: Options): ListItemBody = copy(options = options)
  }

  /** The body of a footnote containing a sequence of block elements.
    */
  case class FootnoteBody (content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer {
    type Self = FootnoteBody
    def withContent (newContent: Seq[Block]): FootnoteBody = copy(content = content)
    def withOptions (options: Options): FootnoteBody = copy(options = options)
  }

  /** An entire bookmark tree and its nested bookmarks,
    * the top level element for FO bookmarks.
    */
  case class BookmarkTree (bookmarks: Seq[Bookmark], options: Options = NoOpt) extends Block {
    type Self = BookmarkTree
    def withOptions (options: Options): BookmarkTree = copy(options = options)
  }

  /** A single bookmark and its nested children.
    */
  case class Bookmark (ref: String, path: LinkPath, title: String, children: Seq[Bookmark], options: Options = NoOpt) extends Block {
    type Self = Bookmark
    def withOptions (options: Options): Bookmark = copy(options = options)
  }

  /** A bookmark title. 
    */
  case class BookmarkTitle (content: String, options: Options = NoOpt) extends Block with TextContainer {
    type Self = BookmarkTitle
    def withOptions (options: Options): BookmarkTitle = copy(options = options)
  }

  /** Creates a new formatter instance based on the specified render context.
    */
  def apply(context: RenderContext[FOFormatter]): FOFormatter =
    FOFormatter(context.renderChild, context.root, Nil, context.path, context.styles, context.indentation, context.config.minMessageLevel)
  
}

