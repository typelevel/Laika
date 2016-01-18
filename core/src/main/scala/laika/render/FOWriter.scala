/*
 * Copyright 2014-2016 the original author or authors.
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

import laika.tree.Elements._
import laika.parse.css.Styles.StyleDeclarationSet
import laika.tree.Documents.Path
import laika.tree.Documents.Root
import FOWriter._ 

/** API for renderers that produce XSL-FO output.
 * 
 *  @param out the render function to write string values to
 *  @param render the render function for writing elements
 *  @param path the path of the document getting rendered, used for generating unique ids
 *  @param styles the styles to apply when writing the attributes of an element
 *  @param newLine the newline character to use
 *  @param formatted whether the output is formatted (adding indentation and newlines)
 * 
 *  @author Jens Halm
 */
class FOWriter (out: String => Unit,  
                render: Element => Unit, 
                path: Path,
                styles: StyleDeclarationSet,
                newLine: String = "\n",
                formatted: Boolean = true) extends TagWriter(out, render, newLine, formatted) 
                                           with FOProperties {

  
  protected def attributes (tag: String, options: Options, attrs: Seq[(String,Any)]): Seq[(String, Any)] = {
    filterAttributes(tag, ("id"->options.id) +: attrs)
  }
  
  protected def attributes (tag: String, element: Element, attrs: Seq[(String,Any)]): Seq[(String, Any)] = {
    val fromCSS = styles.collectStyles(element, parents).toSeq
    val options = element match {
      case c: Customizable => c.options
      case _ => NoOpt
    }
    filterAttributes(tag, ("id"->options.id.map(buildId(path, _))) +: fromCSS ++: attrs)
  }
  
  
  /** Generates a unique id for the specified path of the target
   *  document and the local reference.
   */
  def buildId (path: Path, ref: String): String = {
    val treePath = if (path.parent == Root) "" else path.parent.toString.replaceAllLiterally("/", "_")
    val docPath = if (path == Root) "" else treePath + "_" + path.basename
    docPath + "_" + ref
  }
  
  /** Renders an FO `block` element, containing nested blocks.
   */
  def blockContainer (element: Element, content: Seq[Block], attr: (String,String)*): FOWriter = 
    this <<@ ("fo:block", element, attr: _*) <<|> content <<| "</fo:block>"
  
  /** Renders an FO `list-block` element, and the specified list items.
   */
  def listBlock (element: Element, content: Seq[ListItem], attr: (String,String)*): FOWriter = 
    this <<@ ("fo:list-block", element, attr: _*) <<|> content <<| "</fo:list-block>"
    
  /** Renders an FO `block` element and the specified nested spans.
   */
  def block (element: Element, content: Seq[Span], attr: (String,String)*): FOWriter = 
    this <<@ ("fo:block", element, attr: _*) << content << "</fo:block>"
    
  /** Renders an empty FO `block` element.
   */
  def block (element: Element, attr: (String,String)*): FOWriter = 
    this <<@/ ("fo:block", element, attr: _*)

  /** Renders an FO `block` element and the specified nested spans, preserving
   *  all whitespace within the text elements of those spans.
   */
  def blockWithWS (element: Element, content: Seq[Span], attr: (String,String)*): FOWriter = 
    this <<@ ("fo:block", element, attr: _*) <<< content << "</fo:block>"
  
  /** Renders an FO `inline` element and the specified nested spans.
   */
  def inline (element: Element, content: Seq[Span], attr: (String,String)*): FOWriter = 
    this <<@ ("fo:inline", element, attr: _*) << content << "</fo:inline>"
    
  /** Renders an FO `basic-link` element for an internal target.
   */
  def internalLink (element: Element, target: String, content: Seq[Span], attr: (String,String)*): FOWriter = 
    this <<@ ("fo:basic-link", element, (attr :+ ("internal-destination"->target)): _*) << content << "</fo:basic-link>"
  
  /** Renders an FO `basic-link` element for an external target.
   */
  def externalLink (element: Element, url: String, content: Seq[Span], attr: (String,String)*): FOWriter = 
    this <<@ ("fo:basic-link", element, (attr :+ ("external-destination"->url)): _*) << content << "</fo:basic-link>"
  
  /** Renders an FO `external-graphic` element.
   */
  def externalGraphic (element: Element, src: String): FOWriter =
    this <<@/ ("fo:external-graphic", element, "src"->src,
        "inline-progression-dimension.maximum"->"100%", 
        "content-width"->"scale-down-to-fit")
   
  /** Renders an FO `list-item` element with the specified label and body.
   */
  def listItem (element: Element, label: Seq[Span], body: Seq[Block], attr: (String,String)*): FOWriter = {
    val content = List(ListItemLabel(Paragraph(label)), ListItemBody(body))
    this <<@ ("fo:list-item", element, attr: _*) <<|> content <<| "</fo:list-item>"
  }
   
  /** Renders an FO `list-item-label` element.
   */
  def listItemLabel (element: Element, content: Block, attr: (String,String)*): FOWriter =
    this <<@ ("fo:list-item-label", element, attr :+ ("end-indent"->"label-end()"): _*) <<|> List(content) <<| "</fo:list-item-label>"
  
  /** Renders an FO `list-item-body` element.
   */
  def listItemBody (element: Element, content: Seq[Block], attr: (String,String)*): FOWriter =
    this <<@ ("fo:list-item-body", element, attr :+ ("start-indent"->"body-start()"): _*) <<|> content <<| "</fo:list-item-body>"
  
  /** Renders an FO `footnote` element.
   */
  def footnote (element: Element, label: String, body: Seq[Block], options: Options): FOWriter = {
    val labelElement = Text(s"[$label]", Styles("footnote-label"))
    val bodyElements = body match {
      case Paragraph(content, opts) +: rest => Paragraph(labelElement +: Text(" ") +: content, opts) +: rest
      case _ => Paragraph(List(labelElement)) +: body
    }
    this <<@ ("fo:footnote",element,Nil:_*) <<|> List(labelElement, FootnoteBody(bodyElements, options)) <<| "</fo:footnote>"
  }
  
  /** Renders an FO `inline` element and the specified text. 
   *  Renders only the text itself in case there are no
   *  attributes associated with the text.
   */
  def text (element: Element, content: String, attr: (String,String)*): FOWriter = {
    val attrs = attributes("fo:inline",element,attr)
    if (hasAttributes(attrs)) this <<@ ("fo:inline", NoOpt, attrs: _*) <<& content << "</fo:inline>"
    else this <<& content
  }
    
  /** Renders an FO `inline` element and the specified text, preserving
   *  all whitespace. Renders only the text itself in case there are no
   *  attributes associated with the text.
   */
  def textWithWS (element: Element, content: String, attr: (String,String)*): FOWriter = {
    val attrs = attributes("fo:inline",element,attr)
    if (hasAttributes(attrs)) this <<@ ("fo:inline", NoOpt, attrs: _*) <<<& content << "</fo:inline>"
    else this <<<& content
  }
  
  /** Renders an FO `block` element and the specified text, preserving
   *  all whitespace. Renders only the text itself in case there are no
   *  attributes associated with the text.
   */
  def textBlockWithWS (element: Element, content: String, attr: (String,String)*): FOWriter = {
    val attrs = attributes("fo:block",element,attr)
    if (hasAttributes(attrs)) this <<@ ("fo:block", NoOpt, attrs: _*) <<<& content << "</fo:block>"
    else this <<<& content
  }
    
  /** Renders an FO `inline` element and the specified text, treating it as
   *  "raw", prerendered XSL-FO output. Renders only the text itself in case there are no
   *  attributes associated with the text.
   */
  def rawText (element: Element, content: String, attr: (String,String)*): FOWriter = {
    val attrs = attributes("fo:inline",element,attr)
    if (hasAttributes(attrs)) this <<@ ("fo:inline", NoOpt, attrs: _*) << content << "</fo:inline>"
    else this << content
  }
  
  private def hasAttributes (attrs: Seq[(String,Any)]): Boolean = attrs.exists(_._2 != None)
  
  /** Renders an FO `bookmark-tree` element and all of its nested bookmarks.
   */
  def bookmarkTree (tree: BookmarkTree): FOWriter =
    this <<@ ("fo:bookmark-tree", tree, Nil:_*) <<|> tree.bookmarks << "</fo:bookmark-tree>"
  
  /** Renders an FO `bookmark` element and all of its nested bookmarks.
   */
  def bookmark (bookmark: Bookmark): FOWriter = {
    val content = BookmarkTitle(bookmark.title) +: bookmark.children
    this <<@ ("fo:bookmark", bookmark, ("internal-destination" -> buildId(bookmark.path.absolute, bookmark.ref))) <<|> content << "</fo:bookmark>"
  }
  
  /** Renders an FO `bookmark-title` element.
   */
  def bookmarkTitle (title: BookmarkTitle): FOWriter = {
    this <<@ ("fo:bookmark-title", title, Nil:_*) <<|> title.content << "</fo:bookmark-title>"
  }
  
 
}

/** Companion providing tree elements specific to the XSL-FO renderer. 
 *  These are usually not part of the document AST produced by a parser,
 *  but only inserted dynamically during the render process to drive
 *  features specific to FO output. 
 */
object FOWriter {
  
  /** A leader element.
   */
  case class Leader (options: Options = NoOpt) extends Span 
  
  /** An internal link to be rendered as a page number.
   *  
   *  @param ref the local reference to the target element
   *  @param path the path of the target document containing the local reference
   *  @param options optional render hints
   */
  case class PageNumberCitation (ref: String, path: PathInfo, options: Options = NoOpt) extends Span

  /** A label for a list item, represented by a single Block element.
   */
  case class ListItemLabel (content: Block, options: Options = NoOpt) extends Block

  /** The body of a list item containing a sequence of block elements.
   */
  case class ListItemBody (content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer[ListItemBody]

  /** The body of a footnote containing a sequence of block elements.
   */
  case class FootnoteBody (content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer[FootnoteBody]
  
  /** An entire bookmark tree and its nested bookmarks,
   *  the top level element for FO bookmarks.
   */
  case class BookmarkTree (bookmarks: Seq[Bookmark], options: Options = NoOpt) extends Block

  /** A single bookmark and its nested children.
   */
  case class Bookmark (ref: String, path: PathInfo, title: String, children: Seq[Bookmark], options: Options = NoOpt) extends Block
  
  /** A bookmark title. 
   */
  case class BookmarkTitle (content: String, options: Options = NoOpt) extends Block with TextContainer
  
}
