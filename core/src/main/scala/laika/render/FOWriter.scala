/*
 * Copyright 2014 the original author or authors.
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

  
  protected def attributes (tag: String, options: Options, attrs: Seq[(String,Any)]) = {
    filterAttributes(tag, ("id"->options.id) +: attrs)
  }
  
  protected def attributes (tag: String, element: Element, attrs: Seq[(String,Any)]) = {
    val fromCSS = styles.collectStyles(element, parents).toSeq
    val options = element match {
      case c: Customizable => c.options
      case _ => NoOpt
    }
    filterAttributes(tag, ("id"->options.id.map(buildId(path, _))) +: fromCSS ++: attrs)
  }
  
  
  def buildId (path: Path, ref: String): String = {
    val treePath = if (path.parent == Root) "" else path.parent.toString.replaceAllLiterally("/", "_")
    val docPath = if (path == Root) "" else treePath + "_" + path.basename
    docPath + "_" + ref
  }
  
  def blockContainer (element: Element, content: Seq[Block], attr: (String,String)*) = 
    this <<@ ("fo:block", element, attr: _*) <<|> content <<| "</fo:block>"
  
  def listBlock (element: Element, content: Seq[ListItem], attr: (String,String)*) = 
    this <<@ ("fo:list-block", element, attr: _*) <<|> content <<| "</fo:list-block>"
    
  def block (element: Element, content: Seq[Span], attr: (String,String)*) = 
    this <<@ ("fo:block", element, attr: _*) << content << "</fo:block>"
    
  def block (element: Element, attr: (String,String)*) = 
    this <<@/ ("fo:block", element, attr: _*)

  def blockWithWS (element: Element, content: Seq[Span], attr: (String,String)*) = 
    this <<@ ("fo:block", element, attr: _*) <<< content << "</fo:block>"
  
  def inline (element: Element, content: Seq[Span], attr: (String,String)*) = 
    this <<@ ("fo:inline", element, attr: _*) << content << "</fo:inline>"
    
  def internalLink (element: Element, target: String, content: Seq[Span], attr: (String,String)*) = 
    this <<@ ("fo:basic-link", element, (attr :+ ("internal-destination"->target)): _*) << content << "</fo:basic-link>"
  
  def externalLink (element: Element, url: String, content: Seq[Span], attr: (String,String)*) = 
    this <<@ ("fo:basic-link", element, (attr :+ ("external-destination"->url)): _*) << content << "</fo:basic-link>"
    
  def externalGraphic (element: Element, src: String) =
    this <<@/ ("fo:external-graphic", element, "src"->src,
        "inline-progression-dimension.maximum"->"100%", 
        "content-width"->"scale-down-to-fit")
        
  def listItem (element: Element, label: Seq[Span], body: Seq[Block], attr: (String,String)*) = {
    val content = List(ListItemLabel(Paragraph(label)), ListItemBody(body))
    this <<@ ("fo:list-item", element, attr: _*) <<|> content <<| "</fo:list-item>"
  }
   
  def listItemLabel (element: Element, content: Block, attr: (String,String)*) =
    this <<@ ("fo:list-item-label", element, attr :+ ("end-indent"->"label-end()"): _*) <<|> List(content) <<| "</fo:list-item-label>"
  
  def listItemBody (element: Element, content: Seq[Block], attr: (String,String)*) =
    this <<@ ("fo:list-item-body", element, attr :+ ("start-indent"->"body-start()"): _*) <<|> content <<| "</fo:list-item-body>"
  
  def footnote (element: Element, label: String, body: Seq[Block], options: Options) = {
    val labelElement = Text(s"[$label]", Styles("footnote-label"))
    val bodyElements = body match {
      case Paragraph(content, opts) +: rest => Paragraph(labelElement +: Text(" ") +: content, opts) +: rest
      case _ => Paragraph(List(labelElement)) +: body
    }
    this <<@ ("fo:footnote",element,Nil:_*) <<|> List(labelElement, FootnoteBody(bodyElements, options)) <<| "</fo:footnote>"
  }
  
  def text (element: Element, content: String, attr: (String,String)*) = {
    val attrs = attributes("fo:inline",element,attr)
    if (hasAttributes(attrs)) this <<@ ("fo:inline", NoOpt, attrs: _*) <<& content << "</fo:inline>"
    else this <<& content
  }
    
  def textWithWS (element: Element, content: String, attr: (String,String)*) = {
    val attrs = attributes("fo:inline",element,attr)
    if (hasAttributes(attrs)) this <<@ ("fo:inline", NoOpt, attrs: _*) <<<& content << "</fo:inline>"
    else this <<<& content
  }
  
  def textBlockWithWS (element: Element, content: String, attr: (String,String)*) = {
    val attrs = attributes("fo:block",element,attr)
    if (hasAttributes(attrs)) this <<@ ("fo:block", NoOpt, attrs: _*) <<<& content << "</fo:block>"
    else this <<<& content
  }
    
  def rawText (element: Element, content: String, attr: (String,String)*) = {
    val attrs = attributes("fo:inline",element,attr)
    if (hasAttributes(attrs)) this <<@ ("fo:inline", NoOpt, attrs: _*) << content << "</fo:inline>"
    else this << content
  }
  
  def hasAttributes (attrs: Seq[(String,Any)]) = attrs.exists(_._2 != None)
  
  def bookmarkTree (tree: BookmarkTree) =
    this <<@ ("fo:bookmark-tree", tree, Nil:_*) <<|> tree.bookmarks << "</fo:bookmark-tree>"
  
  def bookmark (bookmark: Bookmark) = {
    val content = BookmarkTitle(bookmark.title) +: bookmark.children
    this <<@ ("fo:bookmark", bookmark, ("internal-destination"->bookmark.ref)) <<|> content << "</fo:bookmark>"
  }
  
  def bookmarkTitle (title: BookmarkTitle) = {
    this <<@ ("fo:bookmark-title", title, Nil:_*) <<|> title.content << "</fo:bookmark-title>"
  }
  
 
}

object FOWriter {
  
  case class Leader (options: Options = NoOpt) extends Span 
  
  case class PageNumberCitation (ref: String, options: Options = NoOpt) extends Span
  
  case class ListItemLabel (content: Block, options: Options = NoOpt) extends Block
  
  case class ListItemBody (content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer[ListItemBody]
  
  case class FootnoteBody (content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer[FootnoteBody]
  
  case class BookmarkTree (bookmarks: Seq[Bookmark], options: Options = NoOpt) extends Block

  case class Bookmark (ref: String, title: String, children: Seq[Bookmark], options: Options = NoOpt) extends Block
  
  case class BookmarkTitle (content: String, options: Options = NoOpt) extends Block with TextContainer
  
}
