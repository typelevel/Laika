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
import FOWriter._ 

/** API for renderers that produce XSL-FO output.
 * 
 *  @param out the render function to write string values to
 *  @param render the render function for writing elements
 *  @param newLine the newline character to use
 *  @param formatted whether the output is formatted (adding indentation and newlines)
 * 
 *  @author Jens Halm
 */
class FOWriter (out: String => Unit,  
                render: Element => Unit, 
                styles: StyleDeclarationSet,
                newLine: String = "\n",
                formatted: Boolean = true) extends TagWriter(out, render, newLine, formatted) {

  
  protected def attributes (tag: String, options: Options, attrs: Seq[(String,Any)]) = {
    ("id"->options.id) +: attrs
  }
  
  protected def attributes (tag: String, element: Element, attrs: Seq[(String,Any)]) = {
    val fromCSS = styles.collectStyles(element, parents).toSeq
    val options = element match {
      case c: Customizable => c.options
      case _ => NoOpt
    }
    ("id"->options.id) +: fromCSS ++: attrs
  }
  
  
  def blockContainer (opt: Options, content: Seq[Block], attr: (String,String)*) = 
    this <<@ ("fo:block", NoOpt, attr: _*) <<|> content <<| "</fo:block>"
  
  def listBlock (opt: Options, content: Seq[ListItem], attr: (String,String)*) = 
    this <<@ ("fo:list-block", NoOpt, attr: _*) <<|> content <<| "</fo:list-block>"
    
  def block (opt: Options, content: Seq[Span], attr: (String,String)*) = 
    this <<@ ("fo:block", NoOpt, attr: _*) << content << "</fo:block>"

  def blockWithWS (opt: Options, content: Seq[Span], attr: (String,String)*) = 
    this <<@ ("fo:block", NoOpt, attr: _*) <<< content << "</fo:block>"
  
  def inline (opt: Options, content: Seq[Span], attr: (String,String)*) = 
    this <<@ ("fo:inline", NoOpt, attr: _*) << content << "</fo:inline>"
    
  def internalLink (opt: Options, target: String, content: Seq[Span], attr: (String,String)*) = 
    this <<@ ("fo:basic-link", NoOpt, (attr :+ ("internal-destination"->target)): _*) << content << "</fo:basic-link>"
  
  def externalLink (opt: Options, url: String, content: Seq[Span], attr: (String,String)*) = 
    this <<@ ("fo:basic-link", NoOpt, (attr :+ ("external-destination"->url)): _*) << content << "</fo:basic-link>"
    
  def externalGraphic (opt: Options, src: String) =
    this <<@ ("fo:external-graphic", NoOpt, "src"->src,
        "inline-progression-dimension.maximum"->"100%", 
        "content-width"->"scale-down-to-fit")
        
  def listItem (opt: Options, label: Seq[Span], body: Seq[Block], attr: (String,String)*) = {
    val content = List(ListItemLabel(Paragraph(label)), ListItemBody(body))
    this <<@ ("fo:list-item", NoOpt, attr: _*) <<|> content <<| "</fo:list-item>"
  }
   
  def listItemLabel (opt: Options, content: Block, attr: (String,String)*) = {
    this <<@ ("fo:list-item-label", NoOpt, attr :+ ("end-indent"->"label-end()"): _*) <<|> content <<| "</fo:list-item-label>"
  }
  
  def listItemBody (opt: Options, content: Seq[Block], attr: (String,String)*) = {
    this <<@ ("fo:list-item-body", NoOpt, attr :+ ("start-indent"->"body-start()"): _*) <<|> content <<| "</fo:list-item-body>"
  }
  
  def footnote (opt: Options, label: String, body: Element) = {
    this <<@ ("fo:footnote",opt,Nil:_*) <<|> List(Text(label,Styles("footnote-link")),body) <<| "</fo:footnote>"
  }
  
  def text (opt: Options, content: String, attr: (String,String)*) = 
    this <<@ ("fo:inline", NoOpt, attr: _*) <<& content << "</fo:inline>"
    
  def textWithWS (opt: Options, content: String, attr: (String,String)*) = 
    this <<@ ("fo:inline", NoOpt, attr: _*) <<<& content << "</fo:inline>"
    
  def rawText (opt: Options, content: String, attr: (String,String)*) = 
    this <<@ ("fo:inline", NoOpt, attr: _*) << content << "</fo:inline>"
  
 
}

object FOWriter {
  
  case class ListItemLabel (content: Block, options: Options = NoOpt) extends Block
  
  case class ListItemBody (content: Seq[Block], options: Options = NoOpt) extends Block with BlockContainer[ListItemBody]
  
}