/*
 * Copyright 2013 the original author or authors.
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
import laika.tree._
import laika.io.Output
 
/** A renderer for PrettyPrint output, primarily useful for debugging purposes. 
 *  May be directly passed to the `Render` or `Transform` APIs:
 * 
 *  {{{
 *  Render as PrettyPrint from document toString
 *  
 *  Transform from Markdown to PrettyPrint fromFile "hello.md" toConsole
 *  }}}
 * 
 *  @author Jens Halm
 */
class PrettyPrint extends ((Output, Element => Unit) => (TextWriter, Element => Unit)) {

  
  /** The maximum width of a single text element.
   *  For any text that exceeds this limit only the beginnig
   *  and end of the line will be displayed up to the maximum
   *  number of characters allowed. This increases readability
   *  for the majority of cases where primarily the document
   *  structure is relevant.
   */
  val maxTextWidth = 50
  
  
  /** The actual setup method for providing both the writer API for customized
   *  renderers as well as the actual default render function itself. The default render
   *  function always only renders a single element and then delegates to the composite
   *  renderer passed to this function as a parameter when rendering children. This way
   *  user customizations are possible on a per-element basis.
   *  
   *  @param output the output to write to
   *  @param render the composite render function to delegate to when elements need to render their children
   *  @return a tuple consisting of the writer API for customizing
   *  the renderer as well as the actual default render function itself
   */
  def apply (output: Output, render: Element => Unit) = {
    val out = new TextWriter(output asFunction, render, ". ") 
    (out, renderElement(out))
  }
  
  private case class Content (content: Seq[Element], desc: String) extends Element with ElementContainer[Element,Content]
  
  private def renderElement (out: TextWriter)(elem: Element): Unit = {
    
    def attributes (attr: Iterator[Any], content: AnyRef) = {
      val it = attr.asInstanceOf[Iterator[AnyRef]]
      val res = it filter (_ ne content) mkString ("(", ",", ")")
      if (res == "()") "" else res
    } 
    
    def elementContainerDesc (con: ElementContainer[Element,_], elementType: String) = {
      val (elements, rest) = con.productIterator partition (_.isInstanceOf[Element])
      out << con.productPrefix << attributes(rest, con.content)
      
      val contentDesc = " - " + elementType + ": " + con.content.length.toString
      if (!elements.isEmpty) out <<|> (elements.toList.asInstanceOf[Seq[Element]] ++ List(Content(con.content, "Content" + contentDesc)))
      else out << contentDesc <<|> con.content  
    }
    
    def textContainerDesc (con: TextContainer) = {
      out << con.productPrefix << attributes(con.productIterator, con.content) << " - '"
      
      val text = con.content.replaceAllLiterally("\n", "|")
      val len = text.length
      
      if (len <= maxTextWidth) out << text << "'"
      else out << text.substring(0, maxTextWidth / 2) << " [...] " << text.substring(len - maxTextWidth / 2) << "'"
    }
    
    elem match {
      case bc: BlockContainer[_]  => elementContainerDesc(bc, "Blocks")
      case sc: SpanContainer[_]   => elementContainerDesc(sc, "Spans")
      case tc: TextContainer      => textContainerDesc(tc)
      case Content(content,desc)  => out << desc <<|> content
      case e                      => out << elem.toString
    }
  }
    

}

object PrettyPrint extends PrettyPrint
