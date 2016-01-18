package laika.parse.markdown.html

import laika.parse.markdown.html.HTMLElements._
import laika.tree.Elements.{Span, TextContainer}

trait HTMLModelBuilder {

  
  def toAttributes (attributes: (String,Span with TextContainer)*): List[HTMLAttribute] = 
    attributes.toList map (a => HTMLAttribute(a._1, List(a._2), Some('\"')))
   
  def emptyTag (name: String, attributes: (String,Span with TextContainer)*): HTMLEmptyElement = 
    HTMLEmptyElement(name, toAttributes(attributes:_*))

  def startTag (name: String, attributes: (String,Span with TextContainer)*): HTMLStartTag = 
    HTMLStartTag(name, toAttributes(attributes:_*))
  
  def startTag (name: String, attribute: HTMLAttribute): HTMLStartTag = 
    HTMLStartTag(name, List(attribute))
  
  def endTag (name: String): HTMLEndTag = HTMLEndTag(name)
  
  def element (name: String, content: Span*): HTMLElement = 
    HTMLElement(HTMLStartTag(name, Nil), content.toList)
  
  def element (startTag: HTMLStartTag, content: Span*): HTMLElement = 
    HTMLElement(startTag, content.toList)
  
  def charRef (str: String) = HTMLCharacterReference(str)
  
  def comment (str: String) = HTMLComment(str)
  
  
}
