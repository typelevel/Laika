package laika.parse.markdown.html

import laika.parse.markdown.html.VerbatimHTMLElements._
import laika.tree.Elements.{Span, TextContainer}

trait HTMLModelBuilder {

  
  def toAttributes (attributes: (String,Span with TextContainer)*) = 
    attributes.toList map (a => HTMLAttribute(a._1, List(a._2), Some('\"')))
   
  def emptyTag (name: String, attributes: (String,Span with TextContainer)*) = HTMLEmptyElement(name, toAttributes(attributes:_*))

  def startTag (name: String, attributes: (String,Span with TextContainer)*) = HTMLStartTag(name, toAttributes(attributes:_*))
  
  def startTag (name: String, attribute: HTMLAttribute) = HTMLStartTag(name, List(attribute))
  
  def endTag (name: String) = HTMLEndTag(name)
  
  def element (name: String, content: Span*) = HTMLElement(HTMLStartTag(name, Nil), content.toList)
  
  def element (startTag: HTMLStartTag, content: Span*) = HTMLElement(startTag, content.toList)
  
  def charRef (str: String) = HTMLCharacterReference(str)
  
  def comment (str: String) = HTMLComment(str)
  
  
}