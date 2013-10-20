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

package laika.parse.markdown.html

import laika.tree.Elements._
import laika.parse.markdown.InlineParsers
import laika.parse.markdown.BlockParsers
import HTMLElements._
   
/** Parses verbatim HTML elements which may interleave with standard Markdown markup.
 *  Extends the Markdown block and inline parsers, overriding several of their
 *  parsers to add the HTML functionality.
 * 
 *  @author Jens Halm
 */
trait HTMLParsers extends InlineParsers with BlockParsers { 

  

  private val htmlWSChars = List(' ','\t','\f','\n','\r')

  private val htmlAttrEndChars = List('"','\'','<','=','/','>') ::: htmlWSChars
  
  
  /** Parses and consumes optional whitespace, always succeeds.
   */
  val htmlWS = anyOf(htmlWSChars:_*)
  

  val htmlAttributeName = anyBut(htmlAttrEndChars:_*) min 1
 
  val htmlUnquotedAttributeValue = 
    spans(anyBut(htmlAttrEndChars:_*), Map('&' -> htmlCharReference)) ^?  
      { case x :: xs => ((x::xs).asInstanceOf[List[Span with TextContainer]], None) }
  
  /** Parses an attribute value enclosed by the specified character.
   */
  def htmlQuotedAttributeValue (char: Char) = 
    char ~> spans(anyBut(char), Map('&' -> htmlCharReference)) <~ char ^^ 
      { spans => (spans.asInstanceOf[List[Span with TextContainer]], Some(char)) }
    
  /** Parses quoted and unquoted attribute values.
   */
  val htmlAttributeValue = htmlQuotedAttributeValue('"') | 
                           htmlQuotedAttributeValue('\'') | 
                           htmlUnquotedAttributeValue

  /** Parses a single attribute, consisting of the name and (optional) equals sign
   *  and value.
   */
  val htmlAttribute = 
    (htmlAttributeName <~ htmlWS) ~ opt('=' ~> htmlAttributeValue <~ htmlWS) ^^ { 
      case name ~ Some((value, quotedWith)) => HTMLAttribute(name, value, quotedWith) 
      case name ~ None                      => HTMLAttribute(name, Nil, None) 
    }
   
  
  val htmlTagName = (anyIn('a' to 'z', 'A' to 'Z') min 1) ~ anyBut(('/'::'>'::htmlWSChars):_*) ^^ { 
    case first ~ rest => first + rest
  }
  
  /** Parses an HTML tag without the enclosing `'<'` and `'>'` characters.
   */
  val htmlTagContent = htmlTagName ~ (htmlWS ~> (htmlAttribute *) <~ htmlWS)

  /** Parses an HTML end tag without the leading `'<'`.
   */
  val htmlEndTag = '/' ~> htmlTagName <~ htmlWS <~ '>' ^^ { HTMLEndTag(_) }
  
  /** Parses an HTML end tag if it matches the specified tag name.
   */
  def htmlEndTag (tagName: String) = "</" ~> tagName <~ htmlWS <~ '>'

  /** Parses an HTML comment without the leading `'<'`.
   */
  val htmlComment = "!--" ~> anyUntil("-->") ^^ { HTMLComment(_) }
  
  /** Parses an empty HTML element without the leading `'<'`.
   *  Only recognizes empty tags explicitly closed.
   */
  val htmlEmptyElement = htmlTagContent <~ "/>" ^^ { 
    case tagName ~ attributes => HTMLEmptyElement(tagName, attributes) 
  }
  
  /** Parses an HTML start tag without the leading `'<'`.
   *  Only recognizes empty tags explicitly closed.
   */
  val htmlStartTag = htmlTagContent <~ '>' ^^ { 
    case tagName ~ attributes => HTMLStartTag(tagName, attributes) 
  }
  
  /** Parses an HTML element without the leading `'<'`, but including 
   *  all the nested HTML and Text elements.
   */
  def htmlElement (nested: Map[Char,Parser[Span]]) = htmlStartTag >> { 
    tag => spans(anyUntil(htmlEndTag(tag.name)), nested) ^^ { 
      spans => HTMLElement(tag, spans) 
    }
  }
  
  
  /** Parses any of the HTML span elements supported by this trait, plus standard markdown inside HTML elements.
   */
  lazy val htmlSpanWithNestedMarkdown: Parser[HTMLSpan] = htmlComment | htmlEmptyElement | htmlElement(spanParsers) | htmlEndTag | htmlStartTag
  
  /** Parses any of the HTML span elements supported by this trait, but no standard markdown inside HTML elements.
   */
  lazy val htmlSpan: Parser[HTMLSpan] = htmlComment | htmlEmptyElement | htmlElement(htmlBlockParsers) | htmlEndTag | htmlStartTag
  
  
  private def mkString (result: ~[Char,String]) = result._1.toString + result._2
  
  /** Parses a numeric or named character reference without the leading `'&'`.
   */
  def htmlCharReference = 
    (htmlNumericReference | htmlNamedReference) <~ ';' ^^ 
      { s => HTMLCharacterReference("&" + s + ";") }
  
  /** Parses a numeric character reference (decimal or hexadecimal) without the leading `'&'`.
   */
  def htmlNumericReference = '#' ~ (htmlHexReference | htmlDecReference) ^^ { mkString(_) }
  
  def htmlHexReference = {
    val hexNumber = anyIn('0' to '9', 'a' to 'f', 'A' to 'F') min 1

    (elem('x') | elem('X')) ~ hexNumber ^^ { mkString(_) }
  }
  
  def htmlDecReference = anyIn('0' to '9') min 1
  
  def htmlNamedReference = anyIn('0' to '9', 'a' to 'z', 'A' to 'Z') min 1
  
  
  override protected def newSpanParserMap = super.newSpanParserMap ++ htmlParsers

  
  /** The mapping of start characters to their corresponding HTML span parsers.
   */
  lazy val htmlParsers = Map(
    '<' -> (simpleLink | htmlSpanWithNestedMarkdown),    
    '&' -> htmlCharReference
  )
  
  
  
  /**
   * Elements that the HTML specification does not define as "Phrasing Content".
   * These elements can serve as the root of a Block instance in the Document model.
   * For an HTML renderer this means that it can avoid to wrap these blocks
   * inside p tags as it would do with a normal paragraph.   
   */
  val htmlBlockElements = Set("body", "style", "blockquote", "center", "dir", 
                       "dl", "dd", "dt", "fieldset", "form", "p", "pre", 
                       "h1", "h2", "h3", "h4", "h5", "h6", "ol", "ul", "li", 
                       "div", "hr", "isindex", "menu", "frameset", "noframes", 
                       "table", "thead", "tbody", "tfoot", "th", "tr", "td", 
                       "article", "aside", "section", "address", "details", 
                       "header", "footer", "hgroup", "figure", "figcaption", 
                       "menu", "nav", "summary")
  
  /** Parses the start tag of an HTML block, only matches when the tag name is an
   *  actual block-level HTML tag.
   */
  def htmlBlockStart = '<' ~> htmlStartTag ^? { case t @ HTMLStartTag(name, _, _) if htmlBlockElements.contains(name) => t }

  private lazy val htmlBlockParsers = Map(
    '<' -> htmlSpan,    
    '&' -> htmlCharReference
  )
  
  /** Parses a full HTML block, with the root element being a block-level HTML element
   *  and without parsing any standard Markdown markup.
   */
  def htmlBlock = htmlBlockStart >> { 
    tag => spans(anyUntil(htmlEndTag(tag.name)), htmlBlockParsers) <~ ws <~ eol ^^ {
      spans => HTMLBlock(HTMLElement(tag, spans))  
    } 
  }
  
  override protected def prepareBlockParsers (parsers: List[Parser[Block]], nested: Boolean) = {
    if (nested) super.prepareBlockParsers(parsers, nested)
    else htmlBlock :: ('<' ~> (htmlComment | htmlEmptyElement | htmlStartTag) <~ ws ~ eol ~ blankLine) :: super.prepareBlockParsers(parsers, nested)
  }
  
  
}