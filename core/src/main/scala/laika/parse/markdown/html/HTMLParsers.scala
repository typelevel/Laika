/*
 * Copyright 2013-2016 the original author or authors.
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
import laika.parse.core.{Parser, ~}
   
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
  val htmlWS: Parser[String] = anyOf(htmlWSChars:_*)
  

  val htmlAttributeName: Parser[String] = anyBut(htmlAttrEndChars:_*) min 1
 
  val htmlUnquotedAttributeValue: Parser[(List[Span with TextContainer], Option[Char])] = 
    spans(anyUntil(htmlAttrEndChars:_*).keepDelimiter, Map('&' -> htmlCharReference)) ^?
      { case x :: xs => ((x::xs).asInstanceOf[List[Span with TextContainer]], None) }
  
  /** Parses an attribute value enclosed by the specified character.
   */
  def htmlQuotedAttributeValue (c: Char): Parser[(List[Span with TextContainer], Option[Char])] =
    c ~> spans(anyUntil(c), Map('&' -> htmlCharReference)) ^^
      { spans => (spans.asInstanceOf[List[Span with TextContainer]], Some(c)) }
    
  /** Parses quoted and unquoted attribute values.
   */
  val htmlAttributeValue: Parser[(List[Span with TextContainer], Option[Char])] = 
    htmlQuotedAttributeValue('"') | 
    htmlQuotedAttributeValue('\'') | 
    htmlUnquotedAttributeValue

  /** Parses a single attribute, consisting of the name and (optional) equals sign
   *  and value.
   */
  val htmlAttribute: Parser[HTMLAttribute] = 
    (htmlAttributeName <~ htmlWS) ~ opt('=' ~> htmlAttributeValue <~ htmlWS) ^^ { 
      case name ~ Some((value, quotedWith)) => HTMLAttribute(name, value, quotedWith) 
      case name ~ None                      => HTMLAttribute(name, Nil, None) 
    }
   
  
  val htmlTagName: Parser[String] = (anyIn('a' to 'z', 'A' to 'Z') min 1) ~ anyBut('/' :: '>' :: htmlWSChars:_*) ^^ {
    case first ~ rest => first + rest
  }
  
  /** Parses an HTML tag without the enclosing `'<'` and `'>'` characters.
   */
  val htmlTagContent: Parser[String ~ List[HTMLAttribute]] = htmlTagName ~ (htmlWS ~> (htmlAttribute *) <~ htmlWS)

  /** Parses an HTML end tag without the leading `'<'`.
   */
  val htmlEndTag: Parser[HTMLEndTag] = '/' ~> htmlTagName <~ htmlWS <~ '>' ^^ { HTMLEndTag(_) }
  
  /** Parses an HTML end tag if it matches the specified tag name.
   */
  def htmlEndTag (tagName: String): Parser[String] = "</" ~> tagName <~ htmlWS <~ '>'

  /** Parses an HTML comment without the leading `'<'`.
   */
  val htmlComment: Parser[HTMLComment] = "!--" ~> anyUntil("-->") ^^ { HTMLComment(_) }
  
  /** Parses an empty HTML element without the leading `'<'`.
   *  Only recognizes empty tags explicitly closed.
   */
  val htmlEmptyElement: Parser[HTMLEmptyElement] = htmlTagContent <~ "/>" ^^ { 
    case tagName ~ attributes => HTMLEmptyElement(tagName, attributes) 
  }
  
  /** Parses an HTML start tag without the leading `'<'`.
   *  Only recognizes empty tags explicitly closed.
   */
  val htmlStartTag: Parser[HTMLStartTag] = htmlTagContent <~ '>' ^^ { 
    case tagName ~ attributes => HTMLStartTag(tagName, attributes) 
  }
  
  /** Parses an HTML element without the leading `'<'`, but including 
   *  all the nested HTML and Text elements.
   */
  def htmlElement (nested: Map[Char,Parser[Span]]): Parser[HTMLElement] = htmlStartTag >> { 
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
  
  
  private def mkString (result: ~[Char,String]): String = result._1.toString + result._2
  
  /** Parses a numeric or named character reference without the leading `'&'`.
   */
  def htmlCharReference: Parser[HTMLCharacterReference] = 
    (htmlNumericReference | htmlNamedReference) <~ ';' ^^ 
      { s => HTMLCharacterReference("&" + s + ";") }
  
  /** Parses a numeric character reference (decimal or hexadecimal) without the leading `'&'`.
   */
  def htmlNumericReference: Parser[String] = '#' ~ (htmlHexReference | htmlDecReference) ^^ { mkString }
  
  def htmlHexReference: Parser[String] = {
    val hexNumber = anyIn('0' to '9', 'a' to 'f', 'A' to 'F') min 1

    (char('x') | char('X')) ~ hexNumber ^^ { mkString }
  }
  
  def htmlDecReference: Parser[String] = anyIn('0' to '9') min 1
  
  def htmlNamedReference: Parser[String] = anyIn('0' to '9', 'a' to 'z', 'A' to 'Z') min 1
  
  
  override protected def prepareSpanParsers: Map[Char, Parser[Span]] = super.prepareSpanParsers ++ htmlParsers

  
  /** The mapping of start characters to their corresponding HTML span parsers.
   */
  lazy val htmlParsers: Map[Char, Parser[Span]] = Map(
    '<' -> (simpleLink | htmlSpanWithNestedMarkdown),    
    '&' -> htmlCharReference
  )
  
  
  /**
   * Elements that the HTML specification does not define as "Phrasing Content".
   * These elements can serve as the root of a Block instance in the Document model.
   * For an HTML renderer this means that it can avoid to wrap these blocks
   * inside p tags as it would do with a normal paragraph.   
   */
  val htmlBlockElements: Set[String] = Set("body", "style", "blockquote", "center", "dir", 
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
  def htmlBlockStart: Parser[HTMLStartTag] = '<' ~> htmlStartTag ^? { 
    case t @ HTMLStartTag(name, _, _) if htmlBlockElements.contains(name) => t 
  }

  private lazy val htmlBlockParsers: Map[Char, Parser[Span]] = Map(
    '<' -> htmlSpan,    
    '&' -> htmlCharReference
  )
  
  /** Parses a full HTML block, with the root element being a block-level HTML element
   *  and without parsing any standard Markdown markup.
   */
  def htmlBlock: Parser[HTMLBlock] = htmlBlockStart >> { 
    tag => spans(anyUntil(htmlEndTag(tag.name)), htmlBlockParsers) <~ ws <~ eol ^^ {
      spans => HTMLBlock(HTMLElement(tag, spans))  
    } 
  }
  
  override protected def prepareBlockParsers (nested: Boolean): List[Parser[Block]] = {
    if (nested) super.prepareBlockParsers(nested)
    else htmlBlock :: ('<' ~> (htmlComment | htmlEmptyElement | htmlStartTag) <~ ws ~ eol ~ blankLine) :: super.prepareBlockParsers(nested)
  }
  
  
}
