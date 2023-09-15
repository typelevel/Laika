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

package laika.internal.markdown

import cats.data.NonEmptySet
import laika.api.bundle.{ BlockParserBuilder, SpanParserBuilder }
import laika.ast.*
import laika.ast.html.*
import laika.parse.Parser
import laika.parse.markup.InlineParsers.spans
import laika.parse.markup.RecursiveSpanParsers
import laika.parse.text.{ CharGroup, DelimitedText, PrefixedParser }
import laika.parse.builders.*
import laika.parse.implicits.*

/** Parses verbatim HTML elements which may interleave with standard Markdown markup.
  * Extends the Markdown block and inline parsers,
  * overriding several of their parsers to add the HTML functionality.
  *
  * @author Jens Halm
  */
private[laika] object HTMLParsers {

  private val htmlWSChars = NonEmptySet.of(' ', '\t', '\f', '\n', '\r')

  private val htmlAttrEndChars = NonEmptySet.of('"', '\'', '<', '=', '/', '>') ++ htmlWSChars

  /** Parses and consumes optional whitespace, always succeeds.
    */
  private val htmlWS: Parser[String] = anyOf(htmlWSChars)

  private val htmlHexReference: Parser[String] = {
    val hexNumber = someOf(CharGroup.hexDigit)

    ((oneOf('x') | oneOf('X')) ~ hexNumber).source
  }

  private val htmlDecReference: Parser[String] = someOf(CharGroup.digit)

  private val htmlNamedReference: Parser[String] = someOf(CharGroup.alphaNum)

  /** Parses a numeric character reference (decimal or hexadecimal) without the leading `'&'`.
    */
  private val htmlNumericReference: Parser[String] =
    ("#" ~ (htmlHexReference | htmlDecReference)).source

  /** Parses a numeric or named character reference without the leading `'&'`.
    */
  private val htmlCharReference: PrefixedParser[HTMLCharacterReference] =
    ("&" ~> (htmlNumericReference | htmlNamedReference) <~ ";").source.map(
      HTMLCharacterReference(_)
    )

  private val htmlAttributeName: Parser[String] = someNot(htmlAttrEndChars)

  private def asTextContainers(spans: List[Span]): List[TextContainer] = spans.collect {
    case tc: TextContainer => tc
  }

  private val htmlUnquotedAttributeValue: Parser[(List[TextContainer], Option[Char])] =
    spans(delimitedBy(htmlAttrEndChars).keepDelimiter).embed(htmlCharReference)
      .map { spans => (asTextContainers(spans), None) }

  /** Parses an attribute value enclosed by the specified character.
    */
  private def htmlQuotedAttributeValue(c: String): Parser[(List[TextContainer], Option[Char])] =
    c ~> spans(delimitedBy(c)).embed(htmlCharReference)
      .map { spans => (asTextContainers(spans), Some(c.head)) }

  /** Parses quoted and unquoted attribute values.
    */
  private val htmlAttributeValue: Parser[(List[TextContainer], Option[Char])] =
    htmlQuotedAttributeValue("\"") |
      htmlQuotedAttributeValue("'") |
      htmlUnquotedAttributeValue

  /** Parses a single attribute, consisting of the name and (optional) equals sign
    *  and value.
    */
  private val htmlAttribute: Parser[HTMLAttribute] =
    (htmlAttributeName <~ htmlWS) ~ opt("=" ~> htmlAttributeValue <~ htmlWS) ^^ {
      case name ~ Some((value, quotedWith)) => HTMLAttribute(name, value, quotedWith)
      case name ~ None                      => HTMLAttribute(name, Nil, None)
    }

  private val htmlTagName: Parser[String] =
    (someOf(CharGroup.alpha) ~ anyNot(htmlWSChars.add('/').add('>'))).source

  /** Parses an HTML tag without the enclosing `'<'` and `'>'` characters.
    */
  private val htmlTagContent: Parser[String ~ List[HTMLAttribute]] =
    htmlTagName ~ (htmlWS ~> htmlAttribute.rep <~ htmlWS)

  /** Parses an HTML end tag without the leading `'<'`.
    */
  private val htmlEndTag: Parser[HTMLEndTag] = "/" ~> htmlTagName <~ htmlWS <~ ">" ^^ {
    HTMLEndTag(_)
  }

  /** Parses an HTML end tag if it matches the specified tag name.
    */
  private def htmlEndTag(tagName: String): DelimitedText = delimitedBy(
    ("</" ~ tagName ~ htmlWS ~ ">").as("")
  )

  /** Parses an HTML comment without the leading `'<'`.
    */
  private val htmlComment: Parser[HTMLComment] = "!--" ~> delimitedBy("-->").map(HTMLComment(_))

  /** Parses an HTML comment without the leading `'<'`.
    */
  private val htmlScriptElement: Parser[HTMLScriptElement] =
    (("script" ~> (htmlWS ~> htmlAttribute.rep <~ htmlWS) <~ ">") ~ delimitedBy("</script>"))
      .mapN(html.HTMLScriptElement(_, _))

  /** Parses an empty HTML element without the leading `'<'`.
    *  Only recognizes empty tags explicitly closed.
    */
  private val htmlEmptyElement: Parser[HTMLEmptyElement] =
    (htmlTagContent <~ "/>").mapN(html.HTMLEmptyElement(_, _))

  /** Parses an HTML start tag without the leading `'<'`.
    *  Only recognizes empty tags explicitly closed.
    */
  private val htmlStartTag: Parser[HTMLStartTag] =
    (htmlTagContent <~ ">").mapN(html.HTMLStartTag(_, _))

  /** Parses an HTML element without the leading `'<'`, but including
    *  all the nested HTML and Text elements.
    */
  private lazy val htmlElement: Parser[HTMLElement] = htmlStartTag >> { tag =>
    spans(htmlEndTag(tag.name)).embedAll(htmlBlockParsers).map { spans =>
      html.HTMLElement(tag, spans)
    }
  }

  /** Parses an HTML element without the leading `'<'`, but including
    * all the nested HTML and Text elements, as well as any nested Markdown spans.
    */
  private def htmlElementWithNestedMarkdown(recParsers: RecursiveSpanParsers): Parser[HTMLElement] =
    htmlStartTag >> { tag =>
      recParsers.recursiveSpans(htmlEndTag(tag.name)).map { spans =>
        html.HTMLElement(tag, spans)
      }
    }

  /** Parses any of the HTML span elements supported by this trait, plus standard markdown inside HTML elements.
    */
  val htmlSpan: SpanParserBuilder = SpanParserBuilder.recursive { recParsers =>
    "<" ~> (htmlComment | htmlEmptyElement | htmlElementWithNestedMarkdown(
      recParsers
    ) | htmlEndTag | htmlStartTag)
  }

  /** Parses a numeric or named character reference.
    */
  val htmlCharRef: SpanParserBuilder = SpanParserBuilder.standalone(htmlCharReference)

  /** Parses any of the HTML span elements supported by this trait, but no standard markdown inside HTML elements.
    */
  private lazy val htmlSpanInsideBlock: PrefixedParser[HTMLSpan] =
    "<" ~> (htmlComment | htmlScriptElement | htmlEmptyElement | htmlElement | htmlEndTag | htmlStartTag)

  /** Elements that the HTML specification does not define as "Phrasing Content".
    * These elements can serve as the root of a Block instance in the Document model.
    * For an HTML renderer this means that it can avoid to wrap these blocks
    * inside p tags as it would do with a normal paragraph.
    */
  private val htmlBlockElements: Set[String] = Set(
    "body",
    "style",
    "blockquote",
    "center",
    "dir",
    "dl",
    "dd",
    "dt",
    "fieldset",
    "form",
    "p",
    "pre",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "ol",
    "ul",
    "li",
    "div",
    "hr",
    "isindex",
    "menu",
    "frameset",
    "noframes",
    "table",
    "thead",
    "tbody",
    "tfoot",
    "th",
    "tr",
    "td",
    "article",
    "aside",
    "section",
    "address",
    "details",
    "header",
    "footer",
    "hgroup",
    "figure",
    "figcaption",
    "menu",
    "nav",
    "summary"
  )

  /** Parses the start tag of an HTML block, only matches when the tag name is an
    *  actual block-level HTML tag.
    */
  private val htmlBlockStart: Parser[HTMLStartTag] = ("<" ~> htmlStartTag).collect {
    case t @ HTMLStartTag(name, _, _) if htmlBlockElements.contains(name) => t
  }

  private lazy val htmlBlockParsers: Seq[PrefixedParser[Span]] =
    Seq(htmlSpanInsideBlock, htmlCharReference)

  /** Parses a full HTML block, with the root element being a block-level HTML element
    *  and without parsing any standard Markdown markup.
    */
  private lazy val htmlBlock: Parser[HTMLBlock] = htmlBlockStart >> { tag =>
    spans(htmlEndTag(tag.name)).embedAll(htmlBlockParsers) <~ wsEol ^^ { spans =>
      HTMLBlock(html.HTMLElement(tag, spans))
    }
  }

  private lazy val htmlBlockElement: Parser[Block] =
    "<" ~> (htmlComment | htmlEmptyElement | htmlStartTag) <~ wsEol ~ blankLine

  lazy val htmlBlockFragment: BlockParserBuilder =
    BlockParserBuilder.standalone(htmlBlock | htmlBlockElement).rootOnly // TODO - keep separate

}
