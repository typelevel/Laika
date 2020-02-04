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

package laika.markdown

import laika.ast._
import laika.bundle.{SpanParser, SpanParserBuilder}
import laika.parse.Parser
import laika.parse.markup.InlineParsers.text
import laika.parse.markup.RecursiveSpanParsers
import laika.parse.text.TextParsers._
import laika.parse.text.{DelimitedText, PrefixedParser}

import scala.util.Try

/** Provides all inline parsers for Markdown text except for those dealing
 *  with verbatim HTML markup which this library treats as an optional 
 *  feature that has to be explicitly mixed in.
 *  
 *  Inline parsers deal with markup within a block of text, such as a
 *  link or emphasized text. They are used in the second phase of parsing,
 *  after the block parsers have cut the document into a (potentially nested)
 *  block structure.
 * 
 *  @author Jens Halm
 */
object InlineParsers {


  /**  Parses a single escaped character, only recognizing the characters the Markdown syntax document
    *  specifies as escapable.
    *  The `|` has been added to that list to support escaping in tables in the GitHub Flavor syntax.
    *
    *  Note: escaping > is not mandated by the official syntax description, but by the official test suite.
    */
  val escapedChar: Parser[String] = oneOf('\\', '`', '*', '_', '{', '}', '[', ']', '(', ')', '#', '+', '-', '.', '!', '>', '|')


  /** Parses an explicit hard line break.
   */
  val lineBreak: SpanParserBuilder = SpanParser.standalone("\\\r".as(LineBreak()))
  
  /** Parses a span of strong text enclosed by two consecutive occurrences of the specified character. 
   */
  def strong (char: Char)(implicit recParsers: RecursiveSpanParsers): PrefixedParser[Strong] = enclosedByDoubleChar(char) ^^ { Strong(_) }
  
  /** Parses a span of emphasized text enclosed by one occurrence of the specified character.
   */
  def em (char: Char)(implicit recParsers: RecursiveSpanParsers): PrefixedParser[Emphasized] = enclosedBySingleChar(char) ^^ { Emphasized(_) }

  
  /** Creates a parser for an inline span based on the specified parsers that
   *  represent the start and end condition.
   * 
   *  @param start the parser that parses the beginning of the span, result will be discarded
   *  @param end the end delimiter of the span
   */
  def span (start: PrefixedParser[Any], end: PrefixedParser[String])(implicit recParsers: RecursiveSpanParsers): PrefixedParser[List[Span]]
  = start ~> recParsers.recursiveSpans(delimitedBy(end))

  /** Parses either strong spans enclosed in double asterisks or emphasized spans enclosed in single asterisks.
    */
  val enclosedByAsterisk: SpanParserBuilder = SpanParser.recursive { implicit recParsers =>
    strong('*') | em('*')
  }

  /** Parses either strong spans enclosed in double underscores or emphasized spans enclosed in single underscores.
    */
  val enclosedByUnderscore: SpanParserBuilder = SpanParser.recursive { implicit recParsers =>
    strong('_') | em('_')
  }

  /** Parses a span enclosed by a single occurrence of the specified character.
   *  Recursively parses nested spans, too. 
   */
  def enclosedBySingleChar (c: Char)(implicit recParsers: RecursiveSpanParsers): PrefixedParser[List[Span]] = {
    val start = delimiter(c).nextNot(' ', '\n', c)
    val end   = delimiter(c).prevNot(' ')
    span(start, end)
  }
  
  /** Parses a span enclosed by two consecutive occurrences of the specified character.
   *  Recursively parses nested spans, too. 
   */
  def enclosedByDoubleChar (c: Char)(implicit recParsers: RecursiveSpanParsers): PrefixedParser[List[Span]] = {
    val start = delimiter(s"$c$c").nextNot(' ', '\n')
    val end   = delimiter(s"$c$c").prevNot(' ')
    span(start, end)
  }
  
  /** Parses a literal span enclosed by a single backtick.
   *  Does neither parse nested spans nor Markdown escapes. 
   */
  val literalEnclosedBySingleChar: PrefixedParser[Literal] = {
    val start = delimiter('`').nextNot('`')
    val end = '`'
    start ~> delimitedBy(end) ^^ { s => Literal(s.trim) }
  }
  
  /** Parses a literal span enclosed by double backticks.
   *  Does neither parse nested spans nor Markdown escapes. 
   */
  val literalEnclosedByDoubleChar: PrefixedParser[Literal] = {
    val delim = "``"
    delim ~> delimitedBy(delim) ^^ { s => Literal(s.trim) }
  }

  /** Parses a literal span enclosed by double or single backticks.
    */
  val literalSpan: SpanParserBuilder = SpanParser.standalone(literalEnclosedByDoubleChar | literalEnclosedBySingleChar)
  
  
  private def normalizeId (id: String): String = id.toLowerCase.replaceAll("[\n ]+", " ")

  type RecParser = String => List[Span]

  /** Parses a link, including nested spans in the link text.
    *  Recognizes both, an inline link `[text](url)` and a link reference `[text][id]`.
    */
  lazy val link: SpanParserBuilder = SpanParser.recursive { recParsers =>

    def unwrap (ref: LinkReference, suffix: String) = {
      if ((ref select (_.isInstanceOf[LinkReference])).tail.nonEmpty)
        SpanSequence(Text("[") :: ref.content.toList ::: Text(suffix) :: Nil)
      else ref
    }

    def linkInline (p: RecParser, text: String, url: String, title: Option[String]) = ExternalLink(p(text), url, title)
    def linkReference (p: RecParser, text: String, id: String, suffix: String): Span = {
      /* Markdown's design comes with a few arbitrary and inconsistent choices for how to handle nesting of brackets.
       * The logic here is constructed to make the official test suite pass, other edge cases might still yield unexpected results.
       * Users usually should not bother and simply escape brackets which are not meant to be markup. */
      val ref = LinkReference(p(text), normalizeId(id), "[" + text + suffix)
      if (text == id) unwrap(ref, suffix) else ref
    }

    "[" ~> resource(linkInline, linkReference, recParsers)
  }

  /** Parses an inline image.
    *  Recognizes both, an inline image `![text](url)` and an image reference `![text][id]`.
    */
  val image: SpanParserBuilder = SpanParser.recursive { recParsers =>

    def escape (text: String, f: String => Span): Span = 
      recParsers.escapedText(DelimitedText.Undelimited).parse(text).toEither.fold(InvalidElement(_, text).asSpan, f)

    def imageInline (p: RecParser, text: String, uri: String, title: Option[String]) =
      escape(text, Image(_, URI(uri), title = title))

    def imageReference (p: RecParser, text: String, id: String, postFix: String): Span =
      escape(text, ImageReference(_, normalizeId(id), "![" + text + postFix))

    "![" ~> resource(imageInline, imageReference, recParsers)
  }
  
  /** Helper function that abstracts the common parser logic of links and images.
   * 
   *  @param inline factory function for creating a new inline link or image based on the text, url and optional title parameters
   *  @param ref factory function for creating a new link or image reference based on the text and id parameters
   */
  def resource (inline: (RecParser, String, String, Option[String]) => Span,
                ref: (RecParser, String, String, String) => Span,
                recParsers: RecursiveSpanParsers): Parser[Span] = {

    val linkText = text(delimitedBy(']'))
      .embed(recParsers.escapeSequence ^^ {"\\" + _})
      .embed('[' ~> delimitedBy(']') ^^ { "[" + _ + "]" })

    val titleEnd = ws.void ~ ')'
    def enclosedIn(c: Char): Parser[String] = c ~> delimitedBy(c.toString <~ lookAhead(titleEnd))
    val title = ws.void ~> (enclosedIn('"') | enclosedIn('\''))

    val url = ('<' ~> text(delimitedBy('>').failOn(' ')).embed(recParsers.escapeSequence)) |
       text(delimitedBy(')',' ','\t').keepDelimiter).embed(recParsers.escapeSequence)
    
    val urlWithTitle = '(' ~> url ~ opt(title) <~ ws ~ ')' ^^ {  
      case url ~ title => (recParser: RecParser, text:String) => inline(recParser, text, url, title)
    }
    val refId =    ws ~ opt(eol) ~ ('[' ~> recParsers.escapedUntil(']')) ^^ {
      case ws ~ lb ~ id => (recParser: RecParser, text:String) =>
        ref(recParser, text, id,   "]"+ws+ lb.getOrElse("") +"["+id+"]") }

    val refEmpty = ws ~ opt(eol) ~ "[]" ^^ {
      case ws ~ lb ~ _  => (recParser: RecParser, text:String) =>
        ref(recParser, text, text, "]"+ws+ lb.getOrElse("") +"[]") }

    recParsers.withRecursiveSpanParser(linkText) ~ opt(urlWithTitle | refEmpty | refId) ^^ {
      case (recParser, text) ~ None    => ref(recParser, text, text, "]")
      case (recParser, text) ~ Some(f) => f(recParser, text)
    }
  }

  /** Parses a simple inline link in the form of &lt;http://someURL/&gt;
    */
  val simpleLink: SpanParserBuilder = SpanParser.standalone {

    def isAcceptedScheme (s: String) = s == "http" || s == "https" || s == "ftp" || s == "mailto"
    
    def isURI (s: String) = Try { 
      val uri = new java.net.URI(s)
      uri.isAbsolute && isAcceptedScheme(uri.getScheme) 
    }.toOption.getOrElse(false)
    
    def isEmail (s: String) = s.contains("@") && isURI(s"mailto:$s")

    def toLink(s: String) = ExternalLink(List(Text(s)), s)

    ('<' ~> anyNot(' ','\r','\n','\t','>') <~ '>').collect {
      case s if isURI(s) => toLink(s)
      case s if isEmail(s) => toLink(s"mailto:$s")
    }
    
  }

}
