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

package laika.markdown

import laika.ast._
import laika.bundle.{SpanParser, SpanParserBuilder}
import laika.parse.{Parser, ParserContext}
import laika.parse.markup.InlineParsers.text
import laika.parse.markup.RecursiveSpanParsers
import laika.parse.text.TextParsers._
import laika.parse.text.DelimitedText

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
    *
    *  Note: escaping > is not mandated by the official syntax description, but by the official test suite.
    */
  val escapedChar: Parser[String] = anyOf('\\', '`', '*', '_', '{', '}', '[', ']', '(', ')', '#', '+', '-', '.', '!', '>') take 1


  /** Parses an explicit hard line break.
   */
  val lineBreak: SpanParserBuilder = SpanParser.forStartChar('\\').standalone(anyOf('\r').take(1) ^^^ LineBreak())
  
  /** Parses a span of strong text enclosed by two consecutive occurrences of the specified character. 
   */
  def strong (char: Char)(implicit recParsers: RecursiveSpanParsers): Parser[Strong] = enclosedByDoubleChar(char) ^^ { Strong(_) }
  
  /** Parses a span of emphasized text enclosed by one occurrence of the specified character.
   */
  def em (char: Char)(implicit recParsers: RecursiveSpanParsers): Parser[Emphasized] = enclosedBySingleChar(char) ^^ { Emphasized(_) }

  
  /** Creates a parser for an inline span based on the specified parsers that
   *  represent the start and end condition.
   * 
   *  @param start the parser that parses the beginning of the span, result will be discarded
   *  @param endDelim the end delimiter of the span
   *  @param postCondition the parser that checks any post conditions after the end delimiter has been read
   */
  def span (start: Parser[Any], endDelim: String, postCondition: Parser[Any])(implicit recParsers: RecursiveSpanParsers): Parser[List[Span]]
    = start ~> recParsers.delimitedRecursiveSpans(delimitedBy(endDelim, postCondition))

  /** Parses either strong spans enclosed in double asterisks or emphasized spans enclosed in single asterisks.
    */
  val enclosedByAsterisk: SpanParserBuilder = SpanParser.forStartChar('*').recursive { implicit recParsers =>
    strong('*') | em('*')
  }

  /** Parses either strong spans enclosed in double underscores or emphasized spans enclosed in single underscores.
    */
  val enclosedByUnderscore: SpanParserBuilder = SpanParser.forStartChar('_').recursive { implicit recParsers =>
    strong('_') | em('_')
  }

  /** Parses a span enclosed by a single occurrence of the specified character.
   *  Recursively parses nested spans, too. 
   */
  def enclosedBySingleChar (c: Char)(implicit recParsers: RecursiveSpanParsers): Parser[List[Span]] = {
    val start = lookAhead(anyBut(' ', c).take(1).^)
    val end = not(lookBehind(2, ' '))
    span(start, c.toString, end)
  }
  
  /** Parses a span enclosed by two consecutive occurrences of the specified character.
   *  Recursively parses nested spans, too. 
   */
  def enclosedByDoubleChar (c: Char)(implicit recParsers: RecursiveSpanParsers): Parser[List[Span]] = {
    val start = c ~ not(' ')
    val end = c <~ not(lookBehind(3, ' '))
    span(start, c.toString, end)
  }
  
  /** Parses a literal span enclosed by a single backtick.
   *  Does neither parse nested spans nor Markdown escapes. 
   */
  val literalEnclosedBySingleChar: Parser[Literal] = {
    val start = not('`')
    val end = '`'
    start ~> delimitedBy(end) ^^ { s => Literal(s.trim) }
  }
  
  /** Parses a literal span enclosed by double backticks.
   *  Does neither parse nested spans nor Markdown escapes. 
   */
  val literalEnclosedByDoubleChar: Parser[Literal] = {
    val start = '`'
    val end = "``"
    start ~> delimitedBy(end) ^^ { s => Literal(s.trim) }
  }

  /** Parses a literal span enclosed by double or single backticks.
    */
  val literalSpan: SpanParserBuilder = SpanParser.forStartChar('`')
    .standalone(literalEnclosedByDoubleChar | literalEnclosedBySingleChar)
  
  
  private def normalizeId (id: String): String = id.toLowerCase.replaceAll("[\n ]+", " ")

  type RecParser = (String => List[Span])

  /** Parses a link, including nested spans in the link text.
    *  Recognizes both, an inline link `[text](url)` and a link reference `[text][id]`.
    */
  lazy val link: SpanParserBuilder = SpanParser.forStartChar('[').recursive { recParsers =>

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

    resource(linkInline, linkReference, recParsers)
  }

  /** Parses an inline image.
    *  Recognizes both, an inline image `![text](url)` and an image reference `![text][id]`.
    */
  val image: SpanParserBuilder = SpanParser.forStartChar('!').recursive { recParsers =>

    val escape = unsafeParserFunction(recParsers.escapedText(DelimitedText.Undelimited))

    def imageInline (p: RecParser, text: String, uri: String, title: Option[String]) =
      Image(escape(ParserContext(text)), URI(uri), title = title)

    def imageReference (p: RecParser, text: String, id: String, postFix: String): Span =
      ImageReference(escape(ParserContext(text)), normalizeId(id), "![" + text + postFix)

    '[' ~> resource(imageInline, imageReference, recParsers)
  }
  
  /** Helper function that abstracts the common parser logic of links and images.
   * 
   *  @param inline factory function for creating a new inline link or image based on the text, url and optional title parameters
   *  @param ref factory function for creating a new link or image reference based on the text and id parameters
   */
  def resource (inline: (RecParser, String, String, Option[String]) => Span,
                ref: (RecParser, String, String, String) => Span,
                recParsers: RecursiveSpanParsers): Parser[Span] = {

    val linktext = text(delimitedBy(']'), Map(
      '\\' -> (recParsers.escapedChar ^^ {"\\" + _}),
      '[' -> (delimitedBy(']') ^^ { "[" + _ + "]" })
    ))

    val titleEnd = lookAhead(ws.^ ~ ')')
    val title = ws.^ ~> (('"' ~> delimitedBy("\"", titleEnd)) | ('\'' ~> delimitedBy("'", titleEnd)))

    val url = ('<' ~> text(delimitedBy('>',' ').keepDelimiter, Map('\\' -> recParsers.escapedChar)) <~ '>') |
       text(delimitedBy(')',' ','\t').keepDelimiter, Map('\\' -> recParsers.escapedChar))
    
    val urlWithTitle = '(' ~> url ~ opt(title) <~ ws ~ ')' ^^ {  
      case url ~ title => (recParser: RecParser, text:String) => inline(recParser, text, url, title)
    }
    val refId =    ws ~ opt(eol) ~ ('[' ~> recParsers.escapedUntil(']')) ^^ {
      case ws ~ lb ~ id => (recParser: RecParser, text:String) =>
        ref(recParser, text, id,   "]"+ws+ lb.getOrElse("") +"["+id+"]") }

    val refEmpty = ws ~ opt(eol) ~ "[]" ^^ {
      case ws ~ lb ~ _  => (recParser: RecParser, text:String) =>
        ref(recParser, text, text, "]"+ws+ lb.getOrElse("") +"[]") }

    recParsers.withRecursiveSpanParser(linktext) ~ opt(urlWithTitle | refEmpty | refId) ^^ {
      case (recParser, text) ~ None    => ref(recParser, text, text, "]")
      case (recParser, text) ~ Some(f) => f(recParser, text)
    }
  }

  /** Parses a simple inline link in the form of &lt;http://someURL/&gt;
    */
  val simpleLink: SpanParserBuilder = SpanParser.forStartChar('<').standalone {

    def isAcceptedScheme (s: String) = s == "http" || s == "https" || s == "ftp" || s == "mailto"
    def isURI (s: String) = try { val uri = new java.net.URI(s); uri.isAbsolute && isAcceptedScheme(uri.getScheme) } catch { case _:Throwable => false }
    def isEmail (s: String) = s.contains("@") && isURI(s"mailto:$s") // TODO - improve

    def toLink(s: String) = ExternalLink(List(Text(s)), s)

    anyBut(' ','\r','\n','\t','>') <~ '>' ^? {
      case s if isURI(s) => toLink(s)
      case s if isEmail(s) => toLink(s"mailto:$s")
    }
  }

}
