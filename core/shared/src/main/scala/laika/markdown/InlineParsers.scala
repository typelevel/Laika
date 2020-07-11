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
import laika.parse.builders._
import laika.parse.implicits._
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
  val lineBreak: SpanParserBuilder = SpanParser.standalone(literal("\\\r").as(LineBreak()))
  
  /** Parses a span of strong text enclosed by two consecutive occurrences of the specified character. 
   */
  def strong (char: Char)(implicit recParsers: RecursiveSpanParsers): PrefixedParser[Strong] = enclosedByDoubleChar(char).map { Strong(_) }
  
  /** Parses a span of emphasized text enclosed by one occurrence of the specified character.
   */
  def em (char: Char)(implicit recParsers: RecursiveSpanParsers): PrefixedParser[Emphasized] = enclosedBySingleChar(char).map { Emphasized(_) }

  
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
    start ~> delimitedBy(end).trim.map(Literal(_))
  }
  
  /** Parses a literal span enclosed by double backticks.
   *  Does neither parse nested spans nor Markdown escapes. 
   */
  val literalEnclosedByDoubleChar: PrefixedParser[Literal] = {
    val delim = "``"
    delim ~> delimitedBy(delim).trim.map(Literal(_))
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

    def unwrap (ref: LinkIdReference, suffix: String) = {
      if ((ref select (_.isInstanceOf[LinkIdReference])).tail.nonEmpty)
        SpanSequence(Text("[") :: ref.content.toList ::: Text(suffix) :: Nil)
      else ref
    }

    def linkReference (res: Resource, id: String): Span = {
      /* Markdown's design comes with a few arbitrary and inconsistent choices for how to handle nesting of brackets.
       * The logic here is constructed to make the official test suite pass, other edge cases might still yield unexpected results.
       * Users usually should not bother and simply escape brackets which are not meant to be markup. */
      val ref = LinkIdReference(res.parser(res.text), normalizeId(id), "[" + res.source)
      if (res.text == id) unwrap(ref, res.suffix) else ref
    }

    "[" ~> resource(recParsers).map { res =>
      res.target match {
        case TargetUrl(url, title) => Link.create(res.parser(res.text), url, res.source, title)
        case TargetId(id)   => linkReference(res, id)
        case ImplicitTarget => linkReference(res, res.text)
      }
    }
  }

  /** Parses an inline image.
    *  Recognizes both, an inline image `![text](url)` and an image reference `![text][id]`.
    */
  val image: SpanParserBuilder = SpanParser.recursive { recParsers =>

    def escape (text: String, f: String => Span): Span = 
      recParsers.escapedText(DelimitedText.Undelimited).parse(text).toEither.fold(InvalidElement(_, text).asSpan, f)

    "![" ~> resource(recParsers).map { res =>
      res.target match {
        case TargetUrl(url, title) => escape(res.text, Image.create(_, url, res.source, title = title))
        case TargetId(id)   => escape(res.text, ImageIdReference(_, normalizeId(id),       "![" + res.source))
        case ImplicitTarget => escape(res.text, ImageIdReference(_, normalizeId(res.text), "![" + res.source))
      }
    }
  }
  
  private sealed trait ResourceTarget
  private case class TargetId(id: String) extends ResourceTarget
  private case class TargetUrl(url: String, title: Option[String] = None) extends ResourceTarget
  private case object ImplicitTarget extends ResourceTarget
  private case class Resource(parser: RecParser, text: String, target: ResourceTarget, suffix: String) {
    def source: String = text + suffix
  }
  
  /** Helper function that abstracts the common parser logic of links and images.
   */
  private def resource (recParsers: RecursiveSpanParsers): Parser[Resource] = {

    val linkText = text(delimitedBy("]"))
      .embed(recParsers.escapeSequence.map {"\\" + _})
      .embed("[" ~> delimitedBy("]").map { "[" + _ + "]" })

    val titleEnd = ws.void ~ ")"
    def enclosedIn(delim: String): Parser[String] = delim ~> delimitedBy(delim <~ lookAhead(titleEnd))
    val title = ws.void ~> (enclosedIn("\"") | enclosedIn("'"))

    val url = ("<" ~> text(delimitedBy('>').failOn(' ')).embed(recParsers.escapeSequence)) |
       text(delimitedBy(')',' ','\t').keepDelimiter).embed(recParsers.escapeSequence)
    
    val urlWithTitle = ("(" ~> url ~ opt(title) <~ ws ~ ")").mapN(TargetUrl).withSource
    
    val refId = (ws ~ opt(eol) ~ "[" ~> recParsers.escapedUntil(']').map(TargetId)).withSource

    val refEmpty = (ws ~ opt(eol) ~ "[]").source.map((ImplicitTarget, _))
    val noRef = success((ImplicitTarget,""))

    recParsers.withRecursiveSpanParser(linkText) ~ (urlWithTitle | refEmpty | refId | noRef) ^^ {
      case (recParser, text) ~ ((target, source)) => Resource(recParser, text, target, "]" + source)
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

    def toLink(s: String) = SpanLink(List(Text(s)), ExternalTarget(s))

    ("<" ~> anyNot(' ','\r','\n','\t','>') <~ ">").collect {
      case s if isURI(s) => toLink(s)
      case s if isEmail(s) => toLink(s"mailto:$s")
    }
    
  }

}
