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

package laika.parse.markdown

import laika.tree.Elements._
import java.net.URI
 
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
trait InlineParsers extends laika.parse.InlineParsers { self =>
 
  
  /** Creates a new mapping from the start character of an inline span
    * to the corresponding parser. May be overridden by subtraits.
    */
  protected def prepareSpanParsers = Map(
    '*' -> (strong('*') | em('*')),    
    '_' -> (strong('_') | em('_')),
    '`' -> (literalEnclosedByDoubleChar | literalEnclosedBySingleChar), 
    '\\'-> (lineBreak | (escapedChar ^^ { Text(_) })),
    '[' -> link,
    '<' -> simpleLink,
    '!' -> image
  )
  
  
  /** Parses a single escaped character, only recognizing the characters the Markdown syntax document
   *  specifies as escapable.
   * 
   *  Note: escaping > is not mandated by the official syntax description, but by the official test suite.
   */
  override lazy val escapedChar = anyOf('\\', '`', '*', '_', '{', '}', '[', ']', '(', ')', '#', '+', '-', '.', '!', '>') take 1
  
  /** Parses an explicit hard line break.
   */
  def lineBreak = (anyOf('\r') take 1) ^^^ LineBreak()
  
  /** Parses a span of strong text enclosed by two consecutive occurrences of the specified character. 
   */
  def strong (char: Char) = enclosedByDoubleChar(char) ^^ { Strong(_) }
  
  /** Parses a span of emphasized text enclosed by one occurrence of the specified character.
   */
  def em (char: Char) = enclosedBySingleChar(char) ^^ { Emphasized(_) }

  
  /** Creates a parser for an inline span based on the specified parsers that
   *  represent the start and end condition.
   * 
   *  @param start the parser that parses the beginning of the span, result will be discarded
   *  @param end the parser that recognizes the end of the span, result will be discarded
   */
  def span (start: Parser[Any], end: Parser[Any]): Parser[List[Span]]
    = start ~> spans(anyUntil(end), spanParsers)
  
  /** Parses a span enclosed by a single occurrence of the specified character.
   *  Recursively parses nested spans, too. 
   */
  def enclosedBySingleChar (char: Char) = {
    val start = not(elem(' ') | elem(char))
    val end = char ~ not(lookBehind(2, ' '))
    span(start, end) 
  }
  
  /** Parses a span enclosed by two consecutive occurrences of the specified character.
   *  Recursively parses nested spans, too. 
   */
  def enclosedByDoubleChar (char: Char) = {
    val start = char ~ not(' ')
    val end = char ~ char ~ not(lookBehind(3, ' '))
    span(start, end)
  }
  
  /** Parses a literal span enclosed by a single backtick.
   *  Does neither parse nested spans nor Markdown escapes. 
   */
  def literalEnclosedBySingleChar = { 
    val start = not('`')
    val end = '`'
    start ~> anyUntil(end) ^^ { s => Literal(s.trim) }
  }
  
  /** Parses a literal span enclosed by double backticks.
   *  Does neither parse nested spans nor Markdown escapes. 
   */
  def literalEnclosedByDoubleChar = {
    val start = '`'
    val end = "``"
    start ~> anyUntil(end) ^^ { s => Literal(s.trim) }
  }
  
  
  private val escapedChars = Map('\\' -> escapedChar)
  
  
  private def normalizeId (id: String) = id.toLowerCase.replaceAll("[\n ]+", " ")
  
  /** Parses a link, including nested spans in the link text.
   *  Recognizes both, an inline link `[text](url)` and a link reference `[text][id]`.
   */
  def link: Parser[Span] = {
    lazy val linkSpanParsers = spanParsers // - '['
    
    def unwrap (ref: LinkReference, suffix: String) = {
      if (!(ref select (_.isInstanceOf[LinkReference])).tail.isEmpty)
        SpanSequence(Text("[") :: ref.content.toList ::: Text(suffix) :: Nil)
      else ref
    }
    
    def linkInline (text: String, url: String, title: Option[String]) = ExternalLink(parseInline(text, linkSpanParsers), url, title)
    def linkReference (text: String, id: String, suffix: String): Span = {
      /* Markdown's design comes with a few arbitrary and inconsistent choices for how to handle nesting of brackets. 
       * The logic here is constructed to make the official test suite pass, other edge cases might still yield unexpected results.
       * Users usually should not bother and simply escape brackets which are not meant to be markup. */
      val ref = LinkReference(parseInline(text, linkSpanParsers), normalizeId(id), "[" + text + suffix)
      if (text == id) unwrap(ref, suffix) else ref
    }
    
    resource(linkInline, linkReference)
  }
  
  /** Parses an inline image.
   *  Recognizes both, an inline image `![text](url)` and an image reference `![text][id]`.
   */
  def image: Parser[Span] = {
    def imageInline (text: String, url: String, title: Option[String]) = Image(text, url, title)
    def imageReference (text: String, id: String, postFix: String): Span = ImageReference(text, normalizeId(id), "![" + text + postFix)
     
    '[' ~> resource(imageInline, imageReference)
  }
  
  /** Helper function that abstracts the common parser logic of links and images.
   * 
   *  @param inline factory function for creating a new inline link or image based on the text, url and optional title parameters
   *  @param ref factory function for creating a new link or image reference based on the text and id parameters
   */
  def resource (inline: (String, String, Option[String]) => Span, ref: (String, String, String) => Span): Parser[Span] = {
    
    val linktext = text(anyUntil(']'), Map('\\' -> escapedChar, '[' -> (anyUntil(']') ^^ { "[" + _ + "]" })))
    
    val title = ws ~> (('"' ~> anyUntil('"' ~ guard(ws ~ ')'))) | ('\'' ~> anyUntil('\'' ~ guard(ws ~ ')')))) 
    
    val url = ('<' ~> self.text(anyBut('>',' '), Map('\\' -> escapedChar)) <~ '>') |
       self.text(anyBut(')',' ','\t'), Map('\\' -> escapedChar))
    
    val urlWithTitle = '(' ~> url ~ opt(title) <~ ws ~ ')' ^^ {  
      case url ~ title => text:String => inline(text, url, title)  
    }
    
    val refId =    ws ~ opt(eol) ~ ('[' ~> escapedUntil(']')) ^^ { 
      case ws ~ lb ~ id => text:String => ref(text, id,   "]"+ws+ lb.getOrElse("") +"["+id+"]") }
    val refEmpty = ws ~ opt(eol) ~ "[]" ^^ { 
      case ws ~ lb ~ _  => text:String => ref(text, text, "]"+ws+ lb.getOrElse("") +"[]") }
  
    linktext ~ opt(urlWithTitle | refEmpty | refId) ^^ {
      case text ~ None    => ref(text, text, "]")
      case text ~ Some(f) => f(text)  
    }
  }
  
  /** Parses a simple inline link in the form of &lt;http://someURL/&gt;
   */
  def simpleLink = {
    
    def isAcceptedScheme (s: String) = s == "http" || s == "https" || s == "ftp" || s == "mailto"
    def isURI (s: String) = try { val uri = new URI(s); uri.isAbsolute && isAcceptedScheme(uri.getScheme) } catch { case _:Throwable => false }
    def isEmail (s: String) = s.contains("@") && isURI("mailto:" + s) // TODO - improve
    
    def toLink(s: String) = ExternalLink(List(Text(s)), s) 
    
    anyBut(' ','\r','\n','\t','>') <~ '>' ^? { 
      case s if isURI(s) => toLink(s)
      case s if isEmail(s) => toLink("mailto:" + s)
    }
  }
  
  /** Parses a link definition in the form `[id]: <url> "title"`.
   *  The title is optional as well as the quotes around it and the angle brackets around the url.
   */
  def linkTarget = {
    
    val id = '[' ~> escapedUntil(']') <~ ':' <~ ws
    val url = (('<' ~> escapedUntil('>')) | text(anyBut(' ', '\n'), escapedChars)) ^^ { _.mkString }
    
    def enclosedBy(start: Char, end: Char) = 
      start ~> anyUntil((guard(end ~ ws ~ eol) | '\r' | '\n')) <~ end ^^ { _.mkString }
    
    val title = (ws ~ opt(eol) ~ ws) ~> (enclosedBy('"', '"') | enclosedBy('\'', '\'') | enclosedBy('(', ')'))
    
    id ~ url ~ opt(title) <~ ws ~ eol ^^ { case id ~ url ~ title => ExternalLinkDefinition(id.toLowerCase, url, title) } 
  }
  
  
}