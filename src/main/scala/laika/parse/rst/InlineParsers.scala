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

package laika.parse.rst

import laika.tree.Elements._
import Elements.SubstitutionReference
import Elements.InterpretedText
import scala.util.parsing.input.CharSequenceReader
import scala.collection.mutable.ListBuffer
import laika.parse.util.URIParsers

/** Provides all inline parsers for reStructuredText.
 *  
 *  Inline parsers deal with markup within a block of text, such as a
 *  link or emphasized text. They are used in the second phase of parsing,
 *  after the block parsers have cut the document into a (potentially nested)
 *  block structure.
 * 
 *  @author Jens Halm
 */
trait InlineParsers extends laika.parse.InlineParsers with URIParsers {

  
  private val pairs = Map(/* Ps/Pe pairs */
              '('->')', '['->']', '{'->'}', '\u0f3a'->'\u0f3b', '\u0f3c'->'\u0f3d', '\u169b'->'\u169c', '\u2045'->'\u2046',
                  '\u207d'->'\u207e', '\u208d'->'\u208e', '\u2329'->'\u232a', '\u2768'->'\u2769', '\u276a'->'\u276b',
                  '\u276c'->'\u276d', '\u276e'->'\u276f', '\u2770'->'\u2771', '\u2772'->'\u2773', '\u2774'->'\u2775',
                  '\u27c5'->'\u27c6', '\u27e6'->'\u27e7', '\u27e8'->'\u27e9', '\u27ea'->'\u27eb', '\u27ec'->'\u27ed',
                  '\u27ee'->'\u27ef', '\u2983'->'\u2984', '\u2985'->'\u2986', '\u2987'->'\u2988', '\u2989'->'\u298a',
                  '\u298b'->'\u298c', '\u298d'->'\u298e', '\u298f'->'\u2990', '\u2991'->'\u2992', '\u2993'->'\u2994',
                  '\u2995'->'\u2996', '\u2997'->'\u2998', '\u29d8'->'\u29d9', '\u29da'->'\u29db', '\u29fc'->'\u29fd',
                  '\u2e22'->'\u2e23', '\u2e24'->'\u2e25', '\u2e26'->'\u2e27', '\u2e28'->'\u2e29', '\u3008'->'\u3009',
                  '\u300a'->'\u300b', '\u300c'->'\u300d', '\u300e'->'\u300f', '\u3010'->'\u3011', '\u3014'->'\u3015',
                  '\u3016'->'\u3017', '\u3018'->'\u3019', '\u301a'->'\u301b', '\u301d'->'\u301e', '\ufd3e'->'\ufd3f',
                  '\ufe17'->'\ufe18', '\ufe35'->'\ufe36', '\ufe37'->'\ufe38', '\ufe39'->'\ufe3a', '\ufe3b'->'\ufe3c',
                  '\ufe3d'->'\ufe3e', '\ufe3f'->'\ufe40', '\ufe41'->'\ufe42', '\ufe43'->'\ufe44', '\ufe47'->'\ufe48',
                  '\ufe59'->'\ufe5a', '\ufe5b'->'\ufe5c', '\ufe5d'->'\ufe5e', '\uff08'->'\uff09', '\uff3b'->'\uff3d',
                  '\uff5b'->'\uff5d', '\uff5f'->'\uff60', '\uff62'->'\uff63', 
                  /* Pi/Pf pairs */
                  '\u00ab'->'\u00bb', '\u2018'->'\u2019', '\u201c'->'\u201d', '\u2039'->'\u203a', '\u2e02'->'\u2e03',
                  '\u2e04'->'\u2e05', '\u2e09'->'\u2e0a', '\u2e0c'->'\u2e0d', '\u2e1c'->'\u2e1d', '\u2e20'->'\u2e21',
                  /* Pi/Pf pairs reverse */
                  '\u00bb'->'\u00ab', '\u2019'->'\u2018', '\u201d'->'\u201c', '\u203a'->'\u2039', '\u2e03'->'\u2e02',
                  '\u2e05'->'\u2e04', '\u2e0a'->'\u2e09', '\u2e0d'->'\u2e0c', '\u2e1d'->'\u2e1c', '\u2e21'->'\u2e20',
                  /* pairs added explicitly in the reStructuredText ref impl */
                  '\u301d'->'\u301f', '\u201a'->'\u201b', '\u201e'->'\u201f', '\u201b'->'\u201a', '\u201f'->'\u201e',
                  /* additional pairing of open/close quotes for different typographic conventions in different languages */
                  '\u00bb'->'\u00bb', '\u2018'->'\u201a', '\u2019'->'\u2019', '\u201a'->'\u2018', '\u201a'->'\u2019',
                  '\u201c'->'\u201e', '\u201e'->'\u201c', '\u201e'->'\u201d', '\u201d'->'\u201d', '\u203a'->'\u203a')

                  
  private val startChars = anyOf(' ','-',':','/','\'','"','<','(','[','{') take 1
  
  private val startCategories = Set[Int](Character.DASH_PUNCTUATION, Character.OTHER_PUNCTUATION, Character.START_PUNCTUATION,
                            Character.INITIAL_QUOTE_PUNCTUATION, Character.FINAL_QUOTE_PUNCTUATION)
   
  private val endChars = anyOf(' ','-','.',',',':',';','!','?','\\','/','\'','"','>',')',']','}','\u201a','\u201e') take 1
  
  private val endCategories = Set[Int](Character.DASH_PUNCTUATION, Character.OTHER_PUNCTUATION, Character.END_PUNCTUATION,
                            Character.INITIAL_QUOTE_PUNCTUATION, Character.FINAL_QUOTE_PUNCTUATION)                          

  
  /** Parses the markup at the start of an inline element according to reStructuredText markup recognition rules.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#inline-markup-recognition-rules]].
   * 
   *  @param start the parser that recognizes the markup at the start of an inline element
   *  @param end the parser that recognizes the markup at the end of an inline element, needed to verify
   *  the start sequence is not immediately followed by an end sequence as empty elements are not allowed.
   *  @return a parser without a useful result, as it is only needed to verify it succeeds
   */                          
  def markupStart (start: Parser[Any], end: Parser[String]): Parser[Any] = {
    ((lookBehind(2, beforeStartMarkup) | lookBehind(1, atStart ^^^ ' ')) >> afterStartMarkup(start)) ~ not(end) // not(end) == rule 6
  }
  
  /** Parses the start of an inline element without specific start markup 
   *  according to reStructuredText markup recognition rules.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#inline-markup-recognition-rules]].
   * 
   *  @param end the parser that recognizes the markup at the end of an inline element, needed to verify
   *  the start sequence is not immediately followed by an end sequence as empty elements are not allowed.
   *  @return a parser without a useful result, as it is only needed to verify it succeeds
   */ 
  def markupStart (end: Parser[String]): Parser[Any] = markupStart(success(()), end)
  
  /** Parses the end of an inline element  according to reStructuredText markup recognition rules.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#inline-markup-recognition-rules]].
   * 
   *  @param end the parser that recognizes the markup at the end of an inline element
   *  @return a parser that produces the same result as the parser passed as an argument
   */
  def markupEnd (end: Parser[String]) = {
    end >> { markup => (lookBehind(markup.length, beforeEndMarkup) ~ guard(eol | afterEndMarkup)) ^^^ markup }
  }
  
  /** Inline markup recognition rules 2 and 5
   */
  private def afterStartMarkup (start: Parser[Any])(before: Char) = {
    val matching = pairs.getOrElse(before, ' ') 
    start ~ guard(anyBut(' ','\n', matching) take 1)
  }
  
  /** Inline markup recognition rules 3
   */
  private val beforeEndMarkup = anyBut(' ','\n') take 1
  
  /** Inline markup recognition rule 1
   */
  private val beforeStartMarkup = startChars ^^ {_.charAt(0)} | acceptIf(char => startCategories(Character.getType(char)))("Not a start char: " + _)
  
  /** Inline markup recognition rule 4
   */
  private val afterEndMarkup = endChars | acceptIf(char => endCategories(Character.getType(char)))("Not an end char: " + _)
  
  
  /** A mapping of the start character of an inline element to the corresponding parser.
   *  The mapping is used to provide a fast implementation of an inline parser that
   *  only stops at known special characters. 
   */
  lazy val spanParsers = Map(
    '*' -> (strong | em),   
    '`' -> (inlineLiteral | phraseLinkRef | interpretedTextWithRoleSuffix),
    '[' -> (footnoteRef | citationRef),
    '|' -> substitutionRef,
    '_' -> (internalTarget | simpleLinkRef),
    ':' -> (interpretedTextWithRolePrefix | trim(uri)),
    '@' -> trim(email),
    '\\'-> (escapedChar ^^ (Text(_)))
  )

  
  /** Parses an escaped character. For most characters it produces the character
   *  itself as the result with the only exception being an escaped space character
   *  which is removed from the output in reStructuredText.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#escaping-mechanism]].
   */
  lazy val escapedChar = (" " ^^^ "") | (any take 1)

  
  /** Parses a span of text until the specified character is seen 
   *  (unless it is escaped),
   *  while also processing escaped characters, but no other nested
   *  spans. The final character is not included in the result.
   * 
   *  @param char the character that signals the end of the text span
   *  @return a parser for a text span that supports escape sequences
   */  
  def escapedUntil (char: Char) = escapedText(anyUntil(char) min 1)

  /** Adds support for escape sequences to the specified text parser.
   * 
   *  @param p the parser to add support for escape sequences to
   *  @return a parser for a text span that supports escape sequences
   */  
  def escapedText (p: TextParser) = text(p, Map('\\' -> escapedChar))
  
  /** Parses a span of emphasized text.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#emphasis]]
   */
  lazy val em = span("*") ^^ (Emphasized(_))
  
  /** Parses a span of text with strong emphasis.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#strong-emphasis]]
   */
  lazy val strong = span('*',"**") ^^ (Strong(_))
  

  private def span (start: Parser[Any], end: Parser[String]): Parser[List[Span]]
    = markupStart(start, end) ~> escapedText(anyUntil(markupEnd(end))) ^^ { text => List(Text(text)) }
    
  private def span (end: Parser[String]): Parser[List[Span]] = span(success(()), end)
    
  
  /** Parses an inline literal element.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#inline-literals]].
   */
  lazy val inlineLiteral = markupStart('`', "``") ~> anyUntil(markupEnd("``")) ^^ (Literal(_))
  
  
  /** Represent a reference name.
   *  When resolving references whitespace needs to be normalized
   *  and the name converted to lower case.
   */
  case class ReferenceName (original: String) {
    lazy val normalized = original.replaceAll("[\n ]+", " ").toLowerCase
  }
  
  /** Parses a simple reference name that only allows alphanumerical characters
   *  and the punctuation characters `-`, `_`, `.`, `:`, `+`.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#reference-names]].
   */
  val simpleRefName = {
    val alphanum = anyIn('0' to '9', 'a' to 'z', 'A' to 'Z') min 1 // TODO - check whether non-ascii characters are allowed
    val symbol = anyOf('-', '_', '.', ':', '+') take 1
    
    alphanum ~ ((symbol ~ alphanum)*) ^^ { 
      case start ~ rest => start + (rest map { case a~b => a+b }).mkString
    }
  }
  
  /** Parses a reference name, either a simple or a phrase reference name.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#reference-names]].
   */
  val refName = (simpleRefName | phraseRef) ^^ ReferenceName
  
  /** Parses a phrase reference name enclosed in back ticks.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#reference-names]].
   */
  val phraseRef = '`' ~> escapedUntil('`')
  
  
  /** Parses any of the four supported types of footnote labels.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#footnote-references]].
   */
  val footnoteLabel = {
    val decimal = (anyIn('0' to '9') min 1) ^^ { n => NumericLabel(n.toInt) }
    val autonumber = '#' ^^^ Autonumber 
    val autosymbol = '*' ^^^ Autosymbol
    val autonumberLabel = '#' ~> simpleRefName ^^ AutonumberLabel 
    
    decimal | autonumberLabel | autonumber | autosymbol
  }
  
  private def toSource (label: FootnoteLabel) = label match {
    case Autonumber => "[#]_"
    case Autosymbol => "[*]_"
    case AutonumberLabel(label) => "[#"+label+"]_"
    case NumericLabel(label) => "["+label+"]_"
  }
  
  /** Parses a footnote reference.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#footnote-references]].
   */
  lazy val footnoteRef = markupStart("]_") ~> footnoteLabel <~ markupEnd("]_") ^^ 
      { label => FootnoteReference(label, toSource(label)) }
  
  /** Parses a citation reference.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#citation-references]].
   */
  lazy val citationRef = markupStart("]_") ~> simpleRefName <~ markupEnd("]_") ^^
      { label => CitationReference(label, "["+label+"]_") }
  
  /** Parses a substitution reference.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#substitution-references]].
   */
  lazy val substitutionRef = markupStart("|") ~> simpleRefName >> { ref =>
    markupEnd("|__") ^^ { _ => LinkReference(List(SubstitutionReference(ref)), "", "|" + ref + "|__") } | 
    markupEnd("|_")  ^^ { _ => LinkReference(List(SubstitutionReference(ref)), ref, "|" + ref + "|_") } |
    markupEnd("|")   ^^ { _ => SubstitutionReference(ref) } 
  }
  
  /** Parses an inline internal link target.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#inline-internal-targets]]
   */
  val internalTarget = markupStart('`', "`") ~> 
    (escapedText(anyBut('`') min 1) ^^ {ReferenceName(_).normalized}) <~ 
    markupEnd("`") ^^ (id => InternalLinkTarget(Id(id)))
  
  /** The default text role to use when no role is specified in an interpreted text element.
   */
  def defaultTextRole = "title-reference"

  /** Parses an interpreted text element with the role name as a prefix.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#interpreted-text]]
   */  
  lazy val interpretedTextWithRolePrefix = {
    (markupStart(":") ~> simpleRefName) ~ (":`" ~> escapedText(anyBut('`') min 1) <~ markupEnd("`")) ^^ 
      { case role ~ text => InterpretedText(role,text,":"+role+":`"+text+"`") }
  }
  
  /** Parses an interpreted text element with the role name as a suffix.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#interpreted-text]]
   */  
  lazy val interpretedTextWithRoleSuffix = {
    (markupStart("`") ~> escapedText(anyBut('`') min 1) <~ markupEnd("`")) ~ opt(":" ~> simpleRefName <~ markupEnd(":")) ^^
      { case text ~ role => InterpretedText(role.getOrElse(defaultTextRole), text, "`"+text+"`"+role.map(":"+_+":").getOrElse("")) }
  }
  
  /** Parses a phrase link reference (enclosed in back ticks).
   *  
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#hyperlink-references]]
   */
  lazy val phraseLinkRef = {
    def ref (refName: String, url: String) = if (refName.isEmpty) url else refName
    val url = '<' ~> anyBut('>') <~ '>' ^^ { _.replaceAll("[ \n]+", "") }
    val refName = escapedText(anyBut('`','<')) ^^ ReferenceName
    markupStart("`") ~> refName ~ opt(url) ~ (markupEnd("`__") ^^^ false | markupEnd("`_") ^^^ true) ^^ {
      case refName ~ Some(url) ~ true   => 
        SpanSequence(List(ExternalLink(List(Text(ref(refName.original, url))), url), ExternalLinkDefinition(ref(refName.normalized, url), url)))
      case refName ~ Some(url) ~ false  => ExternalLink(List(Text(ref(refName.original, url))), url)
      case refName ~ None ~ true        => LinkReference(List(Text(refName.original)), refName.normalized, "`" + refName.original + "`_") 
      case refName ~ None ~ false       => LinkReference(List(Text(refName.original)), "", "`" + refName.original + "`__") 
    }
  }
  
  protected case class Reverse (length: Int, target: Span, fallback: Span, options: Options = NoOpt) extends Span
  
  /** Parses a simple link reference.
   *  
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#hyperlink-references]]
   */
  lazy val simpleLinkRef: Parser[Span] = {
    markupEnd('_' ^^^ "__" | success("_")) >> { 
      markup => reverse(markup.length, simpleRefName <~ reverseMarkupStart) ^^ { refName =>
        markup match {
          case "_"  => Reverse(refName.length, LinkReference(List(Text(refName)), ReferenceName(refName).normalized, "" + refName + "_"), Text("_")) 
          case "__" => Reverse(refName.length, LinkReference(List(Text(refName)), "", "" + refName + "__"), Text("__")) 
        }
      }
    } 
  }
  
  private def reverse (offset: Int, p: => Parser[String]) = Parser { in =>
    val source = in.source.subSequence(0, in.offset - offset).toString.reverse
    p(new CharSequenceReader(source)) match {
      case Success(result, _) => Success(result.reverse, in)
      case Failure(msg, _) => Failure(msg, in)
      case Error(msg, _) => Error(msg, in)
    }
  }
  
  private lazy val reverseMarkupStart: Parser[Any] = guard(eof | beforeStartMarkup)
  
  
  override def parseInline (source: String, spanParsers: Map[Char, Parser[Span]]) = {
    val spans = super.parseInline(source, spanParsers)
    if (spans.isEmpty) spans
    else {
      val buffer = new ListBuffer[Span]
      val last = (spans.head /: spans.tail) { 
        case (t @ Text(content,_), Reverse(len, target, fallback, _)) =>
          if (content.length < len) { buffer += t; fallback }
          else { buffer += Text(content.dropRight(len)); target }
        case (prev, Reverse(_, _, fallback, _)) => buffer += prev; fallback
        case (prev, current)                    => buffer += prev; current
      }
      buffer += last
      buffer.toList
    }
  }
  
  
  private def trim (p: Parser[(String,String,String)]): Parser[Span] = p >> { res => Parser { in =>
    val startChar = Set('-',':','/','\'','(','{')
    val endChar   = Set('-',':','/','\'',')','}','.',',',';','!','?') 
    res match {
      case (start, sep, end) => 
        val startTrimmed = start.dropWhile(startChar)
        val endTrimmed = end.reverse.dropWhile(endChar).reverse
        val uri = startTrimmed + sep + endTrimmed
        val uriWithScheme = if (sep == "@" && !uri.startsWith("mailto:")) "mailto:"+uri else uri 
        val nextIn = in.drop(endTrimmed.length - end.length)
        Success(Reverse(startTrimmed.length, ExternalLink(List(Text(uri)), uriWithScheme), Text(sep+endTrimmed)), nextIn)
    }
  }}
  
  /** Parses a standalone HTTP or HTTPS hyperlink (with no surrounding markup).
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#standalone-hyperlinks]]
   */
  lazy val uri = reverse(1, ("ptth" | "sptth") <~ reverseMarkupStart) ~ httpUriNoScheme ^^ {
    case scheme ~ rest => (scheme, ":", rest)
  }
  
  /** Parses a standalone email address (with no surrounding markup).
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#standalone-hyperlinks]]
   */
  lazy val email = reverse(1, localPart <~ reverseMarkupStart) ~ domain ^^ {
    case local ~ domain => (local, "@", domain)
  }
  
  
}