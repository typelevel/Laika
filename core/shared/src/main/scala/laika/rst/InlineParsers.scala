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

package laika.rst

import cats.data.NonEmptySet
import cats.implicits._
import laika.ast._
import laika.bundle.{SpanParser, SpanParserBuilder}
import laika.collection.TransitionalCollectionOps._
import laika.parse.markup.RecursiveSpanParsers
import laika.parse.text.PrefixedParser
import laika.parse.builders._
import laika.parse.implicits._
import laika.parse.uri.AutoLinkParsers
import laika.parse.{Failure, Message, Parser, Success}
import laika.rst.BaseParsers._
import laika.rst.ast.{InterpretedText, ReferenceName, RstStyle, SubstitutionReference}


/** Provides all inline parsers for reStructuredText.
 *  
 *  Inline parsers deal with markup within a block of text, such as a
 *  link or emphasized text. They are used in the second phase of parsing,
 *  after the block parsers have cut the document into a (potentially nested)
 *  block structure.
 * 
 *  @author Jens Halm
 */
object InlineParsers {

  /** Parses an escaped character. For most characters it produces the character
    * itself as the result with the only exception being an escaped space character
    * which is removed from the output in reStructuredText.
    *
    * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#escaping-mechanism]].
    */
  val escapedChar: Parser[String] = literal(" ").as("") | oneChar


  private val pairs: Map[Char, Set[Char]] = List(/* Ps/Pe pairs */
                  '('->')', '['->']', '{'->'}', '<'->'>', '"'->'"', '\''->'\'', 
                  '\u0f3a'->'\u0f3b', '\u0f3c'->'\u0f3d', '\u169b'->'\u169c', '\u2045'->'\u2046',
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
                  .groupBy(_._1).mapValuesStrict(_.map(_._2).toSet)

                  
  private val startChars = NonEmptySet.of(' ','-',':','/','\'','"','<','(','[','{','\n')
  
  private val startCategories = Set[Int](Character.DASH_PUNCTUATION, Character.OTHER_PUNCTUATION, Character.START_PUNCTUATION,
                            Character.INITIAL_QUOTE_PUNCTUATION, Character.FINAL_QUOTE_PUNCTUATION)
   
  private val endChars = NonEmptySet.of(' ','-','.',',',':',';','!','?','\\','/','\'','"','>',')',']','}','\u201a','\u201e')
  
  private val endCategories = Set[Int](Character.DASH_PUNCTUATION, Character.OTHER_PUNCTUATION, Character.END_PUNCTUATION,
                            Character.INITIAL_QUOTE_PUNCTUATION, Character.FINAL_QUOTE_PUNCTUATION)

  /** Inline markup recognition rules 1 and 2 
    * (applies to the character after the start delimiter and before the end delimiter).
    */
  private val invalidInsideDelimiter: NonEmptySet[Char] = NonEmptySet.of(' ','\n')

  /** Inline markup recognition rule 6.
    */
  private val invalidBeforeStartMarkup: Char => Boolean =
    char => !startChars.contains(char) && !startCategories.contains(Character.getType(char))
  
  /** Inline markup recognition rule 7.
    */
  private val invalidAfterEndMarkup: Char => Boolean = char =>
    !endChars.contains(char) && !endCategories.contains(Character.getType(char)) && char != '\n' // TODO - could \n be in endCharSet?

  /** Parses the markup at the start of an inline element according to reStructuredText markup recognition rules.
    *
    *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#inline-markup-recognition-rules]].
    *
    *  @param start the literal string at the start of an inline element
    *  @param end the literal string at the end of an inline element, needed to verify
    *  the start sequence is not immediately followed by an end sequence as empty elements are not allowed.
    *  @return a parser without a useful result, as it is only needed to verify it succeeds
    */
  def markupStart (start: String, end: String): PrefixedParser[Any] = markupStart(literal(start), literal(end))
  
  /** Parses the markup at the start of an inline element according to reStructuredText markup recognition rules.
    *
    *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#inline-markup-recognition-rules]].
    *
    *  @param start the parser that recognizes the markup at the start of an inline element
    *  @param end the parser that recognizes the markup at the end of an inline element, needed to verify
    *  the start sequence is not immediately followed by an end sequence as empty elements are not allowed.
    *  @return a parser without a useful result, as it is only needed to verify it succeeds
    */
  def markupStart (start: PrefixedParser[String], end: Parser[String]): PrefixedParser[Any] = {

    /* Inline markup recognition rules 5 */
    def invalidPair (before: Char, after: Char): Boolean = pairs.getOrElse(before, Set()).contains(after)

    delimiter(start)
      .prevNot(invalidBeforeStartMarkup)
      .nextNot(invalidInsideDelimiter)
      .notEnclosedBy(invalidPair) ~ not(end) // not(end) == rule 3
  }

  /** Parses the end of an inline element according to reStructuredText markup recognition rules.
    *
    *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#inline-markup-recognition-rules]].
    *
    *  @param end the literal string expected as the delimiter at the end of an inline element
    *  @return a parser that produces the same result as the parser passed as an argument
    */
  def markupEnd (end: String): PrefixedParser[String] = markupEnd(literal(end))
  
  /** Parses the end of an inline element according to reStructuredText markup recognition rules.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#inline-markup-recognition-rules]].
   * 
   *  @param end the parser that recognizes the markup at the end of an inline element
   *  @return a parser that produces the same result as the parser passed as an argument
   */
  def markupEnd (end: PrefixedParser[String]): PrefixedParser[String] =
    delimiter(end)
      .prevNot(invalidInsideDelimiter)
      .nextNot(invalidAfterEndMarkup)

  
  /** Parses a span of emphasized text.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#emphasis]]
   */
  lazy val em: SpanParserBuilder = SpanParser.recursive { implicit recParsers =>
    span(delimiter('*').prevNot('*'), delimiter("*").nextNot('*')).map(Emphasized(_))
  }.withLowPrecedence
  
  /** Parses a span of text with strong emphasis.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#strong-emphasis]]
   */
  lazy val strong: SpanParserBuilder = SpanParser.recursive { implicit recParsers =>
    val delim = literal("**")
    span(delim, delim).map(Strong(_))
  }

  private def span (start: PrefixedParser[String], 
                    end: PrefixedParser[String])(implicit recParsers: RecursiveSpanParsers): PrefixedParser[List[Span]]
    = markupStart(start, end) ~> recParsers.escapedText(delimitedBy(markupEnd(end))).map { text => List(Text(text)) }

  
  /** Parses an inline literal element.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#inline-literals]].
   */
  lazy val inlineLiteral: SpanParserBuilder = SpanParser.standalone {
    markupStart("``", "``") ~> delimitedBy(markupEnd("``")).map(Literal(_))
  }
  
  private def toSource (label: FootnoteLabel): String = label match {
    case Autonumber => "[#]_"
    case Autosymbol => "[*]_"
    case AutonumberLabel(name) => s"[#$name]_"
    case NumericLabel(name) => s"[$name]_"
  }
  
  /** Parses a footnote reference.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#footnote-references]].
   */
  lazy val footnoteRef: SpanParserBuilder = SpanParser.standalone {
    markupStart("[", "]_") ~> footnoteLabel <~ markupEnd("]_") ^^ { label => FootnoteReference(label, toSource(label)) }
  }
  
  /** Parses a citation reference.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#citation-references]].
   */
  lazy val citationRef: SpanParserBuilder = SpanParser.standalone {
    markupStart("[", "]_") ~> simpleRefName <~ markupEnd("]_") ^^ { label => CitationReference(label, s"[$label]_") }
  }
  
  /** Parses a substitution reference.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#substitution-references]].
   */
  lazy val substitutionRef: SpanParserBuilder = SpanParser.standalone {
    markupStart("|", "|") ~> simpleRefName >> { ref =>
      markupEnd("|__").as(GenericReference(List(SubstitutionReference(ref)), "", s"|$ref|__")) |
      markupEnd("|_").as(GenericReference(List(SubstitutionReference(ref)), ref, s"|$ref|_")) |
      markupEnd("|").as(SubstitutionReference(ref))
    }
  }
  
  /** Parses an inline internal link target.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#inline-internal-targets]]
   */
  lazy val internalTarget: SpanParserBuilder = SpanParser.recursive { recParsers =>
    markupStart("_`", "`") ~>
    recParsers.escapedText(delimitedBy(markupEnd("`")).nonEmpty).map { name => 
      val id = ReferenceName(name)
      Text(id.original, Id(id.normalized) + RstStyle.target) 
    }
  }
  
  /** Parses an interpreted text element with the role name as a prefix.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#interpreted-text]]
   */  
  lazy val interpretedTextWithRolePrefix: SpanParserBuilder = SpanParser.recursive { recParsers =>
    (markupStart(":", ":") ~> simpleRefName) ~ (":`" ~> recParsers.escapedText(delimitedBy(markupEnd("`")).nonEmpty)) ^^
      { case role ~ text => InterpretedText(role, text, s":$role:`$text`") }
  }
  
  /** Parses an interpreted text element with the role name as a suffix.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#interpreted-text]]
   */  
  def interpretedTextWithRoleSuffix (defaultTextRole: String): SpanParserBuilder = SpanParser.recursive { recParsers =>
    val textParser = markupStart("`", "`") ~> recParsers.escapedText(delimitedBy(markupEnd("`")).nonEmpty)
    val roleSuffix = opt(":" ~> simpleRefName <~ markupEnd(":"))
    (textParser ~ roleSuffix).map { case text ~ role => 
      InterpretedText(role.getOrElse(defaultTextRole), text, s"`$text`" + role.map(":"+_+":").getOrElse("")) }
  }.withLowPrecedence
  
  /** Parses a phrase link reference (enclosed in back ticks).
   *  
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#hyperlink-references]]
   */
  lazy val phraseLinkRef: SpanParserBuilder = SpanParser.recursive { recParsers =>
    
    def ref (refName: String, url: String) = if (refName.isEmpty) url else refName
    val urlPart = "<" ~> delimitedBy('>').map { _.replaceAll("[ \n]+", "") }
    val refName = recParsers.escapedText(delimitedBy('`','<').keepDelimiter).map(ReferenceName)
    val end = markupEnd("`__").as(false) | markupEnd("`_").as(true)
    
    (markupStart("`", "`") ~> refName ~ opt(urlPart) ~ end).withSource.map {
      case (name ~ Some(url) ~ true, src)   => SpanSequence(
        Link.create(List(Text(ref(name.original, url))), url, src), 
        LinkDefinition.create(ref(name.normalized, url), url)
      )
      case (name ~ Some(url) ~ false, src)  => Link.create(List(Text(ref(name.original, url))), url, src)
      case (name ~ None ~ true, src)        => GenericReference(List(Text(name.original)), name.normalized, src) 
      case (name ~ None ~ false, src)       => GenericReference(List(Text(name.original)), "", src) 
    }
  }
  
  /** Parses a simple link reference.
   *  
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#hyperlink-references]]
   */
  lazy val simpleLinkRef: SpanParserBuilder = SpanParser.standalone {
    markupEnd("__" | "_").flatMap { markup => 
      reverse(markup.length, simpleRefName <~ nextNot(invalidBeforeStartMarkup)).map { refName =>
        markup match {
          case "_"  => Reverse(refName.length, GenericReference(List(Text(refName)), ReferenceName(refName).normalized, s"${refName}_"), Text("_")) 
          case "__" => Reverse(refName.length, GenericReference(List(Text(refName)), "", s"${refName}__"), Text("__")) 
        }
      }
    } 
  }.withLowPrecedence
  
  private def reverse (offset: Int, p: => Parser[String]): Parser[String] = Parser { in =>
    p.parse(in.reverse.consume(offset)) match {
      case Success(result, _) => Success(result.reverse, in)
      case Failure(msg, _, _) => Failure(msg, in)
    }
  }
  
  private val autoLinks = new AutoLinkParsers(
    nextNot(invalidBeforeStartMarkup),
    nextNot(invalidAfterEndMarkup),
    Set('-',':','/','\'','(','{'),
    Set('-',':','/','\'',')','}','.',',',';','!','?')
  )

  /** Parses a standalone HTTP or HTTPS hyperlink (with no surrounding markup).
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#standalone-hyperlinks]]
   */
  lazy val uri: SpanParserBuilder = autoLinks.http
  
  /** Parses a standalone email address (with no surrounding markup).
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#standalone-hyperlinks]]
   */
  lazy val email: SpanParserBuilder = autoLinks.email

}
