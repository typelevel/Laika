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

package laika.asciidoc
import laika.bundle.{SpanParser, SpanParserBuilder}
import cats.data.NonEmptySet
import laika.parse.text.PrefixedParser
import laika.ast._

import laika.parse.{Failure, LineSource, Parser, Success}
import laika.parse.builders._
import laika.parse.implicits._
import laika.collection.TransitionalCollectionOps._
import laika.parse.markup.RecursiveParsers
import laika.parse.markup.RecursiveSpanParsers
import laika.parse.uri.AutoLinkParsers
import laika.asciidoc.ast.Mark

/** Provides all inline parsers for asciidoc.
 *  
 *  Inline parsers deal with markup within a block of text, such as a
 *  link or emphasized text. They are used in the second phase of parsing,
 *  after the block parsers have cut the document into a (potentially nested)
 *  block structure.
 *  
 * 
 *  @author i10416
 */
object InlineParsers {

    val escapedChar: Parser[String] = oneOf('\\','*','{','[','_')
    
    /**
      * explicitly invokes inline line break
      */
    val lineBreak:SpanParserBuilder = SpanParser.standalone {
      (literal(" +") <~wsEol ).map(_ => LineBreak())
    }
    
    
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
    
    
    /**
     * documented punctuations
     * 
     * see [[https://docs.asciidoctor.org/asciidoc/latest/text/#constrained]]
    */
    private val punctuations = NonEmptySet.of('.',',',';','!','?','"')
    /**
      *  
      *  Markup with single markup symbol(constrained format) is valid iff it is enclosed by specific characters.
      *
      *  Only these charactors can be located just before single markup characters.
      * 
      *  For example, `some(*text*)here` is valid strong, but `some*text*here` is not.
      * 
      *  On the other hand, markup with double markup symbols(unconstrained format) can be an be applied anywhere. 
      * 
      * e.g. both `some**text**here` and `some **text** here` are valid.
      */
    private val startChars = NonEmptySet.of(' ','\t','(',')','|','~','=','`','[',']','{','\n',':') ++ punctuations

    /**
      * Markup with single markup symbol (constrained format) is valid 
      * iff it is enclosed by space, specific characters,
      * or trailed by punctuation like `!`,`?` and `.`.
      * 
      *  Only these charactors can be located just after single markup characters.
      * 
      *  For example, `some(*text*)here` is valid strong format, but `some*text*here` is not.
      */
    private val endChars = NonEmptySet.of(' ','(',')','"','[',']','}',':') ++ punctuations
    

    private val invalidBeforeStartMarkup: Char => Boolean =
        char => !startChars.contains(char)

    private val invalidAfterEndMarkup: Char => Boolean = char =>
        !endChars.contains(char)

    /**  
    * applies to the character after the single start delimiter and before the single end delimiter.
    * 
    * - ` `: space char
    * - `\n`: line break char
    * 
    * For example, `some _text_ here` is valid emphasis in asciidoc, but `some _ text _ here` is not.
    * 
    * On the other hand, markup with double delimiters  like `some__ text __here` is valid emphasis.
    */
    private val invalidInsideConstrainedDelimiter: NonEmptySet[Char] = NonEmptySet.of(' ','\n')
  

   /** Parses the markup at the start of an inline element with single markup symbol(constrained markup).
    *  
    *  Constrained markup should be enclosed by space and should not have space just inside itself.
    *  For example, `some _text_ here` is valid, but `some _ text_ here`, `some_text_here` and `some _ text _ here` are invalid.
    *  
    *  See [[---]].
    *
    *  @param start the parser that recognizes the markup at the start of an inline element
    *  @param end the parser that recognizes the markup at the end of an inline element, needed to verify
    *  the start sequence is not immediately followed by an end sequence as empty elements are not allowed.
    *  @return a parser without a useful result, as it is only needed to verify it succeeds
    */
  private def constrainedMarkupStart (start: Char, end: Char): PrefixedParser[Any] = {
    delimiter(start)
      .prevNot(invalidBeforeStartMarkup)
      .nextNot(invalidInsideConstrainedDelimiter)
      .nextNot(start)
      .notEnclosedBy(invalidPair)~ not(s"$end")
  }


  private  def invalidPair (before: Char, after: Char): Boolean = pairs.getOrElse(before, Set()).contains(after)



  def constrainedMarkupEnd (end: Char): PrefixedParser[String] = delimiter(end).prevNot(invalidInsideConstrainedDelimiter).nextNot(invalidAfterEndMarkup)



  lazy val mark:SpanParserBuilder = SpanParser.recursive {implicit recParsers =>
    constrainedMarkup('#','#').orElse(unconstrainedMarkupEnclosure('#')).map(Mark(_))
  }

  lazy val em: SpanParserBuilder  = SpanParser.recursive{ implicit recParsers =>
    constrainedMarkup('_','_').orElse(unconstrainedMarkupEnclosure('_')).map(Emphasized(_))
  }

  lazy val strong : SpanParserBuilder = SpanParser.recursive{ implicit recParsers =>
    constrainedMarkup('*','*').orElse(unconstrainedMarkupEnclosure('*')).map{Strong(_)} 
  }

  lazy val inlineLiteral:SpanParserBuilder  = SpanParser.standalone{
    someOf('`').count >> {cnt =>
      delimitedBy("`"*cnt).trim.map(Literal(_))  
    }
  }

 
  private def constrainedMarkup(start:Char,end:Char)(implicit recParsers: RecursiveSpanParsers):PrefixedParser[List[Span]] =
    constrainedMarkupStart(start,end) ~> recParsers.recursiveSpans(delimitedBy(constrainedMarkupEnd(end)))


  /** Parses a span enclosed by two consecutive occurrences of the specified character.
   *  Recursively parses nested spans, too. 
   */
  def unconstrainedMarkupEnclosure (c: Char)(implicit recParsers: RecursiveSpanParsers): PrefixedParser[List[Span]] = {
    val start = delimiter(s"$c$c").nextNot('\n')
    val end   = delimiter(s"$c$c")
    start ~> recParsers.recursiveSpans(delimitedBy(end))  
   }
    
  private val autoLinks = new AutoLinkParsers(
    nextNot(invalidBeforeStartMarkup),
    nextNot(invalidAfterEndMarkup),
    Set('-',':','/','\'','(','{'),
    Set('-',':','/','\'',')','}','.',',',';','!','?')
  )
    /** Parses a standalone HTTP or HTTPS hyperlink (with no surrounding markup).
   * 
   */
  lazy val uri: SpanParserBuilder = autoLinks.http
  
  /** Parses a standalone email address (with no surrounding markup).
   * 
   */
  lazy val email: SpanParserBuilder = autoLinks.email


}
