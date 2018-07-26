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

package laika.parse.markdown

import laika.api.ext.{BlockParser, BlockParserBuilder}
import laika.parse.core.Parser
import laika.parse.core.markup.BlockParsers._
import laika.parse.core.markup.RecursiveParsers
import laika.parse.core.text.TextParsers
import laika.parse.core.text.TextParsers._
import laika.parse.util.WhitespacePreprocessor
import laika.tree.Elements._
import laika.util.~


/** Provides all block parsers for Markdown text except for those dealing
 *  with verbatim HTML markup which this library treats as an optional 
 *  feature that has to be explicitly mixed in.
 *  
 *  Block parsers are only concerned with splitting the document into 
 *  (potentially nested) blocks. They are used in the first phase of parsing,
 *  while delegating to inline parsers for the 2nd phase.
 * 
 *  @author Jens Halm
 */
object BlockParsers {


  /** Parses a single tab or space character.
   */
  val tabOrSpace: Parser[Unit] = anyOf(' ','\t').take(1).noCapture

  /** Parses up to 3 space characters. In Markdown an indentation
   *  of up to 3 spaces is optional and does not have any influence
   *  on the parsing logic.
   */
  val insignificantSpaces: Parser[Unit] = anyOf(' ').max(3).noCapture

  val setextDecoration: Parser[String] = (anyOf('=').min(1) | anyOf('-').min(1)) <~ wsEol

  /**  Parses a single Markdown block. In contrast to the generic block parser of the
    *  generic block parsers this method also consumes and ignores up to three optional space
    *  characters at the start of each line.
    *
    *  @param firstLinePrefix parser that recognizes the start of the first line of this block
    *  @param linePrefix parser that recognizes the start of subsequent lines that still belong to the same block
    *  @param nextBlockPrefix parser that recognizes whether a line after one or more blank lines still belongs to the same block
    */
  def mdBlock (firstLinePrefix: Parser[Any], linePrefix: Parser[Any], nextBlockPrefix: Parser[Any]): Parser[String] = {
    block(firstLinePrefix, insignificantSpaces ~ linePrefix, nextBlockPrefix)
  }

  def decoratedBlock (firstLinePrefix: Parser[Any], linePrefix: Parser[Any], nextBlockPrefix: Parser[Any]): Parser[String] = {
    val skipLine = anyBut('\n','\r').^ <~ eol
    val noHeader = lookAhead(skipLine ~ not(setextDecoration))
    mdBlock(noHeader ~ firstLinePrefix, linePrefix, nextBlockPrefix)
  }

  lazy val rootHeaderOrParagraph: BlockParserBuilder = BlockParser.withoutStartChar.recursive { implicit recParsers =>
    val lineCondition = not(blankLine)
    val listWithoutBlankline = success(None)
    headerOrParagraph(lineCondition, listWithoutBlankline)
  }.rootOnly

  lazy val nestedHeaderOrParagraph: BlockParserBuilder = BlockParser.withoutStartChar.recursive { implicit recParsers =>

    val lineCondition = not(ListParsers.bulletListItemStart | ListParsers.enumListItemStart | blankLine)

    val listParsers = (ListParsers.bulletLists ++ ListParsers.enumLists)
      .map(_.createParser(recParsers).fullParser)
      .reduceLeft(_ | _)

    /**  Markdown allows nested lists without preceding blank lines,
      *  therefore will detect list items in the middle of a paragraph,
      *  whereas a top level paragraph won't do that. One of the questionable
      *  Markdown design decisions.
      */
    val listWithoutBlankline = opt(not(blankLine) ~> listParsers)
    headerOrParagraph(lineCondition, listWithoutBlankline)
  }.nestedOnly

  def headerOrParagraph (lineCondition: Parser[Any], listWithoutBlankline: Parser[Option[Block]])
                        (implicit recParsers: RecursiveParsers) : Parser[Block] = {

      val lines = (lineCondition ~> restOfLine) *

      val decorationOrLines: Parser[Either[String, List[String] ~ Option[Block]]] =
        (setextDecoration ^^ { Left(_) }) | ((lines ~ listWithoutBlankline) ^^ { Right(_) })

      def decoratedHeaderLevel (decoration: String) = if (decoration.head == '=') 1 else 2

      /**  Merges the specified list of lines into a single string,
        *  while looking for lines ending with double spaces which
        *  (sadly) stand for a hard line break in Markdown.
        */
      def processLineBreaks(lines: List[String]): String =
        lines.map { line =>
          /* add a special sequence for hard line breaks so that the
           * inline parser does not have to stop at each space character */
          if (line.endsWith("  ")) line.dropRight(2) ++ "\\\r"
          else line
        }.mkString("\n")

      def paragraph (parser: String => List[Span], firstLine: String, restLines: List[String]): Paragraph =
        Paragraph(parser(processLineBreaks(firstLine +: restLines)))

      (recParsers.withRecursiveSpanParser(textLine) ~ decorationOrLines) ^^ {
        case (parser, firstLine) ~ Right(restLines ~ None)       => paragraph(parser, firstLine, restLines)
        case (parser, firstLine) ~ Right(restLines ~ Some(list)) => BlockSequence(Seq(paragraph(parser, firstLine, restLines), list))
        case (parser, text) ~      Left(decoration)              => Header(decoratedHeaderLevel(decoration), parser(text))
      }
    }

  /** Parses a link definition in the form `[id]: <url> "title"`.
    * The title is optional as well as the quotes around it and the angle brackets around the url.
    */
  val linkTarget: BlockParserBuilder = BlockParser.forStartChar('[').withEscapedText { escapedParsers =>

    import escapedParsers._

    val id = escapedUntil(']') <~ ':' <~ ws.^
    val url = (('<' ~> escapedUntil('>')) | escapedText(delimitedBy(' ', '\n').acceptEOF.keepDelimiter)) ^^ { _.mkString }

    def enclosedBy(start: Char, end: Char) =
      start ~> delimitedBy(end.toString, lookAhead(wsEol)).failOn('\r', '\n') ^^ { _.mkString }

    val title = (ws.^ ~ opt(eol) ~ ws.^) ~> (enclosedBy('"', '"') | enclosedBy('\'', '\'') | enclosedBy('(', ')'))

    id ~ url ~ opt(title) <~ wsEol ^^ { case id ~ url ~ title => ExternalLinkDefinition(id.toLowerCase, url, title) }
  }.rootOnly

  /** Parses an ATX header, a line that starts with 1 to 6 `'#'` characters,
   *  with the number of hash characters corresponding to the level of the header.
   *  Markdown also allows to decorate the line with trailing `'#'` characters which
   *  this parser will remove.
   */
  val atxHeader: BlockParserBuilder = BlockParser.forStartChar('#').recursive { recParsers =>
    def stripDecoration (text: String) = {
      val trimmed = text.trim 
      if (trimmed.last == '#') trimmed.take(trimmed.lastIndexWhere(_ != '#') + 1).trim
      else trimmed
    } 
    
    anyOf('#').max(5).count ~ (not(blankLine) ~> recParsers.recursiveSpans(restOfLine ^^ stripDecoration)) ^^ {
      case level ~ spans => Header(level + 1, spans)
    }
  }

  /** Parses a horizontal rule, a line only decorated with three or more `'*'`, `'-'` or `'_'`
   *  characters with optional spaces between them
   */
  val rules: Seq[BlockParserBuilder] = Seq('*', '-', '_').map { decoChar =>
    BlockParser.forStartChar(decoChar).standalone {
      val pattern = (anyOf(' ').^ ~ decoChar).rep.min(2)
      pattern ~ wsEol ^^^ Rule()
    }
  }

  /** Parses a literal block, text indented by a tab or 4 spaces.
   */
  val literalBlocks: Seq[BlockParserBuilder] = Seq(' ', '\t').map { startChar =>
    val wsPreProcessor = new WhitespacePreprocessor
    BlockParser.forStartChar(startChar).standalone {
      decoratedBlock(success(()), tabOrSpace, tabOrSpace) ^^ { lines => LiteralBlock(wsPreProcessor(lines)) }
    }
  }

  /** Parses a quoted block, a paragraph starting with a `'>'` character,
   *  with subsequent lines optionally starting with a `'>'`, too.
   */
  val quotedBlock: BlockParserBuilder = BlockParser.forStartChar('>').recursive { recParsers =>
    val textAfterDeco = ws.max(1).noCapture
    val decoratedLine = '>' ~ textAfterDeco
    recParsers.recursiveBlocks(decoratedBlock(textAfterDeco, decoratedLine | not(blankLine), '>')) ^^ (QuotedBlock(_, Nil))
  }

  /** Parses just a plain paragraph after the maximum nest level has been reached.
    * This is necessary as a separate parser as the default markdown paragraph parser
    * is combined with potentially nested lists which makes that parser recursive.
    */
  val fallbackParagraph: BlockParserBuilder = BlockParser.withoutStartChar.withSpans { spanParsers =>
    val block: Parser[String] = TextParsers.textLine.rep.min(1) ^^ (_.mkString)
    spanParsers.recursiveSpans(block).map(Paragraph(_))
  }.nestedOnly.withLowPrecedence


}
