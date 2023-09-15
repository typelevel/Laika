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

import laika.api.bundle.{ BlockParserBuilder, BlockPosition }
import laika.ast.*
import laika.parse.{ BlockSource, LineSource, Parser }
import laika.parse.markup.RecursiveParsers
import laika.parse.builders.*
import laika.parse.implicits.*
import laika.parse.text.{ PrefixedParser, WhitespacePreprocessor }

/** Provides all block parsers for Markdown text except for lists
  * which are factored out into a separate parser object
  * and those blocks dealing with verbatim HTML markup
  * which this library treats as an optional feature that has to be explicitly mixed in.
  *
  * Block parsers are only concerned with splitting the document into (potentially nested) blocks.
  * They are used in the first phase of parsing, while delegating to inline parsers for the 2nd phase.
  *
  * @author Jens Halm
  */
private[laika] object BlockParsers {

  /** Parses a single tab or space character.
    */
  val tabOrSpace: Parser[Unit] = oneOf(' ', '\t').void

  /** Parses up to 3 space characters.
    * In Markdown an indentation of up to 3 spaces is optional
    * and does not have any influence on the parsing logic.
    */
  val insignificantSpaces: Parser[Unit] = anyOf(' ').max(3).void

  /** Parses the decoration (underline) of a setext header.
    */
  private val setextDecoration: Parser[String] = (someOf('=') | someOf('-')) <~ wsEol

  /** Parses a single Markdown block. In contrast to the generic block parser of the
    * generic block parsers this method also consumes and ignores up to three optional space
    * characters at the start of each line.
    *
    * @param firstLinePrefix parser that recognizes the start of the first line of this block
    * @param linePrefix parser that recognizes the start of subsequent lines that still belong to the same block
    * @param nextBlockPrefix parser that recognizes whether a line after one or more blank lines still belongs to the same block
    */
  def mdBlock(
      firstLinePrefix: Parser[Any],
      linePrefix: Parser[Any],
      nextBlockPrefix: Parser[Any]
  ): Parser[BlockSource] = {
    block(firstLinePrefix, insignificantSpaces ~ linePrefix, nextBlockPrefix)
  }

  /** Parses a single Markdown block. In contrast to the `mdBlock` parser
    * this method also verifies that the second line is not a setext header decoration.
    *
    * @param firstLinePrefix parser that recognizes the start of the first line of this block
    * @param linePrefix parser that recognizes the start of subsequent lines that still belong to the same block
    * @param nextBlockPrefix parser that recognizes whether a line after one or more blank lines still belongs to the same block
    */
  private def decoratedBlock(
      firstLinePrefix: Parser[Any],
      linePrefix: Parser[Any],
      nextBlockPrefix: Parser[Any]
  ): Parser[BlockSource] = {
    val skipLine = anyNot('\n', '\r').void <~ eol
    val noHeader = lookAhead(skipLine ~ not(setextDecoration))
    mdBlock(noHeader ~ firstLinePrefix, linePrefix, nextBlockPrefix)
  }

  lazy val rootHeaderOrParagraph: BlockParserBuilder =
    BlockParserBuilder.recursive(headerOrParagraph(_, BlockPosition.RootOnly)).rootOnly

  lazy val nestedHeaderOrParagraph: BlockParserBuilder =
    BlockParserBuilder.recursive(headerOrParagraph(_, BlockPosition.NestedOnly)).nestedOnly

  private def headerOrParagraph(recParsers: RecursiveParsers, pos: BlockPosition): Parser[Block] = {

    val interruptions = recParsers.paragraphInterruptions(pos)
    val line          = not(blankLine) ~> restOfLine.line
    val lineAndCond   = interruptions.map(res => (Nil, Some(res))) | line.repUntil(interruptions)

    val decorationOrLines: Parser[Either[String, (Seq[LineSource], Option[Block])]] =
      setextDecoration.map { Left(_) } | lineAndCond.map { Right(_) }

    def decoratedHeaderLevel(decoration: String) = if (decoration.head == '=') 1 else 2

    /*  Merges the specified list of lines into a single string,
     *  while looking for lines ending with double spaces which (sadly) stand for a hard line break in Markdown.
     */
    def processLineBreaks(line: LineSource): LineSource =
      /* add a special sequence for hard line breaks so that the
       * inline parser does not have to stop at each space character */
      if (line.input.endsWith("  ")) LineSource(line.input.dropRight(2) ++ "\\\r", line.parent)
      else line

    def paragraph(firstLine: LineSource, restLines: Seq[LineSource]): Paragraph =
      Paragraph(
        recParsers.recursiveSpans.parseAndRecover(
          BlockSource(processLineBreaks(firstLine), restLines.map(processLineBreaks): _*)
        )
      )

    (textLine.line ~ decorationOrLines).map {
      case firstLine ~ Right((restLines, None))       => paragraph(firstLine, restLines)
      case firstLine ~ Right((restLines, Some(list))) =>
        BlockSequence(paragraph(firstLine, restLines), list)

      case text ~ Left(decoration) =>
        Header(decoratedHeaderLevel(decoration), recParsers.recursiveSpans.parseAndRecover(text))
    }
  }

  val linkTarget: BlockParserBuilder = BlockParserBuilder.withEscapedText { escapedParsers =>
    import escapedParsers._

    val id  = "[" ~> escapedUntil(']').map(_.toLowerCase) <~ ":" <~ ws.void
    val url =
      ("<" ~> escapedUntil('>')) | escapedText(delimitedBy(' ', '\n').acceptEOF.keepDelimiter)

    def enclosedBy(start: Char, end: Char) =
      start.toString ~> delimitedBy(end.toString <~ lookAhead(wsEol)).failOn('\r', '\n')

    val title =
      (ws.void ~ opt(eol) ~ ws.void) ~> (enclosedBy('"', '"') | enclosedBy('\'', '\'') | enclosedBy(
        '(',
        ')'
      ))

    (id ~ url ~ opt(title) <~ wsEol).mapN(LinkDefinition.create)
  }.rootOnly

  val atxHeader: BlockParserBuilder = BlockParserBuilder.recursive { recParsers =>
    def stripDecoration(text: String) = text.trim.reverse.dropWhile(_ == '#').reverse.trim

    val level = someOf('#').max(6).count
    val text  = recParsers.recursiveSpans(restOfLine.map(stripDecoration).line)

    (level ~ (not(blankLine) ~ ws ~> text)).mapN(Header(_, _))
  }

  val rules: BlockParserBuilder = BlockParserBuilder.standalone {
    val decoChar = oneOf('*', '-', '_')
    val pattern  = (decoChar ~ (anyOf(' ').void ~ decoChar).rep.min(2)).as(Rule())
    pattern <~ wsEol
  }

  val literalBlocks: BlockParserBuilder = BlockParserBuilder.standalone {
    val wsPreProcessor = WhitespacePreprocessor.forString
    PrefixedParser(' ', '\t') {
      decoratedBlock(tabOrSpace, tabOrSpace, tabOrSpace).map { lines =>
        LiteralBlock(wsPreProcessor(lines.input))
      }
    }
  }

  val quotedBlock: BlockParserBuilder = BlockParserBuilder.recursive { recParsers =>
    PrefixedParser('>') {
      val decoratedLine = ">" ~ ws.max(1).void
      recParsers
        .recursiveBlocks(
          decoratedBlock(decoratedLine, decoratedLine | not(blankLine), literal(">"))
        )
        .map(QuotedBlock(_, Nil))
    }
  }

  /** Parses just a plain paragraph after the maximum nest level has been reached.
    * This is necessary as a separate parser as the default markdown paragraph parser
    * is combined with potentially nested lists which makes that parser recursive.
    */
  val fallbackParagraph: BlockParserBuilder = BlockParserBuilder.withSpans { spanParsers =>
    val block = textLine.rep.min(1).map(_.mkString).line
    spanParsers.recursiveSpans(block).map(Paragraph(_))
  }.nestedOnly.withLowPrecedence

}
