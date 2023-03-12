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
import laika.bundle.{ BlockParser, BlockParserBuilder, BlockParserBuilderOps, BlockPosition }
import laika.parse.{ BlockSource, LineSource, Parser }
import laika.parse.markup.RecursiveParsers
import laika.parse.builders._
import laika.parse.implicits._
import laika.parse.text.{ PrefixedParser, WhitespacePreprocessor }

/** Provides all block parsers for Markdown text except for for lists which
  *  are factored out into a separate parser object and those blocks dealing
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
  val tabOrSpace: Parser[Unit] = oneOf(' ', '\t').void

  /** Parses up to 3 space characters. In Markdown an indentation
    *  of up to 3 spaces is optional and does not have any influence
    *  on the parsing logic.
    */
  val insignificantSpaces: Parser[Unit] = anyOf(' ').max(3).void

  /** Parses the decoration (underline) of a setext header.
    */
  val setextDecoration: Parser[String] = (someOf('=') | someOf('-')) <~ wsEol

  /**  Parses a single Markdown block. In contrast to the generic block parser of the
    *  generic block parsers this method also consumes and ignores up to three optional space
    *  characters at the start of each line.
    *
    *  @param firstLinePrefix parser that recognizes the start of the first line of this block
    *  @param linePrefix parser that recognizes the start of subsequent lines that still belong to the same block
    *  @param nextBlockPrefix parser that recognizes whether a line after one or more blank lines still belongs to the same block
    */
  def mdBlock(
      firstLinePrefix: Parser[Any],
      linePrefix: Parser[Any],
      nextBlockPrefix: Parser[Any]
  ): Parser[BlockSource] = {
    block(firstLinePrefix, insignificantSpaces ~ linePrefix, nextBlockPrefix)
  }

  /**  Parses a single Markdown block. In contrast to the `mdBlock` parser
    *  this method also verifies that the second line is not a setext header
    *  decoration.
    *
    *  @param firstLinePrefix parser that recognizes the start of the first line of this block
    *  @param linePrefix parser that recognizes the start of subsequent lines that still belong to the same block
    *  @param nextBlockPrefix parser that recognizes whether a line after one or more blank lines still belongs to the same block
    */
  def decoratedBlock(
      firstLinePrefix: Parser[Any],
      linePrefix: Parser[Any],
      nextBlockPrefix: Parser[Any]
  ): Parser[BlockSource] = {
    val skipLine = anyNot('\n', '\r').void <~ eol
    val noHeader = lookAhead(skipLine ~ not(setextDecoration))
    mdBlock(noHeader ~ firstLinePrefix, linePrefix, nextBlockPrefix)
  }

  /** Parses either a setext header, or a plain paragraph if the second line of the block
    * is not a setext header decoration.
    * Only used for root level blocks where lists starting in the middle of a paragraph are not allowed.
    */
  lazy val rootHeaderOrParagraph: BlockParserBuilder =
    BlockParser.recursive(headerOrParagraph(_, BlockPosition.RootOnly)).rootOnly

  /** Parses either a setext header, or a plain paragraph if the second line of the block
    * is not a setext header decoration.
    * Only used for nested blocks where lists starting in the middle of a paragraph are allowed.
    */
  lazy val nestedHeaderOrParagraph: BlockParserBuilder =
    BlockParser.recursive(headerOrParagraph(_, BlockPosition.NestedOnly)).nestedOnly

  private def headerOrParagraph(recParsers: RecursiveParsers, pos: BlockPosition): Parser[Block] = {

    val interruptions = recParsers.paragraphInterruptions(pos)
    val line          = not(blankLine) ~> restOfLine.line
    val lineAndCond   = interruptions.map(res => (Nil, Some(res))) | line.repUntil(interruptions)

    val decorationOrLines: Parser[Either[String, (Seq[LineSource], Option[Block])]] =
      setextDecoration.map { Left(_) } | lineAndCond.map { Right(_) }

    def decoratedHeaderLevel(decoration: String) = if (decoration.head == '=') 1 else 2

    /**  Merges the specified list of lines into a single string,
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

  /** Parses a link definition in the form `[id]: <url> "title"`.
    * The title is optional as well as the quotes around it and the angle brackets around the url.
    */
  val linkTarget: BlockParserBuilder = BlockParser.withEscapedText { escapedParsers =>
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

  /** Parses an ATX header, a line that starts with 1 to 6 `'#'` characters,
    *  with the number of hash characters corresponding to the level of the header.
    *  Markdown also allows to decorate the line with trailing `'#'` characters which
    *  this parser will remove.
    */
  val atxHeader: BlockParserBuilderOps = BlockParser.recursive { recParsers =>
    def stripDecoration(text: String) = text.trim.reverse.dropWhile(_ == '#').reverse.trim

    val level = someOf('#').max(6).count
    val text  = recParsers.recursiveSpans(restOfLine.map(stripDecoration).line)

    (level ~ (not(blankLine) ~ ws ~> text)).mapN(Header(_, _))
  }

  /** Parses a horizontal rule, a line only decorated with three or more `'*'`, `'-'` or `'_'`
    *  characters with optional spaces between them
    */
  val rules: BlockParserBuilder = BlockParser.standalone {
    val decoChar = oneOf('*', '-', '_')
    val pattern  = (decoChar ~ (anyOf(' ').void ~ decoChar).rep.min(2)).as(Rule())
    pattern <~ wsEol
  }

  /** Parses a literal block, text indented by a tab or 4 spaces.
    */
  val literalBlocks: BlockParserBuilder = BlockParser.standalone {
    val wsPreProcessor = new WhitespacePreprocessor
    PrefixedParser(' ', '\t') {
      decoratedBlock(tabOrSpace, tabOrSpace, tabOrSpace).map { lines =>
        LiteralBlock(wsPreProcessor(lines.input))
      }
    }
  }

  /** Parses a quoted block, a paragraph starting with a `'>'` character,
    *  with subsequent lines optionally starting with a `'>'`, too.
    */
  val quotedBlock: BlockParserBuilder = BlockParser.recursive { recParsers =>
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
  val fallbackParagraph: BlockParserBuilder = BlockParser.withSpans { spanParsers =>
    val block = textLine.rep.min(1).map(_.mkString).line
    spanParsers.recursiveSpans(block).map(Paragraph(_))
  }.nestedOnly.withLowPrecedence

}
