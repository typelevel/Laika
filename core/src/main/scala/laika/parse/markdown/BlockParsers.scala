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

import laika.parse.core.Parser
import laika.parse.core.markup.BlockParsers._
import laika.parse.core.markup.RecursiveParsers
import laika.parse.core.text.TextParsers._
import laika.parse.util.WhitespacePreprocessor
import laika.tree.Elements._
import laika.util.~

import scala.collection.mutable
 

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
class BlockParsers (recParsers: RecursiveParsers) {


  import recParsers._


  /** Parses a single tab or space character.
   */
  val tabOrSpace: Parser[Unit] = anyOf(' ','\t').take(1).noCapture

  /** Parses up to 3 space characters. In Markdown an indentation
   *  of up to 3 spaces is optional and does not have any influence
   *  on the parsing logic.
   */
  val insignificantSpaces: Parser[Unit] = anyOf(' ').max(3).noCapture

  private val processWS = new WhitespacePreprocessor


  /** Parses the start of a bullet list item.
    */
  val bulletListItemStart: Parser[String] = anyOf('*','-','+').take(1) <~ anyOf(' ', '\t').min(1).^

  /** Parses the start of an enumerated list item.
    */
  val enumListItemStart: Parser[String] = anyIn('0' to '9').min(1) <~ '.' ~ anyOf(' ', '\t').min(1).^
  
  /** Parses a link definition in the form `[id]: <url> "title"`.
    *  The title is optional as well as the quotes around it and the angle brackets around the url.
    */
  val linkTarget: Parser[ExternalLinkDefinition] = {

    val id = '[' ~> escapedUntil(']') <~ ':' <~ ws.^
    val url = (('<' ~> escapedUntil('>')) | escapedText(delimitedBy(' ', '\n').acceptEOF.keepDelimiter)) ^^ { _.mkString }

    def enclosedBy(start: Char, end: Char) =
      start ~> delimitedBy(end.toString, lookAhead(wsEol)).failOn('\r', '\n') ^^ { _.mkString }

    val title = (ws.^ ~ opt(eol) ~ ws.^) ~> (enclosedBy('"', '"') | enclosedBy('\'', '\'') | enclosedBy('(', ')'))

    id ~ url ~ opt(title) <~ wsEol ^^ { case id ~ url ~ title => ExternalLinkDefinition(id.toLowerCase, url, title) }
  }

  /** Parses a single Markdown block. In contrast to the generic block parser of the
    *  super-trait this method also consumes and ignores up to three optional space
    *  characters at the start of each line.
    *
    *  @param firstLinePrefix parser that recognizes the start of the first line of this block
    *  @param linePrefix parser that recognizes the start of subsequent lines that still belong to the same block
    *  @param nextBlockPrefix parser that recognizes whether a line after one or more blank lines still belongs to the same block
    */
  def mdBlock (firstLinePrefix: Parser[Any], linePrefix: Parser[Any], nextBlockPrefix: Parser[Any]): Parser[String] = {
    block(firstLinePrefix, insignificantSpaces ~ linePrefix, nextBlockPrefix)
  }

  class ParserMapBuilder {

    val map = new mutable.HashMap[Char, Parser[Block]]

    def add (p: Parser[Block], chars: Char*): this.type = {
      for (char <- chars) map.put(char, p)
      this
    }

  }

  lazy val nestedBlockParserMap: Map[Char, Parser[Block]] = {
    (new ParserMapBuilder)
      .add(atxHeader, '#')
      .add(literalBlock, '\t', ' ')
      .add(quotedBlock, '>')
      .add(rule, '_')
      .add(bulletList, '+')
      .add(rule | bulletList, '*', '-')
      .add(enumList, ('0' to '9'):_*)
      .map.toMap
  }

  lazy val rootBlockParserMap: Map[Char, Parser[Block]] = nestedBlockParserMap + ('[' -> linkTarget)

  lazy val nonRecursiveParserMap: Map[Char, Parser[Block]] = {
    (new ParserMapBuilder)
      .add(atxHeader, '#')
      .add(literalBlock, '\t', ' ')
      .add(rule, '*', '-', '_')
      .map.toMap
  }

  /** Merges the specified list of lines into a single string,
    *  while looking for lines ending with double spaces which
    *  stand for a hard line break in Markdown.
    */
  private def processLineBreaks(lines: List[String]): String = {

    var first = true
    val builder = new mutable.StringBuilder

    for (line <- lines) {
      if (first) first = false
      else builder append "\n"

      /* add a special sequence for hard line breaks so that the
       * inline parser does not have to stop at each space character */
      if (line.endsWith("  ")) builder append line.dropRight(2) append "\\\r"
      else builder append line
    }

    builder.toString

  }

  lazy val rootHeaderOrParagraph: Parser[Block] = {

    val lines = (not(blankLine) ~> restOfLine) *

    val decorationOrLines: Parser[Either[String, List[String]]] =
      (setextDecoration ^^ { Left(_) }) | (lines ^^ { Right(_) })

    (withRecursiveSpanParser(textLine) ~ decorationOrLines) ^^ {
      case (parser, firstLine) ~ Right(restLines)                      => Paragraph(parser(processLineBreaks(firstLine +: restLines)))
      case (parser, text) ~ Left(decoration) if decoration.head == '=' => Header(1, parser(text))
      case (parser, text) ~ Left(_)                                    => Header(2, parser(text))
    }
  }

  lazy val nestedHeaderOrParagraph: Parser[Block] = {

    val lines = (not(bulletListItemStart | enumListItemStart | blankLine) ~> restOfLine) *

    /** Parses a single paragraph nested inside another block.
      *  Markdown allows nested lists without preceding blank lines,
      *  therefore will detect list items in the middle of a paragraph,
      *  whereas a top level paragraph won't do that. One of the questionable
      *  Markdown design decisions.
      */
    val decorationOrLines: Parser[Either[String, List[String] ~ Option[Block]]] =
      (setextDecoration ^^ { Left(_) }) | (lines ~ opt(not(blankLine) ~> (bulletList | enumList)) ^^ { Right(_) })

    (withRecursiveSpanParser(textLine) ~ decorationOrLines) ^^ {
      case (parser, firstLine) ~ Right(restLines ~ None)               => Paragraph(parser(processLineBreaks(firstLine +: restLines)))
      case (parser, firstLine) ~ Right(restLines ~ Some(list))         =>
        BlockSequence(Seq(Paragraph(parser(processLineBreaks(firstLine +: restLines))), list))
      case (parser, text) ~ Left(decoration) if decoration.head == '=' => Header(1, parser(text))
      case (parser, text) ~ Left(_)                                    => Header(2, parser(text))
    }
  }

  def markdownBlocks (decoratedBlocks: Map[Char, Parser[Block]], undecoratedBlock: Parser[Block]): Parser[Block] = {

    val startChars = anyOf(decoratedBlocks.keySet.toSeq:_*).take(1)

    val skipLine = anyBut('\n','\r').^ <~ eol

    val decoratedBlock = lookAhead(startChars <~ skipLine ~ not(setextDecoration)) >> { startChar =>
      decoratedBlocks(startChar.charAt(0))
    }

    insignificantSpaces ~> (decoratedBlock | undecoratedBlock)
  }

  lazy val rootMarkdownBlock: Parser[Block] = markdownBlocks(rootBlockParserMap, rootHeaderOrParagraph)

  lazy val nestedMarkdownBlock: Parser[Block] = markdownBlocks(nestedBlockParserMap, nestedHeaderOrParagraph)

  lazy val nonRecursiveMarkdownBlock: Parser[Block] = markdownBlocks(nonRecursiveParserMap, nestedHeaderOrParagraph)



  /** Parses an ATX header, a line that starts with 1 to 6 `'#'` characters,
   *  with the number of hash characters corresponding to the level of the header.
   *  Markdown also allows to decorate the line with trailing `'#'` characters which
   *  this parser will remove.
   */
  lazy val atxHeader: Parser[Header] = {
    def stripDecoration (text: String) = {
      val trimmed = text.trim 
      if (trimmed.last == '#') trimmed.take(trimmed.lastIndexWhere(_ != '#') + 1).trim
      else trimmed
    } 
    
    anyOf('#').min(1).max(6).count ~ (not(blankLine) ~> recursiveSpans(restOfLine ^^ stripDecoration)) ^^ {
      case level ~ spans => Header(level, spans)
    }
  }

  val setextDecoration: Parser[String] = (anyOf('=').min(1) | anyOf('-').min(1)) <~ wsEol


  /** Parses a horizontal rule, a line only decorated with three or more `'*'`, `'-'` or `'_'`
   *  characters with optional spaces between them
   */
  lazy val rule: Parser[Block] = {
    def pattern (c: Char) = c ~ (anyOf(' ').^ ~ c).rep.min(2)
    (pattern('*') | pattern('-') | pattern('_')) ~ wsEol ^^^ { Rule() }
  }

  /** Parses a literal block, text indented by a tab or 4 spaces.
   */
  val literalBlock: Parser[LiteralBlock] = {
    mdBlock(tabOrSpace, tabOrSpace, tabOrSpace) ^^ { lines => LiteralBlock(processWS(lines)) }
  }

  /** Parses a quoted block, a paragraph starting with a `'>'` character,
   *  with subsequent lines optionally starting with a `'>'`, too.
   */
  lazy val quotedBlock: Parser[QuotedBlock] = {
    val decoratedLine = '>' ~ ws.max(1).noCapture
    recursiveBlocks(mdBlock(decoratedLine, decoratedLine | not(blankLine), '>')) ^^ (QuotedBlock(_, Nil))
  }

  /** Parses a list based on the specified helper parsers.
   * 
   *  @param itemStart parser that recognizes the start of a list item, result will be discarded
   *  @param newList function that produces a block element for the document tree
   *  @param newItem function that produces a new list item element based on position and content arguments
   */
  def list [T <: Block, I <: ListItem] (itemStart: Parser[String], 
                                        newList: List[ListItem] => T, 
                                        newItem: (Int, Seq[Block]) => I): Parser[T] = {
    
    def flattenItems (items: List[~[Option[Any], Seq[Block]]]) = {
      val hasBlankLines = items exists { 
        case Some(_) ~ _  => true
        case None ~ _     => false
      }
      def rewriteItemContent (blocks: Seq[Block], pos: Int) = {
        val rewritten = blocks match {
          /* Promoting Paragraph to ForcedParagraph if the list has any blank lines 
             between list items or if it is adjacent to blank lines within the list item 
             itself. This is ugly, but forced by the (in this respect odd) design of Markdown. */
          case Paragraph(content,opt) :: Nil if hasBlankLines => 
            ForcedParagraph(content, opt) :: Nil
          case BlockSequence((p @ Paragraph(content,opt)) :: rest, _) :: xs => 
            if (!hasBlankLines) SpanSequence(content,opt) :: rest ::: xs else p :: rest ::: xs
          case other => other
        }
        
        newItem(pos,rewritten)
      } 
      val pos = Stream.from(1).iterator
      items map { item => rewriteItemContent(item._2, pos.next) } 
    }
    
    lookAhead(itemStart) ~> ((opt(blankLines) ~ listItem(itemStart)) *) ^^
      { x => newList(flattenItems(x)) }
  }
  
  /** Parses a single list item.
   * 
   *  @param itemStart parser that recognizes the start of a list item, result will be discarded
   */
  def listItem [I <: ListItem] (itemStart: Parser[String]): Parser[Seq[Block]]
    = recursiveBlocks(mdBlock(
        not(rule) ~ itemStart, not(blankLine | itemStart) ~ opt(tabOrSpace), tabOrSpace
      ))
 
  /** Parses a bullet list, called "unordered list" in the Markdown syntax description.
   */
  lazy val bulletList: Parser[BulletList] = {
    lookAhead(bulletListItemStart) >> { symbol =>
      val bullet = StringBullet(symbol)
      list(bulletListItemStart, BulletList(_, bullet), (_, blocks) => BulletListItem(blocks, bullet))
    }
  }
    
  /** Parses an enumerated list, called "ordered list" in the Markdown syntax description.
   */
  lazy val enumList: Parser[EnumList] = {
    list(enumListItemStart, EnumList(_, EnumFormat()), (pos, blocks) => EnumListItem(blocks, EnumFormat(), pos))
  }
    
  
}
