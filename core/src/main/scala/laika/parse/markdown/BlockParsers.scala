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
import laika.parse.core.combinator.Parsers._
import laika.parse.util.WhitespacePreprocessor
import laika.tree.Elements._
import laika.util.~

import scala.collection.mutable.StringBuilder
 

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
trait BlockParsers extends laika.parse.BlockParsers { self: InlineParsers => 
  

  
  /** Parses a single tab or space character.
   */
  val tabOrSpace: Parser[Any] = anyOf(' ','\t') take 1

  /** Parses 0 or 1 space character, always succeeds.
   */
  val optSpace: Parser[String] = anyOf(' ') max 1
  
  /** Parses up to 3 space characters. In Markdown an indentation
   *  of up to 3 spaces is optional and does not have any influence
   *  on the parsing logic.
   */
  val insignificantSpaces: Parser[String] = anyOf(' ') max 3

  private val processWS = new WhitespacePreprocessor


  /** Parses the start of a bullet list item.
    */
  val bulletListItemStart: Parser[String] = anyOf('*','-','+').take(1) <~ (anyOf(' ','\t') min 1)

  /** Parses the start of an enumerated list item.
    */
  val enumListItemStart: Parser[String] = anyIn('0' to '9').min(1) <~ '.' ~ (anyOf(' ','\t') min 1)
  
  
  /** Merges the specified list of lines into a single string,
   *  while looking for lines ending with double spaces which
   *  stand for a hard line break in Markdown.
   */
  def linesToString (lines: List[String]): String = {

    val (builder, _) = ((new StringBuilder, true) /: lines) { (acc, line) => 
      val (b, first) = acc
      if (!first) b ++= "\n"
      /* add a special sequence for hard line breaks so that the
       * inline parser does not have to stop at each space character */
      if (line.endsWith("  ")) b ++= line.dropRight(2) ++= "\\\r"
      else b ++= line
      (b, false)
    }
    builder.toString
  }
  
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
    
    ((anyOf('#') min 1 max 6) ^^ { _.length }) ~ (not(blankLine) ~> recursiveSpans(restOfLine ^^ stripDecoration)) ^^
      { case level ~ spans => Header(level, spans) }
  }
  
  /** Parses a 1st or 2nd level Setext header. A first level header consists of the
   *  text of the header followed by a line of one or more `'='` characters, a 2nd
   *  level header uses `'-'` characters instead.
   *  
   *  In contrast to several other Markdown parsers this parser requires a blank line
   *  before the header. 
   */
  lazy val setextHeader: Parser[Header] = recursiveSpans(textLine) ~ (anyOf('=').min(1) | anyOf('-').min(1)) <~ wsEol ^^ {
    case spans ~ decoration if decoration.head == '=' => Header(1, spans)
    case spans ~ _                                    => Header(2, spans)
  }
  
  /** Parses a horizontal rule, a line only decorated with three or more `'*'`, `'-'` or `'_'`
   *  characters with optional spaces between them
   */
  lazy val rule: Parser[Block] = {
    def pattern (c: Char) = c ~ repMin(2, anyOf(' ') ~ c)
    (pattern('*') | pattern('-') | pattern('_')) ~ wsEol ^^^ { Rule() }
  }


  /** Parses all of the standard Markdown blocks, except normal paragraphs and those blocks
   *  that deal with verbatim HTML. For the latter parsers are provided by a separate, optional trait.
   */
  lazy val standardMarkdownBlock: Parser[Block] =
    atxHeader | setextHeader | (insignificantSpaces ~> 
      (literalBlock | quotedBlock | rule | bulletList | enumList))

  protected def prepareBlockParsers (nested: Boolean): List[Parser[Block]] = {
    if (nested) standardMarkdownBlock :: nestedParagraph :: Nil
    else standardMarkdownBlock :: (insignificantSpaces ~> linkTarget) :: paragraph :: Nil
  }
 
  lazy val nonRecursiveBlock: Parser[Block] =
    atxHeader | setextHeader | (insignificantSpaces ~> (literalBlock | rule )) | paragraph

  /** Parses a single paragraph. Everything between two blank lines that is not
   *  recognized as a special Markdown block type will be parsed as a regular paragraph.
   */
  val paragraph: Parser[Paragraph] =
    not(bulletListItemStart | enumListItemStart) ~>
      recursiveSpans(((not(blankLine) ~> restOfLine) +) ^^ linesToString) ^^ { Paragraph(_) }
   
  /** Parses a single paragraph nested inside another block.
   *  Markdown allows nested lists without preceding blank lines,
   *  therefore will detect list items in the middle of a parapraph,
   *  whereas a top level paragraph won't do that. One of the questionable
   *  Markdown design decisions.
   */
  val nestedParagraph: Parser[Block] = {
    val list: Parser[Block] = bulletList | enumList
    val line = not(bulletListItemStart | enumListItemStart | blankLine) ~> restOfLine
    ((recursiveSpans((line +) ^^ linesToString) ^^ { Paragraph(_) })
        ~ opt(not(blankLine) ~> list)) ^^ {
      case p ~ None => p
      case p ~ Some(list) => BlockSequence(p :: list :: Nil) // another special case of a "tight" list
    }
  }

  
  /** Parses a single Markdown block. In contrast to the generic block parser of the
   *  super-trait this method also consumes and ignores up to three optional space
   *  characters at the start of each line.
   *   
   *  @param firstLinePrefix parser that recognizes the start of the first line of this block
   *  @param linePrefix parser that recognizes the start of subsequent lines that still belong to the same block
   *  @param nextBlockPrefix parser that recognizes whether a line after one or more blank lines still belongs to the same block 
   */
  def mdBlock (firstLinePrefix: Parser[Any], linePrefix: Parser[Any], nextBlockPrefix: Parser[Any]): Parser[List[String]] = {
    block(firstLinePrefix, insignificantSpaces ~ linePrefix, nextBlockPrefix)
  }
  
  
  /** Parses a literal block, text indented by a tab or 4 spaces.
   */
  val literalBlock: Parser[LiteralBlock] = {
    mdBlock(tabOrSpace, tabOrSpace, tabOrSpace) ^^ { lines => LiteralBlock(lines.map(processWS).mkString("\n")) }
  }
  
  
  /** Parses a quoted block, a paragraph starting with a `'>'` character,
   *  with subsequent lines optionally starting with a `'>'`, too.
   */
  lazy val quotedBlock: Parser[QuotedBlock] = {
    val decoratedLine = '>' ~ (ws max 1)
    recursiveBlocks(mergeLines(mdBlock(decoratedLine, decoratedLine | not(blankLine), '>'))) ^^ (QuotedBlock(_, Nil))
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
    = recursiveBlocks(mergeLines(mdBlock(
        not(rule) ~ itemStart, not(blankLine | itemStart) ~ opt(tabOrSpace), tabOrSpace
      )))
 
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
