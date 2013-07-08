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
import scala.collection.mutable.StringBuilder
import laika.parse.util.WhitespacePreprocessor
 

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
    
    ((anyOf('#') min 1 max 6) ^^ { _.length }) ~ restOfLine ^^ 
      { case level ~ text => Header(level, parseInline(stripDecoration(text))) }
  }
  
  /** Parses a 1st or 2nd level Setext header. A first level header consists of the
   *  text of the header followed by a line of one or more `'='` characters, a 2nd
   *  level header uses `'-'` characters instead.
   *  
   *  In contrast to several other Markdown parsers this parser requires a blank line
   *  before the header. 
   */
  lazy val setextHeader: Parser[Header] = textLine ~ (anyOf('=').min(1) | anyOf('-').min(1)) <~ (ws ~ eol) ^^ {
    case text ~ decoration if decoration.head == '=' => Header(1, parseInline(text)) 
    case text ~ _                                    => Header(2, parseInline(text))
  }
  
  /** Parses a horizontal rule, a line only decorated with three or more `'*'`, `'-'` or `'_'`
   *  characters with optional spaces between them
   */
  lazy val rule: Parser[Block] = {
    def pattern (char: Char) = char ~ repMin(2, (anyOf(' ')) ~ char)
    (pattern('*') | pattern('-') | pattern('_')) ~ ws ~ eol ^^^ { Rule() }
  }


  /** Parses all of the standard Markdown blocks, except normal paragraphs and those blocks
   *  that deal with verbatim HTML. For the latter parsers are provided by a separate, optional trait.
   */
  def standardMarkdownBlock: Parser[Block] = 
    atxHeader | setextHeader | (insignificantSpaces ~> 
      (literalBlock | quotedBlock | rule | bulletList | enumList))

  
  def topLevelBlock: Parser[Block] = standardMarkdownBlock | (insignificantSpaces ~> linkTarget) | paragraph 
 
  def nestedBlock: Parser[Block] = standardMarkdownBlock | nestedParagraph
  
  def nonRecursiveBlock: Parser[Block] = 
    atxHeader | setextHeader | (insignificantSpaces ~> (literalBlock | rule )) | paragraph


  /** Parses a single paragraph. Everything between two blank lines that is not
   *  recognized as a special Markdown block type will be parsed as a regular paragraph.
   */
  def paragraph: Parser[Paragraph] = 
    not(bulletListItemStart | enumListItemStart) ~> ((not(blankLine) ~> restOfLine) +) ^^ { 
      lines => Paragraph(parseInline(linesToString(lines))) 
    }
   
  /** Parses a single paragraph nested inside another block.
   *  Markdown allows nested lists without preceding blank lines,
   *  therefore will detect list items in the middle of a parapraph,
   *  whereas a top level paragraph won't do that. One of the questionable
   *  Markdown design decisions.
   */
  def nestedParagraph: Parser[Paragraph] = 
    ((not(bulletListItemStart | enumListItemStart | blankLine) ~> restOfLine) +) ^^ { 
      lines => Paragraph(parseInline(linesToString(lines))) 
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
    mdBlock(tabOrSpace, tabOrSpace, tabOrSpace) ^^ { lines => new LiteralBlock(lines.map(processWS).mkString("\n")) }
  }
  
  
  /** Parses a quoted block, a paragraph starting with a `'>'` character,
   *  with subsequent lines optionally starting with a `'>'`, too.
   */
  def quotedBlock: Parser[QuotedBlock] 
    = withNestLevel(mdBlock('>' ~ (ws max 1), ('>' ~ (ws max 1)) | not(blankLine), '>')) ^^ 
      { case (nestLevel,lines) => QuotedBlock(parseNestedBlocks(lines, nestLevel), Nil) }

  
  /** Parses a list based on the specified helper parsers.
   * 
   *  @param itemStart parser that recognizes the start of a list item, result will be discarded
   *  @param newList function that produces a block element for the document tree
   *  @param newItem function that produces a new list item element based on position and content arguments
   *  @param nestLevel the current level of nesting of blocks
   */
  def list [T <: Block, I <: ListItem] (itemStart: Parser[String], 
                                        newList: List[ListItem] => T, 
                                        newItem: (Int,List[Block]) => I) = {
    
    def flattenItems (items: List[~[Option[Any],List[Block]]]) = {
      val hasBlankLines = items exists { 
        case Some(_) ~ _  => true
        case None ~ _     => false
      }
      def rewriteItemContent (blocks: List[Block], pos: Int) = {
        val rewritten = blocks match {
          /* Promoting Paragraph to ForcedParagraph if the list has any blank lines 
             between list items or if it is adjacent to blank lines within the list item 
             itself. This is ugly, but forced by the (in this respect odd) design of Markdown. */
          case Paragraph(content,opt) :: Nil if hasBlankLines => 
            BlockParsers.ForcedParagraph(content, opt) :: Nil
          case other => other
        }
        
        newItem(pos,rewritten)
      } 
      val pos = Stream.from(1).iterator
      items map { item => rewriteItemContent(item._2, pos.next) } 
    }
    
    guard(itemStart) ~> ((opt(blankLines) ~ listItem(itemStart)) *) ^^ 
      { x => newList(flattenItems(x)) }
  }
  
  /** Parses a single list item.
   * 
   *  @param itemStart parser that recognizes the start of a list item, result will be discarded
   */
  def listItem [I <: ListItem] (itemStart: Parser[String]): Parser[List[Block]]
    = withNestLevel(mdBlock(itemStart, not(blankLine | itemStart) ~ opt(tabOrSpace), tabOrSpace)) ^^
        { case (nestLevel,lines) => parseNestedBlocks(lines, nestLevel) }
 
  
  /** Parses the start of a bullet list item.
   */
  val bulletListItemStart = anyOf('*','-','+').take(1) <~ (anyOf(' ','\t') min 1)
  
  /** Parses the start of an enumerated list item.
   */
  val enumListItemStart = anyIn('0' to '9').min(1) <~ '.' ~ (anyOf(' ','\t') min 1)
  
  /** Parses a bullet list, called "unordered list" in the Markdown syntax description.
   */
  def bulletList: Parser[BulletList] = {
    guard(bulletListItemStart) >> { symbol =>
      val bullet = StringBullet(symbol)
      list(bulletListItemStart, BulletList(_,bullet), (_,blocks)=>BulletListItem(blocks,bullet))
    }
  }
    
  /** Parses an enumerated list, called "ordered list" in the Markdown syntax description.
   */
  def enumList: Parser[EnumList] = {
    list(enumListItemStart, EnumList(_, EnumFormat()), (pos,blocks)=>EnumListItem(blocks,EnumFormat(),pos)) 
  }
    
  
}

/** Holds element tree nodes for blocks which are too specific for Markdown to be included
 *  in the generic model.
 */
object BlockParsers {
  
  /** Represents a paragraph that does not get optimized to a simple span sequence
   *  in renderers. Needed for the Markdown-specific way of dealing with list items
   *  separated by blank lines which force an extra paragraph tag inside the `li` tag
   *  in HTML renderers.
   */
  case class ForcedParagraph (content: Seq[Span], baseOptions: Options = NoOpt) extends Block 
                                                  with SpanContainer[ForcedParagraph] {
    def options = baseOptions + Fallback(Paragraph(content,baseOptions))
  }
  
}
