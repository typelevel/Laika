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
  
  
  /** The maximum level of block nesting. Some block types like lists
   *  and blockquotes contain nested blocks. To protect against malicious
   *  input or accidentally broken markup, the level of nesting is restricted.
   */
  val maxNestLevel: Int = 12
  
   
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
    (pattern('*') | pattern('-') | pattern('_')) ~ ws ~ eol ^^^ { Rule }
  }


  /** Parses all of the standard Markdown blocks, except normal paragraphs and those blocks
   *  that deal with verbatim HTML. For the latter parsers are provided by a separate, optional trait.
   */
  def standardMarkdownBlock (nestLevel: Int): Parser[Block] = 
    atxHeader | setextHeader | (insignificantSpaces ~> 
      (codeBlock | quotedBlock(nestLevel) | rule | unorderedList(nestLevel) | orderedList(nestLevel)))

  /** Parses Markdown blocks, except normal paragraphs, blocks that deal with verbatim HTML
   *  and blocks that allow nesting of blocks. Only used in rare cases when the maximum
   *  nest level allowed had been reached
   */
  def nonRecursiveMarkdownBlock: Parser[Block] = 
    atxHeader | setextHeader | (insignificantSpaces ~> (codeBlock | rule ))

  /** Parses Markdown blocks which are only recognized on the top document
   *  level, not nested inside other blocks.
   */
  def topLevelMarkdownBlock: Parser[Block] = linkDefinition
 
  
  def topLevelBlock: Parser[Block] = standardMarkdownBlock(0) | topLevelMarkdownBlock | paragraph 
 
  def nestedBlock: Parser[Block] = standardMarkdownBlock(1) | paragraph
  
  def nestedBlock (nestLevel: Int): Parser[Block] = 
    if (nestLevel < maxNestLevel) standardMarkdownBlock(nestLevel) | paragraph
    else nonRecursiveMarkdownBlock | paragraph
  
  /** Parses blocks that may appear inside a list item.
   */
  def listItemBlocks (nestLevel: Int) = 
    if (nestLevel < maxNestLevel) (standardMarkdownBlock(nestLevel) | flowContent | preserveBlankLines) *
    else (nonRecursiveMarkdownBlock | flowContent | preserveBlankLines) *
  

  /** Parses a single paragraph. Everything between two blank lines that is not
   *  recognized as a special Markdown block type will be parsed as a regular paragraph.
   */
  def paragraph: Parser[Paragraph] = 
    (flowLine +) ^^ { lines => Paragraph(parseInline(linesToString(lines))) }

  /** Parses flow content, a block type which may be used inside list items
   *  to differentiate it from full paragraphs inside list items.
   */
  def flowContent: Parser[FlowContent] = 
    (flowLine +) ^^ { lines => FlowContent(parseInline(linesToString(lines))) }
  
   /** Parses a single line of regular flow content.
    */
  def flowLine: Parser[String] = 
    not(blankLine | unorderedListItemStart | orderedListItemStart) ~> restOfLine
  
  
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
  
  
  /** Parses a code block, text indented by a tab or 4 spaces.
   */
  val codeBlock: Parser[CodeBlock] = {
    mdBlock(tabOrSpace, tabOrSpace, tabOrSpace) ^^ { lines => new CodeBlock(lines.mkString("\n")) }
  }
  
  
  /** Parses a quoted block, a paragraph starting with a `'>'` character,
   *  with subsequent lines optionally starting with a `'>'`, too.
   */
  def quotedBlock (nestLevel: Int): Parser[QuotedBlock] 
    = mdBlock('>', accept('>') | not(blankLine), '>') ^^ 
      { lines => QuotedBlock(parseNestedBlocks(lines, nestedBlock(nestLevel + 1)), Nil) }

  
  /** Represents one or more consecutive blank lines. For parsing Markdown lists
   *  these blank lines become part of the document tree temporarily. This is because
   *  blank lines between list items are significant in Markdown. They will be processed
   *  and removed from the model, before the final Document instance gets produced.
   */
  case object BlankLines extends Block
  
  /** Parses blank lines and produces the corresponding element model.
   */
  def preserveBlankLines = blankLines ^^^ {BlankLines}

  /** Parses a list based on the specified helper parsers.
   * 
   *  @param itemStart parser that recognizes the start of a list item, result will be discarded
   *  @param newList function that produces a block element for the document tree
   *  @param nestLevel the current level of nesting of blocks
   */
  def list [T <: Block] (itemStart: Parser[String], newList: List[ListItem] => T, nestLevel: Int) = {
    
    def flattenItems (items: List[~[Option[Block],ListItem]]) = {
      val hasBlankLines = items exists { 
        case Some(_) ~ _  => true
        case None ~ _     => false
      }
      def rewriteItemContent (item:ListItem) = {
        ListItem(((List[Block](), false) /: item.content) { 
          /* Promoting FlowContent to Paragraph if the list has any blank lines between list items or if it is adjacent
             to blank lines within the list item itself. This is ugly, but forced by the (in this respect odd) design of Markdown. 
             The second (boolean) value in the accumulator tuple signals whether the previous item represented one or more blank lines */
          case ((FlowContent(content) :: xs,_), BlankLines)     => (Paragraph(content) :: xs, true)
          case ((xs,true), FlowContent(content))                => (Paragraph(content) :: xs, false)
          case ((xs,_), FlowContent(content)) if hasBlankLines  => (Paragraph(content) :: xs, false)
          case ((xs,_), BlankLines)                             => (xs, true)
          case ((xs,_), item)                                   => (item :: xs, false)
        }._1.reverse)
      } 
      items map { case _ ~ (li @ ListItem(_)) => rewriteItemContent(li) } 
    }
    
    guard(itemStart) ~> ((opt(preserveBlankLines) ~ listItem(itemStart, nestLevel)) *) ^^ 
      { x => newList(flattenItems(x)) }
  }
  
  /** Parses a single list item.
   * 
   *  @param itemStart parser that recognizes the start of a list item, result will be discarded
   */
  def listItem (itemStart: Parser[String], nestLevel: Int): Parser[ListItem]
    = mdBlock(itemStart, not(blankLine | itemStart) ~ opt(tabOrSpace), tabOrSpace) ^^
        { lines => ListItem(parseMarkup(listItemBlocks(nestLevel + 1), lines mkString "\n")) }
 
  
  /** Parses the start of an unordered list item.
   */
  val unorderedListItemStart = anyOf('*','-','+').take(1) <~ (anyOf(' ','\t') min 1)
  
  /** Parses the start of an ordered list item.
   */
  val orderedListItemStart = anyIn('0' to '9').min(1) <~ '.' ~ (anyOf(' ','\t') min 1)
  
  /** Parses an unordered list.
   */
  def unorderedList (nestLevel: Int): Parser[UnorderedList] = list(unorderedListItemStart, UnorderedList, nestLevel)
    
  /** Parses an ordered list.
   */
  def orderedList (nestLevel: Int): Parser[OrderedList] = list(orderedListItemStart, OrderedList(_), nestLevel) 
    
  
}