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
import laika.parse.InlineParsers
import scala.annotation.tailrec

trait ListParsers extends laika.parse.BlockParsers { self: InlineParsers => // TODO - probably needs to be rst.InlineParsers {

  
  // TODO - several members need to be extracted to a base trait once more features get added
  
  
  val tabStops = 8
  
  
  /** The maximum level of block nesting. Some block types like lists
   *  and blockquotes contain nested blocks. To protect against malicious
   *  input or accidentally broken markup, the level of nesting is restricted.
   */
  val maxNestLevel: Int = 12
  
  
  case class BlockPosition (nestLevel: Int, column: Int) {
    
    //def nextNestLevel = new BlockPosition(nestLevel + 1, column)
    
    def indent (value: Int) = new BlockPosition(nestLevel + 1, column + value)
    
  }
  
  
  /** Parses blocks that may appear inside a list item.
   * 
   *  @param pos the current parsing position 
   */
  def listItemBlocks (pos: BlockPosition) = 
    if (pos.nestLevel < maxNestLevel) (standardRstBlock(pos) | paragraph) *
    else (nonRecursiveRstBlock | paragraph) *
   
  
  /** Parses all of the standard reStructuredText blocks, except normal paragraphs. 
   * 
   *  @param pos the current parsing position 
   */
  def standardRstBlock (pos: BlockPosition): Parser[Block] = unorderedList(pos) | orderedList(pos) | definitionList(pos)

  /** Parses reStructuredText blocks, except normal paragraphs
   *  and blocks that allow nesting of blocks. Only used in rare cases when the maximum
   *  nest level allowed had been reached
   */
  def nonRecursiveRstBlock: Parser[Block]
  
  def paragraph: Parser[Paragraph]
  
  /*
   * unordered list:
   * 
   * - items start with '*', '+', '-' in column 1 (TODO - 3 more non-ASCII characters)
   * - text left-aligned and indented with one or more spaces
   * - sublist indented like text in top-list
   * - blank lines required before and after lists (including sub-list)
   * - blank lines between list items optional
   * - unindenting without blank line ends list with a warning (TODO)
   * 
   * ordered list:
   * 
   * - enumerations sequence members: 1 - A - a - II - ii
   * - auto-numerator: # (either after an initial explicit symbol, or from the start which implies arabic numbers)
   * - formatting: 1. - (1) - 1)
   * - a new list starts when there is a different formatting or the numbers are not in sequence (e.g. 1, 2, 4)
   * - lists using Roman literals must start with i or a multi-character value (otherwise will be interpreted as latin)
   * - for an unindent on the 2nd line of a list item, the entire list item will be interpreted as a normal paragraph
   * - for an unindent on other lines, the list item will end before the unindented line
   * 
   * definition list:
   * 
   * - the term is a one line phrase (all inline markup supported)
   * - it may be followed by one or more classifiers on the same line, separated by ' : '
   * - the definition is indented relative to the term
   * - no blank line allowed between term and definition
   * - definition may be any sequence of blocks
   * - blank lines only required before and after the list
   * 
   * field list:
   * 
   * - name can be any character between ':', literal ':' must be escaped
   * - all inline markup supported in names
   * - field body may contain multiple block elements
   * - the field body is indented relative to the name
   * 
   * option list:
   * 
   * - supported option types: +v, -v, --long, /V, /Long
   * - option arguments may follow after ' ' or '=' (short options may omit this separator)
   * - arguments begin with a letter followed by [a-zA-Z0-9_-] or anything between angle brackets
   * - option synonym is separated by ', '
   * - description follows after 2 spaces or on the next line
   * 
   * line block:
   * 
   * - lines start with '|'
   * - text left-aligned and indented by one or more spaces
   * - increase of indentation creates a nested line block
   * - continuation of a line starts with a space in the place of the vertical bar
   * - continuation line does not need to be aligned with preceding line
   * - any blank line ends the block
   */
  
  override def ws = anyOf(' ','\t', '\f', '\u000b') // 0x0b: vertical tab
  
  
  
  def indent (current: Int, expect: Int = 0) = Parser { in =>
    
    val source = in.source
    val eof = source.length

    val finalIndent = if (expect > 0) current + expect else Int.MaxValue
    
    def result (offset: Int, indent: Int) = Success(indent - current, in.drop(offset - in.offset))
    
    @tailrec
    def parse (offset: Int, indent: Int): ParseResult[Int] = {
      if (offset == eof) Failure("unecpected end of input", in)
      else if (indent == finalIndent) result(offset, indent)
      else {
        source.charAt(offset) match {
          case ' ' | '\f' | '\u000b' => parse(offset + 1, indent + 1)
          case '\t' => val newIndent = (indent / tabStops + 1) * tabStops
                       if (newIndent > finalIndent) result(offset, indent) else parse(offset + 1, newIndent)
          case _    => if (indent < finalIndent && expect > 0) Failure("Less than expected indentation", in) else result(offset, indent)
        }
        
      }
    }
  
    parse(in.offset, current)
  }
  
  
  def minIndent (current: Int, min: Int) = indent(current) ^? { case i if i >= min => i }
    
  
  /** Parses a full block based on the specified helper parser. When the parser for
   *  the first line succeeds, this implementation assumes that for any subsequent
   *  lines or blocks the only requirement is that they are not empty and indented
   *  like the first line.
   * 
   *  @param firstLinePrefix parser that recognizes the start of the first line of this block
   *  @param pos the current parsing position 
   */
  def indentedBlock (firstLinePrefix: Parser[Int], pos: BlockPosition): Parser[(List[String],BlockPosition)] = {
    indentedBlock(firstLinePrefix, not(blankLine), not(blankLine), pos)
  }
  
  /** Parses a full block based on the specified helper parsers.
   * 
   *  @param firstLinePrefix parser that recognizes the start of the first line of this block
   *  @param linePrefix parser that recognizes the start of subsequent lines that still belong to the same block
   *  @param nextBlockPrefix parser that recognizes whether a line after one or more blank lines still belongs to the same block
   *  @param pos the current parsing position 
   */
  def indentedBlock (firstLinePrefix: Parser[Int], linePrefix: => Parser[Any], nextBlockPrefix: => Parser[Any], pos: BlockPosition): Parser[(List[String],BlockPosition)] = {
    firstLinePrefix >> { width => indent(pos.column + width) >> { firstIndent =>
      val indentParser = indent(pos.column, width + firstIndent)
      block(success( () ), indentParser ~> linePrefix, indentParser ~> nextBlockPrefix) ^^ { lines => 
        (lines, pos.indent(firstIndent))   
      } 
    }}  
  }
  
  
  /** Parses a single list item.
   * 
   *  @param itemStart parser that recognizes the start of a list item, result will be discarded
   *  @param pos the current parsing position 
   */
  def listItem (itemStart: Parser[String], pos: BlockPosition): Parser[ListItem] = {
      indentedBlock(itemStart ^^ { res => res.length }, pos) ^^
      { case (lines,pos) => ListItem(parseMarkup(listItemBlocks(pos), lines mkString "\n")) }
  }
  
  
  /** Parses an unordered list.
   * 
   *  @param pos the current parsing position 
   */
  def unorderedList (pos: BlockPosition): Parser[UnorderedList] = {
    val itemStart = anyOf('*','-','+').take(1)
    
    guard(itemStart) >> { symbol =>
      ((listItem(symbol, pos)) *) ^^ { UnorderedList(_) }
    }
  }
  
  
  /** Parses an ordered list in any of the supported combinations of enumeration style and formatting.
   * 
   *  @param pos the current parsing position 
   */
  def orderedList (pos: BlockPosition): Parser[OrderedList] = {
    
    val firstLowerRoman = (anyOf('i','v','x','l','c','d','m').min(2) | anyOf('i').take(1)) ^^^ { LowerRoman }
    val lowerRoman = anyOf('i','v','x','l','c','d','m').min(1)
    
    val firstUpperRoman = (anyOf('I','V','X','L','C','D','M').min(2) | anyOf('I').take(1)) ^^^ { UpperRoman }
    val upperRoman = anyOf('I','V','X','L','C','D','M').min(1)
    
    val firstLowerAlpha = anyIn('a' to 'h', 'j' to 'z').take(1) ^^^ { LowerAlpha } // 'i' is interpreted as Roman numerical
    val lowerAlpha = anyIn('a' to 'z').take(1)
  
    val firstUpperAlpha = anyIn('A' to 'H', 'J' to 'Z').take(1) ^^^ { UpperAlpha }
    val upperAlpha = anyIn('A' to 'Z').take(1)
    
    val arabic = anyIn('0' to '9').min(1)
    val firstArabic = arabic ^^^ { Arabic }
    
    val autoNumber = anyOf('#').take(1)
    val firstAutoNumber = autoNumber ^^^ { Arabic }
    
    lazy val enumTypes = Map[EnumType,Parser[String]] (
      Arabic -> arabic,
      LowerAlpha -> lowerAlpha,
      UpperAlpha -> upperAlpha,
      LowerRoman -> lowerRoman,
      UpperRoman -> upperRoman
    )
    
    def enumType (et: EnumType) = enumTypes(et) | autoNumber
    
    lazy val firstEnumType: Parser[EnumType] = firstAutoNumber | firstArabic | firstLowerAlpha | firstUpperAlpha | firstLowerRoman | firstUpperRoman
    
    lazy val firstItemStart: Parser[(String, EnumType, String)] = 
      ('(' ~ firstEnumType ~ ')') ^^ { case prefix ~ enumType ~ suffix => (prefix.toString, enumType, suffix.toString) } | 
      (firstEnumType ~ ')' | firstEnumType ~ '.') ^^ { case enumType ~ suffix => ("", enumType, suffix.toString) }
    
    def itemStart (prefix: Parser[String], et: EnumType, suffix: Parser[String]): Parser[String] = 
      (prefix ~ enumType(et) ~ suffix) ^^ { case prefix ~ enumType ~ suffix => prefix + enumType + suffix }
      
    guard(firstItemStart) >> { case (prefix, enumType, suffix) => // TODO - keep start number
      ((listItem(itemStart(prefix, enumType, suffix), pos)) *) ^^ { OrderedList(_, enumType, prefix, suffix) }
    }
  }
  
  
  /** Parses a definition list.
   * 
   *  @param pos the current parsing position 
   */
  def definitionList (pos: BlockPosition): Parser[Block] = {
    
    def term: Parser[String] = not(blankLine) ~> restOfLine
    
    val itemStart = not(blankLine) ~> minIndent(pos.column, 1)
    
    def item (pos: BlockPosition) = (term ~ indentedBlock(itemStart, pos)) ^^
      { case term ~ ((lines, pos)) => DefinitionListItem(parseInline(term), parseMarkup(listItemBlocks(pos), lines mkString "\n")) }
    
    ((item(pos)) *) ^^ { DefinitionList(_) }
  }
  
  def parseInline (source: String): List[Span] = parseInline(source, Map.empty) // TODO - implement
  
    
  
}