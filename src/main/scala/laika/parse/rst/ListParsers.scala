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
import laika.parse.rst.Elements._
import laika.parse.InlineParsers
import scala.annotation.tailrec
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer

trait ListParsers extends BlockBaseParsers { self: InlineParsers => // TODO - probably needs to be rst.InlineParsers {

  
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
  
  
  /** Parses blocks that may appear inside a list item.
   * 
   *  @param pos the current parsing position 
   */
  def listItemBlocks = 
    if (true) (standardRstBlock | paragraph) * // TODO - check nest level
    else (nonRecursiveRstBlock | paragraph) *
    
  
  /** Parses a single list item.
   * 
   *  @param itemStart parser that recognizes the start of a list item, result will be discarded
   *  @param pos the current parsing position 
   */
  def listItem (itemStart: Parser[String]): Parser[ListItem] = {
      indentedBlock(itemStart ^^ { res => res.length }) ^^
      { case (lines,pos) => ListItem(parseMarkup(listItemBlocks, lines mkString "\n")) }
  }
  
  
  /** Parses an unordered list.
   * 
   *  @param pos the current parsing position 
   */
  def unorderedList: Parser[UnorderedList] = {
    val itemStart = anyOf('*','-','+').take(1)
    
    guard(itemStart) >> { symbol =>
      ((listItem(symbol)) *) ^^ { UnorderedList(_) }
    }
  }
  
  
  /** Parses an ordered list in any of the supported combinations of enumeration style and formatting.
   * 
   *  @param pos the current parsing position 
   */
  def orderedList: Parser[OrderedList] = {
    
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
      (listItem(itemStart(prefix, enumType, suffix)) *) ^^ { OrderedList(_, enumType, prefix, suffix) }
    }
  }
  
  
  /** Parses a definition list.
   * 
   *  @param pos the current parsing position 
   */
  def definitionList: Parser[Block] = {
    
    val term: Parser[String] = not(blankLine) ~> restOfLine
    
    val itemStart = not(blankLine) ^^^ 0
    
    val item = (term ~ indentedBlock(itemStart)) ^^ // TODO - add classifier parser to parseInline map
      { case term ~ ((lines, pos)) => 
          DefinitionListItem(parseInline(term), parseMarkup(listItemBlocks, lines mkString "\n")) }
    
    (item *) ^^ DefinitionList
  }
  
  
  /** Parses a field list.
   * 
   *  @param pos the current parsing position 
   */
  def fieldList: Parser[Block] = {
    
    val name = ':' ~> anyBut(':') <~ ':' // TODO - escaped ':' in name should be supported
    
    val firstLine = restOfLine // TODO - may need to check for non-empty body 
    
    val itemStart = success(0)
    
    val item = (name ~ firstLine ~ opt(indentedBlock(itemStart))) ^^
      { case name ~ firstLine ~ Some((lines, pos)) => 
          Field(parseInline(name), parseMarkup(listItemBlocks, (firstLine :: lines) mkString "\n"))
        case name ~ firstLine ~ None => 
          Field(parseInline(name), parseMarkup(listItemBlocks, firstLine)) }
    
    (item *) ^^ FieldList
  }
  
  
  /** Parses an option list.
   * 
   *  @param pos the current parsing position 
   */
  def optionList: Parser[Block] = {
    
    val optionString = anyIn('a' to 'z', 'A' to 'Z', '0' to '9', '_', '-').min(1)
    
    val gnu =        '+' ~ anyIn('a' to 'z', 'A' to 'Z', '0' to '9').take(1) ^^ mkString
    val shortPosix = '-' ~ anyIn('a' to 'z', 'A' to 'Z', '0' to '9').take(1) ^^ mkString
    val longPosix = "--" ~ optionString ^^ { case a ~ b => a+b }
    val dos = '/' ~ optionString ^^ mkString
    
    val arg = opt(accept('=') | ' ') ~ optionString ^^ { 
      case Some(delim) ~ argStr => OptionArgument(argStr, delim.toString)
      case None ~ argStr => OptionArgument(argStr, "") 
    }
    
    val option = (gnu | shortPosix | longPosix | dos) ~ opt(arg) ^^ { case option ~ arg => Option(option, arg) }
    
    val options = (option ~ ((", " ~> option)*)) ^^ mkList
    
    val firstLine = ("  " ~ not(blankLine) ~> restOfLine) | (blankLine ~ guard((ws min 1) ~ not(blankLine))) ^^^ "" 
    
    val itemStart = success(0)
    
    val item = (options ~ firstLine ~ indentedBlock(itemStart)) ^^
      { case name ~ firstLine ~ ((lines, pos)) => 
          OptionListItem(name, parseMarkup(listItemBlocks, (firstLine :: lines) mkString "\n")) }
    
    (item *) ^^ OptionList
  }
  
  /** Parses a block of lines with line breaks preserved.
   */
  def lineBlock: Parser[Block] = {
    val itemStart = anyOf('|').take(1)
    
    val line: Parser[(Line,Int)] = {
      indentedBlock(itemStart ^^^ 1, not(blankLine), failure("line blocks always end after blank lines")) ^^
      { case (lines,pos) => (Line(parseInline(lines mkString "\n")), pos) }
    }
    
    def nest (lines: Seq[(Line,Int)]) : LineBlock = {
      
      val stack = new Stack[(ListBuffer[LineBlockItem],Int)]
  
      @tailrec
      def addItem (item: LineBlockItem, level: Int): Unit = {
        if (stack.isEmpty || level > stack.top._2) stack push ((ListBuffer(item), level))
        else if (level == stack.top._2) stack.top._1 += item
        else {
          val newBlock = LineBlock(stack.pop._1.toList)
          if (!stack.isEmpty && stack.top._2 >= level) {
            stack.top._1 += newBlock
            addItem(item, level)
          }
          else {
            stack push ((ListBuffer(newBlock, item), level))
          }
        }
      }
      
      lines.foreach { 
        case (line, level) => addItem(line, level)
      }
  
      val (topBuffer, _) = stack.reduceLeft { (top, next) =>
        next._1 += LineBlock(top._1.toList)
        next
      }
      
      LineBlock(topBuffer.toList)
    }
    
    (line *) ^^ nest
  } 
  
  
  private def mkString (result: ~[Char,String]) = result._1.toString + result._2 // TODO - maybe promote to MarkupParsers
  
  def parseInline (source: String): List[Span] = parseInline(source, Map.empty) // TODO - implement
  
    
  
}
