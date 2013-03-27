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
import scala.annotation.tailrec
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer

/**
 * @author Jens Halm
 */
trait ListParsers extends BlockBaseParsers { self: InlineParsers =>

  
  /*
   * unordered list:
   * 
   * - items start with '*', '+', '-', \u2022, \u2023, \u2043 in column 1
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
  
  
  
  /** Parses a single list item.
   * 
   *  @param itemStart parser that recognizes the start of a list item, result will be discarded
   */
  def listItem (itemStart: Parser[String]): Parser[ListItem] = {
      (itemStart ^^ {_.length}) ~ ((ws min 1) ^^ {_.length}) >> { 
        case start ~ ws => fixedIndentedBlock(start + ws) ^^
          { block => ListItem(parseNestedBlocks(block)) }
      } 
  }
  
  
  /** Parses an unordered list with any of the supported bullet characters.
   */
  def unorderedList: Parser[UnorderedList] = {
    val itemStart = anyOf('*','-','+','\u2022','\u2023','\u2043').take(1)
    
    guard(itemStart <~ (ws min 1)) >> { symbol =>
      ((listItem(symbol)) +) ^^ { UnorderedList(_) }
    }
  }
  
  
  /** Parses an ordered list in any of the supported combinations of enumeration style and formatting.
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
      (listItem(itemStart(prefix, enumType, suffix)) +) ^^ { OrderedList(_, enumType, prefix, suffix) }
    }
  }
  
  
  /** Parses a definition list.
   */
  def definitionList: Parser[Block] = {
    
    val term: Parser[String] = not(blankLine) ~> anyBut('\n') <~ guard(eol ~ (ws min 1) ~ not(blankLine))
    
    val item = (term ~ varIndentedBlock()) ^? // TODO - add classifier parser to parseInline map
      { case term ~ block if !block.lines.tail.isEmpty => 
          DefinitionListItem(parseInline(term), parseNestedBlocks(block.lines.tail, block.nestLevel)) }
    
    (item +) ^^  DefinitionList
  }
  
  
  /** Parses a field list.
   */
  def fieldList: Parser[Block] = {
    
    val name = ':' ~> anyBut(':') <~ ':' ~ (guard(eol) | ' ') // TODO - escaped ':' in name should be supported
    
    val item = (name ~ opt(varIndentedBlock())) ^^
      { case name ~ Some(block) => 
          Field(parseInline(name), parseNestedBlocks(block))
        case name ~ None => 
          Field(parseInline(name), Nil) }
    
    (item +) ^^ FieldList
  }
  
  
  /** Parses an option list.
   */
  def optionList: Parser[Block] = {
    
    def mkString (result: ~[Char,String]) = result._1.toString + result._2
    
    val optionString = anyIn('a' to 'z', 'A' to 'Z', '0' to '9', '_', '-').min(1)
    
    val gnu =        '+' ~ anyIn('a' to 'z', 'A' to 'Z', '0' to '9').take(1) ^^ mkString
    val shortPosix = '-' ~ anyIn('a' to 'z', 'A' to 'Z', '0' to '9').take(1) ^^ mkString
    val longPosix = "--" ~ optionString ^^ { case a ~ b => a+b }
    val dos = '/' ~ optionString ^^ mkString
    
    val arg = opt(accept('=') | ' ') ~ optionString ^^ { 
      case Some(delim) ~ argStr => OptionArgument(argStr, delim.toString)
      case None ~ argStr => OptionArgument(argStr, "") 
    }
    
    val option = (gnu | shortPosix | longPosix | dos) ~ opt(arg) ^^ { case option ~ arg => ProgramOption(option, arg) }
    
    val options = (option ~ ((", " ~> option)*)) ^^ mkList
    
    val descStart = ("  " ~ not(blankLine)) | (guard(blankLine ~ (ws min 1) ~ not(blankLine))) ^^^ "" 
    
    val item = (options ~ (descStart ~> varIndentedBlock())) ^^
      { case name ~ block =>
        println("?: " + block.lines)
          OptionListItem(name, parseNestedBlocks(block)) }
    
    (item +) ^^ OptionList
  }
  
  /** Parses a block of lines with line breaks preserved.
   */
  def lineBlock: Parser[Block] = {
    val itemStart = anyOf('|').take(1)
    
    val line: Parser[(Line,Int)] = {
      itemStart ~> (ws min 1) ~ varIndentedBlock(1, not(blankLine), failure("line blocks always end after blank lines")) ^^ { 
        case indent ~ block => (Line(parseInline(block.lines mkString "\n")), indent.length) 
      }
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
    
    (line +) ^^ nest
  } 
  
  
}
