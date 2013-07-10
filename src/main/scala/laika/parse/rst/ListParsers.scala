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
import laika.util.RomanNumerals

/** Provides the parsers for all reStructuredText list types.
 * 
 * @author Jens Halm
 */
trait ListParsers extends laika.parse.BlockParsers { self: InlineParsers =>

  
  private def listItem [I <: ListItem] (itemStart: Parser[String], newListItem: List[Block] => I): Parser[I] = {
      (itemStart ^^ {_.length}) ~ ((ws min 1) ^^ {_.length}) >> { 
        case start ~ ws => indentedBlock(minIndent = start + ws) <~ opt(blankLines) ^^ { 
          block => newListItem(parseNestedBlocks(block)) 
        }
      } 
  }

  
  private lazy val bulletListStart = anyOf('*','-','+','\u2022','\u2023','\u2043').take(1)
  
  /** Parses a bullet list with any of the supported bullet characters.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#bullet-lists]].
   */
  def bulletList: Parser[BulletList] = {
    guard(bulletListStart <~ (ws min 1)) >> { symbol =>
      val bullet = StringBullet(symbol)
      ((listItem(symbol, BulletListItem(_,bullet))) +) ^^ { BulletList(_,bullet) }
    }
  }
  
  
  private lazy val enumListStart: Parser[(EnumFormat, Int)] = {
    val firstLowerRoman = (anyOf('i','v','x','l','c','d','m').min(2) | anyOf('i').take(1)) ^^ 
      { num => (RomanNumerals.romanToInt(num.toUpperCase), LowerRoman) }
    
    val firstUpperRoman = (anyOf('I','V','X','L','C','D','M').min(2) | anyOf('I').take(1)) ^^ 
      { num => (RomanNumerals.romanToInt(num), UpperRoman) }
    
    val firstLowerAlpha = anyIn('a' to 'h', 'j' to 'z').take(1) ^^ 
      { char => (char.charAt(0) + 1 - 'a', LowerAlpha) } // 'i' is interpreted as Roman numerical
    
    val firstUpperAlpha = anyIn('A' to 'H', 'J' to 'Z').take(1) ^^ 
      { char => (char.charAt(0) + 1 - 'A', UpperAlpha) }
    
    val firstAutoNumber = anyOf('#').take(1) ^^^ { (1,Arabic) }
    val firstArabic = anyIn('0' to '9').min(1) ^^ { num => (num.toInt, Arabic) }
    
    lazy val firstEnumType: Parser[(Int,EnumType)] = 
      firstAutoNumber | firstArabic | firstLowerAlpha | firstUpperAlpha | firstLowerRoman | firstUpperRoman
      
      ('(' ~ firstEnumType ~ ')') ^^ { case prefix ~ enumType ~ suffix => (EnumFormat(enumType._2, prefix.toString, suffix.toString), enumType._1) } | 
      (firstEnumType ~ ')' | firstEnumType ~ '.') ^^ { case enumType ~ suffix => (EnumFormat(enumType._2, "", suffix.toString), enumType._1) }
  }
  
  /** Parses an enumerated list in any of the supported combinations of enumeration style and formatting.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#enumerated-lists]].
   */
  def enumList: Parser[EnumList] = {
    
    val lowerRoman = anyOf('i','v','x','l','c','d','m').min(1)
    val upperRoman = anyOf('I','V','X','L','C','D','M').min(1)
    val lowerAlpha = anyIn('a' to 'z').take(1)
    val upperAlpha = anyIn('A' to 'Z').take(1)
    val arabic = anyIn('0' to '9').min(1)
    val autoNumber = anyOf('#').take(1)
    
    lazy val enumTypes = Map[EnumType,Parser[String]] (
      Arabic -> arabic,
      LowerAlpha -> lowerAlpha,
      UpperAlpha -> upperAlpha,
      LowerRoman -> lowerRoman,
      UpperRoman -> upperRoman
    )
    
    def enumType (et: EnumType) = enumTypes(et) | autoNumber
    
    def itemStart (format: EnumFormat): Parser[String] = 
      (format.prefix ~ enumType(format.enumType) ~ format.suffix) ^^ { case prefix ~ enumType ~ suffix => prefix + enumType + suffix }
      
    guard(enumListStart <~ (ws min 1)) >> { case (format, start) =>
      val pos = Stream.from(start).iterator
      (listItem(itemStart(format), EnumListItem(_, format, pos.next)) +) ^^ { EnumList(_, format, start) }
    }
  }
  
  /** Parser used to parse decorative lines like for rules or headers.
   *  Abstract in this trait as it does not belong to list parsers-
   */
  def punctuationChar: TextParser
  
  /** Parses a definition list.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#definition-lists]].
   */
  def definitionList: Parser[Block] = {
    
    val tableStart = anyOf(' ','=') ~ eol
    val explicitStart = ".. " | "__ "
    val listStart = (bulletListStart | enumListStart) ~ (ws min 1)
    val headerStart = (punctuationChar take 1) >> { start => (anyOf(start.charAt(0)) min 2) ~ ws ~ eol }
    
    val term: Parser[String] = not(blankLine | tableStart | explicitStart | listStart | headerStart) ~> 
        anyBut('\n') <~ eol ~ guard((ws min 1) ~ not(blankLine))
    
    val classifier = lookBehind(2,' ') ~ ' ' ~> spans(any, spanParsers) ^^ (Classifier(_))
    val nested = spanParsers + (':' -> classifier)
    
    val item = (term ~ indentedBlock(firstLineIndented = true)) ^?
      { case term ~ block => 
          DefinitionListItem(parseInline(term, nested), parseNestedBlocks(block.lines, block.nestLevel)) }
    
    ((item <~ opt(blankLines)) +) ^^ (DefinitionList(_))
  }
  
  
  /** Parses a field list.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#field-lists]].
   */
  def fieldList: Parser[Block] = {
    
    val name = ':' ~> escapedUntil(':') <~ (guard(eol) | ' ')
    
    val item = (name ~ indentedBlock()) ^^ { 
      case name ~ block => Field(parseInline(name), parseNestedBlocks(block))
    }
    
    (item +) ^^ (FieldList(_))
  }
  
  
  /** Parses an option list.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#option-lists]].
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
    
    val item = (options ~ (descStart ~> indentedBlock())) ^^ { 
      case name ~ block => OptionListItem(name, parseNestedBlocks(block))
    }
    
    ((item <~ opt(blankLines)) +) ^^ (OptionList(_))
  }
  
  /** Parses a block of lines with line breaks preserved.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#line-blocks]].
   */
  def lineBlock: Parser[Block] = {
    val itemStart = anyOf('|').take(1)
    
    val line: Parser[(Line,Int)] = {
      itemStart ~> (ws min 1) ~ indentedBlock(endsOnBlankLine = true) ^^ { 
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
