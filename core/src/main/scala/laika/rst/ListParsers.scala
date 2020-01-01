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

package laika.rst

import laika.ast._
import laika.bundle.{BlockParser, BlockParserBuilder}
import laika.collection.Stack
import laika.parse.Parser
import laika.parse.markup.BlockParsers._
import laika.parse.markup.RecursiveParsers
import laika.parse.text.TextParsers._
import laika.rst.ast._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.Try

/** Provides the parsers for all reStructuredText list types.
 * 
 * @author Jens Halm
 */
object ListParsers {


  private def listItem [I <: ListItem] (itemStart: Parser[String], newListItem: Seq[Block] => I)
                                       (implicit recParsers: RecursiveParsers): Parser[I] = {
      (itemStart ^^ {_.length}) ~ ws.min(1).count >> {
        case start ~ ws =>
          recParsers.recursiveBlocks(indentedBlock(minIndent = start + ws, maxIndent = start + ws) ~
              opt(blankLines | eof | lookAhead(itemStart)) ^^? {
            case (block ~ None) if block.linesIterator.length < 2 => Left("not a list item")
            case (block ~ _) => Right(block)
          }).map(newListItem)
      } 
  }
  
  private def rewriteListItems [I <: BlockContainer](items: List[I], newListItem: (I,List[Block]) => I): List[I] = {
    
    /* The reStructuredText reference parser makes a distinction between "simple" lists
     * and normal lists. The exact rules are not documented, but tests seem to hint at
     * a "simple" list being a list where all items only have a single paragraph and optionally
     * a nested list below the paragraph. The distinction has an influence on the HTML renderer
     * for example. 
     */
    
    val isSimple = items.forall { con => con.content match {
      case Paragraph(_,_) :: Nil => true
      case Paragraph(_,_) :: (_:ListContainer) :: Nil => true
      case _ => false
    }}
    
    if (isSimple) {
      items map { con => con.content match {
        case Paragraph(content,opt) :: (nested: ListContainer) :: Nil =>
          newListItem(con, SpanSequence(content,opt) :: nested :: Nil)
        case _ =>
          con
      }}
    }
    else {
      items map { con => con.content match {
        case Paragraph(content,opt) :: Nil => newListItem(con, ForcedParagraph(content,opt) :: Nil)
        case _ => con
      }}
    }
  }

  
  private lazy val bulletListStart = anyOf('*','-','+','\u2022','\u2023','\u2043').take(1)
  
  /** Parses a bullet list with any of the supported bullet characters.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#bullet-lists]].
   */
  lazy val bulletList: BlockParserBuilder = BlockParser.withoutStartChar.recursive { implicit recParsers =>
    lookAhead(bulletListStart <~ (ws min 1)) >> { symbol =>
      val bullet = StringBullet(symbol)
      (listItem(symbol, BulletListItem(_, bullet)) +) ^^
        { items => BulletList(rewriteListItems(items,(item:BulletListItem,content) => item.copy(content = content)),bullet) }
    }
  }
  
  private lazy val enumListStart: Parser[(EnumFormat, Int)] = {
    import EnumType._
    val firstLowerRoman = (anyOf('i','v','x','l','c','d','m').min(2) | anyOf('i').take(1)) ^^? 
      { num => RomanNumerals.romanToInt(num.toUpperCase).map(_ -> LowerRoman) }
    
    val firstUpperRoman = (anyOf('I','V','X','L','C','D','M').min(2) | anyOf('I').take(1)) ^^? 
      { num => RomanNumerals.romanToInt(num.toUpperCase).map(_ -> UpperRoman) }
    
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
  lazy val enumList: BlockParserBuilder = BlockParser.withoutStartChar.recursive { implicit recParsers =>
    import EnumType._

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
      
    lookAhead(enumListStart <~ (ws min 1)) >> { case (format, start) =>
      val pos = Iterator.from(start)
      (listItem(itemStart(format), EnumListItem(_, format, pos.next)) +) ^^ 
        { items => EnumList(rewriteListItems(items,(item:EnumListItem,content) => item.copy(content = content)), format, start) }
    }
  }
  
  /** Parses a definition list.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#definition-lists]].
   */
  lazy val definitionList: BlockParserBuilder = BlockParser.withoutStartChar.recursive { recParsers =>
    
    val tableStart = anyOf(' ','=') ~ eol
    val explicitStart = ".. " | "__ "
    val listStart = (bulletListStart | enumListStart) ~ (ws min 1)
    val headerStart = (BaseParsers.punctuationChar take 1) >> { start => (anyOf(start.charAt(0)) min 2) ~ wsEol }
    
    val term: Parser[String] = not(blankLine | tableStart | explicitStart | listStart | headerStart) ~> 
        anyBut('\n') <~ eol ~ lookAhead((ws min 1) ~ not(blankLine))
    
    val classifier = lookBehind(2,' ') ~ ' ' ~> recParsers.recursiveSpans ^^ (Classifier(_))
    val termWithClassifier = recParsers.recursiveSpans(term, Map(':' -> classifier))

    val item = (termWithClassifier ~ recParsers.recursiveBlocks(indentedBlock(firstLineIndented = true))) ^? {
      case term ~ blocks => DefinitionListItem(term, blocks)
    }
    
    ((item <~ opt(blankLines)) +) ^^ (DefinitionList(_))
  }
  
  
  /** Parses a field list.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#field-lists]].
   */
  lazy val fieldList: BlockParserBuilder = BlockParser.withoutStartChar.recursive { recParsers =>
    
    val name = ':' ~> recParsers.escapedUntil(':') <~ (lookAhead(eol) | ' ')
    
    val item = (recParsers.recursiveSpans(name) ~ recParsers.recursiveBlocks(indentedBlock())) ^^ {
      case name ~ blocks => Field(name, blocks)
    }
    
    (item +) ^^ (FieldList(_))
  }
  
  
  /** Parses an option list.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#option-lists]].
   */
  lazy val optionList: BlockParserBuilder = BlockParser.withoutStartChar.recursive { recParsers =>
    
    def mkString (result: ~[Char,String]) = result._1.toString + result._2
    
    val optionString = anyIn('a' to 'z', 'A' to 'Z', '0' to '9', '_', '-').min(1)
    val optionArg = optionString | (('<' ~> delimitedBy('>')) ^^ { "<" + _ + ">" } )
    
    val gnu =        '+' ~ anyIn('a' to 'z', 'A' to 'Z', '0' to '9').take(1) ^^ mkString
    val shortPosix = '-' ~ anyIn('a' to 'z', 'A' to 'Z', '0' to '9').take(1) ^^ mkString
    val longPosix = ("--" <~ not('-')) ~ optionString ^^ { case a ~ b => a+b }
    val dos = '/' ~ optionString ^^ mkString
    
    val arg = opt(char('=') | ' ') ~ optionArg ^^ {
      case Some(delim) ~ argStr => OptionArgument(argStr, delim.toString)
      case None ~ argStr => OptionArgument(argStr, "") 
    }
    
    val option = (gnu | shortPosix | longPosix | dos) ~ opt(arg) ^^ { case option ~ arg => ProgramOption(option, arg) }
    
    val options = (option ~ ((", " ~> option)*)) ^^ { case x ~ xs => x :: xs }
    
    val descStart = ((anyOf(' ') min 2) ~ not(blankLine)) | lookAhead(blankLine ~ (ws min 1) ~ not(blankLine)) ^^^ ""
    
    val item = (options ~ (descStart ~> recParsers.recursiveBlocks(indentedBlock()))) ^^ {
      case name ~ blocks => OptionListItem(name, blocks)
    }
    
    ((item <~ opt(blankLines)) +) ^^ (OptionList(_))
  }
  
  /** Parses a block of lines with line breaks preserved.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#line-blocks]].
   */
  lazy val lineBlock: BlockParserBuilder = BlockParser.withoutStartChar.recursive { recParsers =>
    val itemStart = anyOf('|').take(1)
    
    val line: Parser[(Line, Int)] = {
      itemStart ~> (ws min 1) ~ recParsers.recursiveSpans(indentedBlock(endsOnBlankLine = true)) ^^ {
        case indent ~ block => (Line(block), indent.length)
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
          if (stack.nonEmpty && stack.top._2 >= level) {
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
  
      val (topBuffer, _) = stack.elements.reduceLeft { (top, next) =>
        next._1 += LineBlock(top._1.toList)
        next
      }
      
      LineBlock(topBuffer.toList)
    }
    
    (line +) ^^ nest
  } 
  
  
}
