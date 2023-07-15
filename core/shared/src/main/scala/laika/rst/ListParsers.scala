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
import laika.bundle.BlockParserBuilder
import laika.collection.Stack
import laika.parse.Parser
import laika.parse.markup.RecursiveParsers
import laika.parse.text.CharGroup
import laika.parse.builders._
import laika.parse.implicits._
import laika.rst.ast._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/** Provides the parsers for all reStructuredText list types.
  *
  * @author Jens Halm
  */
private[laika] object ListParsers {

  private def listItem[I <: ListItem](itemStart: Parser[String], newListItem: Seq[Block] => I)(
      implicit recParsers: RecursiveParsers
  ): Parser[I] = {
    itemStart.count ~ ws.min(1).count >> { case startCount ~ wsCount =>
      recParsers.recursiveBlocks(
        (indentedBlock(minIndent = startCount + wsCount, maxIndent = startCount + wsCount) ~
          opt(blankLines | eof | lookAhead(itemStart))).evalMap {
          case block ~ None if block.lines.length < 2 => Left("not a list item")
          case block ~ _                              => Right(block)
        }
      ).map(newListItem)
    }
  }

  private def rewriteListItems[I <: BlockContainer](
      items: List[I],
      newListItem: (I, List[Block]) => I
  ): List[I] = {

    /* The reStructuredText reference parser makes a distinction between "simple" lists
     * and normal lists. The exact rules are not documented, but tests seem to hint at
     * a "simple" list being a list where all items only have a single paragraph and optionally
     * a nested list below the paragraph. The distinction has an influence on the HTML renderer
     * for example.
     */

    val isSimple = items.forall { con =>
      con.content match {
        case Paragraph(_, _) :: Nil                       => true
        case Paragraph(_, _) :: (_: ListContainer) :: Nil => true
        case _                                            => false
      }
    }

    if (isSimple) {
      items map { con =>
        con.content match {
          case Paragraph(content, opt) :: (nested: ListContainer) :: Nil =>
            newListItem(con, SpanSequence(content, opt) :: nested :: Nil)
          case _                                                         =>
            con
        }
      }
    }
    else {
      items map { con =>
        con.content match {
          case Paragraph(content, opt) :: Nil =>
            newListItem(con, ForcedParagraph(content, opt) :: Nil)
          case _                              => con
        }
      }
    }
  }

  private lazy val bulletListStart = oneOf('*', '-', '+', '\u2022', '\u2023', '\u2043')

  /** Parses a bullet list with any of the supported bullet characters.
    *
    * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#bullet-lists]].
    */
  lazy val bulletList: BlockParserBuilder = BlockParserBuilder.recursive { implicit recParsers =>
    lookAhead(bulletListStart <~ ws.min(1)) >> { symbol =>
      val bullet = StringBullet(symbol)
      listItem(literal(symbol), BulletListItem(_, bullet)).rep.min(1).map { items =>
        BulletList(
          rewriteListItems(items, (item: BulletListItem, content) => item.withContent(content)),
          bullet
        )
      }
    }
  }

  private lazy val enumListStart: Parser[(EnumFormat, Int)] = {
    import EnumType._
    val firstLowerRoman = (someOf('i', 'v', 'x', 'l', 'c', 'd', 'm').min(2) | oneOf('i')).evalMap {
      num => RomanNumerals.romanToInt(num.toUpperCase).map(_ -> LowerRoman)
    }

    val firstUpperRoman = (someOf('I', 'V', 'X', 'L', 'C', 'D', 'M').min(2) | oneOf('I')).evalMap {
      num => RomanNumerals.romanToInt(num.toUpperCase).map(_ -> UpperRoman)
    }

    val firstLowerAlpha = oneOf(range('a', 'h') ++ range('j', 'z')).map { char =>
      (char.charAt(0) + 1 - 'a', LowerAlpha)
    } // 'i' is interpreted as Roman numerical

    val firstUpperAlpha = oneOf(range('A', 'H') ++ range('J', 'Z')).map { char =>
      (char.charAt(0) + 1 - 'A', UpperAlpha)
    }

    val firstAutoNumber = oneOf('#').as((1, Arabic))
    val firstArabic     = someOf(CharGroup.digit).map(num => (num.toInt, Arabic))

    lazy val firstEnumType: Parser[(Int, EnumType)] =
      firstAutoNumber | firstArabic | firstLowerAlpha | firstUpperAlpha | firstLowerRoman | firstUpperRoman

    ("(" ~ firstEnumType ~ ")").map { case prefix ~ enumType ~ suffix =>
      (EnumFormat(enumType._2, prefix.toString, suffix.toString), enumType._1)
    } | (firstEnumType ~ ")" | firstEnumType ~ ".").map { case enumType ~ suffix =>
      (EnumFormat(enumType._2, "", suffix.toString), enumType._1)
    }
  }

  /** Parses an enumerated list in any of the supported combinations of enumeration style and formatting.
    *
    * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#enumerated-lists]].
    */
  lazy val enumList: BlockParserBuilder = BlockParserBuilder.recursive { implicit recParsers =>
    import EnumType._

    val lowerRoman = someOf('i', 'v', 'x', 'l', 'c', 'd', 'm')
    val upperRoman = someOf('I', 'V', 'X', 'L', 'C', 'D', 'M')
    val lowerAlpha = oneOf(CharGroup.lowerAlpha)
    val upperAlpha = oneOf(CharGroup.upperAlpha)
    val arabic     = someOf(CharGroup.digit)
    val autoNumber = oneOf('#')

    lazy val enumTypes = Map[EnumType, Parser[String]](
      Arabic     -> arabic,
      LowerAlpha -> lowerAlpha,
      UpperAlpha -> upperAlpha,
      LowerRoman -> lowerRoman,
      UpperRoman -> upperRoman
    )

    def enumType(et: EnumType) = enumTypes(et) | autoNumber

    def itemStart(format: EnumFormat): Parser[String] = {
      def literalOrEmpty(str: String) = if (str.nonEmpty) literal(str) else success("")
      (literalOrEmpty(format.prefix) ~ enumType(format.enumType) ~ literalOrEmpty(
        format.suffix
      )).source
    }

    lookAhead(enumListStart <~ ws.min(1)) >> { case (format, start) =>
      val pos = Iterator.from(start)
      listItem(itemStart(format), EnumListItem(_, format, pos.next())).rep.min(1).map { items =>
        EnumList(
          rewriteListItems(items, (item: EnumListItem, content) => item.withContent(content)),
          format,
          start
        )
      }
    }
  }

  /** Parses a definition list.
    *
    * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#definition-lists]].
    */
  lazy val definitionList: BlockParserBuilder = BlockParserBuilder.recursive { recParsers =>
    val tableStart    = anyOf(' ', '=') ~ eol
    val explicitStart = ".. " | "__ "
    val listStart     = (bulletListStart | enumListStart) ~ ws.min(1)
    val headerStart   = BaseParsers.punctuationChar.take(1) >> { start =>
      anyOf(start.charAt(0)).min(2) ~ wsEol
    }

    val term: Parser[String] =
      not(blankLine | tableStart | explicitStart | listStart | headerStart) ~>
        anyNot('\n') <~ eol ~ lookAhead(ws.min(1) ~ not(blankLine))

    val classifier = delimiter(" : ") ~> recParsers.recursiveSpans(anyChars.line).map(Classifier(_))
    val termWithClassifier = recParsers.recursiveSpans(term.line).embed(classifier)

    val item = (termWithClassifier ~ recParsers.recursiveBlocks(
      indentedBlock(firstLineIndented = true)
    )).collect { case termRes ~ blocks =>
      DefinitionListItem(termRes, blocks)
    }

    (item <~ opt(blankLines)).rep.min(1).map(DefinitionList(_))
  }

  /** Parses a field list.
    *
    * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#field-lists]].
    */
  lazy val fieldList: BlockParserBuilder = BlockParserBuilder.recursive { recParsers =>
    val nameParser = ":" ~> recParsers.escapedUntil(':').line <~ (lookAhead(eol).as("") | " ")

    val name    = recParsers.recursiveSpans(nameParser)
    val content = recParsers.recursiveBlocks(indentedBlock())

    val item = (name ~ content).mapN(Field(_, _))

    item.rep.min(1).map(FieldList(_))
  }

  /** Parses an option list.
    *
    * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#option-lists]].
    */
  lazy val optionList: BlockParserBuilder = BlockParserBuilder.recursive { recParsers =>
    val optionString = someOf(CharGroup.alphaNum.add('_').add('-'))
    val optionArg    = optionString | ("<" ~> delimitedBy('>')).map { "<" + _ + ">" }

    val gnu        = ("+" ~ oneOf(CharGroup.alphaNum)).source
    val shortPosix = ("-" ~ oneOf(CharGroup.alphaNum)).source
    val longPosix  = (("--" <~ nextNot('-')) ~ optionString).source
    val dos        = ("/" ~ optionString).source

    val arg = anyOf('=', ' ').max(1) ~ optionArg ^^ { case delim ~ argStr =>
      OptionArgument(argStr, delim)
    }

    val option = ((gnu | shortPosix | longPosix | dos) ~ opt(arg)).mapN(ProgramOption(_, _))

    val options = option.rep(", ").min(1)

    val descStart = (anyOf(' ').min(2) ~ not(blankLine)) | lookAhead(
      blankLine ~ ws.min(1) ~ not(blankLine)
    ).as("")

    val item = (options ~ (descStart ~> recParsers.recursiveBlocks(indentedBlock()))).mapN(
      OptionListItem(_, _)
    )

    (item <~ opt(blankLines)).rep.min(1).map(OptionList(_))
  }

  /** Parses a block of lines with line breaks preserved.
    *
    * See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#line-blocks]].
    */
  lazy val lineBlock: BlockParserBuilder = BlockParserBuilder.recursive { recParsers =>
    val itemStart = oneOf('|')

    val line: Parser[Int ~ Line] =
      itemStart ~> ws.min(1).count ~ recParsers.recursiveSpans(
        indentedBlock(endsOnBlankLine = true)
      ).map(Line(_))

    def nest(lines: Seq[Int ~ Line]): LineBlock = {

      val stack = new Stack[(ListBuffer[LineBlockItem], Int)]

      @tailrec
      def addItem(item: LineBlockItem, level: Int): Unit = {
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

      lines.foreach { case level ~ line =>
        addItem(line, level)
      }

      val (topBuffer, _) = stack.elements.reduceLeft { (top, next) =>
        next._1 += LineBlock(top._1.toList)
        next
      }

      LineBlock(topBuffer.toList)
    }

    line.rep.min(1).map(nest)
  }

}
