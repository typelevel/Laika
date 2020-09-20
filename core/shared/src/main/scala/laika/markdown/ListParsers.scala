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

package laika.markdown

import laika.ast._
import laika.bundle.{BlockParser, BlockParserBuilder}
import laika.parse.Parser
import laika.parse.combinator.Parsers.opt
import laika.parse.markup.RecursiveParsers
import laika.parse.text.{CharGroup, PrefixedParser}
import laika.parse.builders._

/** Provides parsers for bullet lists ("unordered list" in the Markdown spec)
  * and enumerated lists ("ordered list" in the Markdown spec).
  *
  * @author Jens Halm
  */
object ListParsers {

  private val bulletChar: Parser[String] = oneOf('*','-','+')
  private val enumChar: Parser[String] = oneOf(CharGroup.digit)

  private val wsAfterItemStart: Parser[Unit] = someOf(' ', '\t').void
  private val enumStartRest: Parser[String] = (anyOf(CharGroup.digit) ~ "." ~ wsAfterItemStart).as("")

  /** Parses the start of a bullet list item.
    */
  val bulletListItemStart: Parser[String] = bulletChar <~ wsAfterItemStart

  /** Parses the start of an enumerated list item.
    */
  val enumListItemStart: Parser[String] = enumChar ~> enumStartRest

  /** Parses a list based on the specified helper parsers.
    *
    *  @param itemStartChar the parser for the character that starts a list item
    *  @param itemStartRest parser that recognizes the start of a list item after the first character, result will be discarded
    *  @param newList function that produces a block element for the document tree
    *  @param newItem function that produces a new list item element based on position and content arguments
    */
  def list [T <: Block, I <: ListItem] (itemStartChar: Parser[Any],
                                        itemStartRest: Parser[Any],
                                        newList: List[I] => T,
                                        newItem: (Int, Seq[Block]) => I)(implicit recParsers: RecursiveParsers): Parser[T] = {

    def flattenItems (firstItem: Seq[Block], items: List[Boolean ~ Seq[Block]]) = {

      val hasBlankLines = items.exists(_._1)
      val blockItems = firstItem +: items.map(_._2)

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
      val pos = Iterator.from(1)
      blockItems map { item => rewriteItemContent(item, pos.next()) }
    }

    val rule = BlockParsers.rules.createParser(recParsers).parser

    val itemStart = itemStartChar ~ itemStartRest

    val listItem: Parser[Seq[Block]] = recParsers.recursiveBlocks2(BlockParsers.mdBlock2(
      not(rule) ~> itemStart, not(blankLine | itemStart) ~ opt(BlockParsers.tabOrSpace), BlockParsers.tabOrSpace
    ))

    listItem ~ (opt(blankLines).map(_.isDefined) ~ listItem).rep ^^
      { case firstItem ~ items => newList(flattenItems(firstItem, items)) }
  }

  /** Parses a bullet list, called "unordered list" in the Markdown syntax description.
    */
  val bulletLists: BlockParserBuilder = 
    BlockParser.recursive { implicit recParsers =>
      PrefixedParser('+', '*', '-') {
        lookAhead(oneChar) >> { char =>
          val bullet = StringBullet(char)
          list(bulletChar, wsAfterItemStart, BulletList(_: List[BulletListItem], bullet), (_, blocks) => BulletListItem(blocks, bullet))
        }
      }
    }.withLowPrecedence

  /** Parses an enumerated list, called "ordered list" in the Markdown syntax description.
    */
  val enumLists: BlockParserBuilder = 
    BlockParser.recursive { implicit recParsers =>
      PrefixedParser(CharGroup.digit) {
        list(enumChar, enumStartRest, EnumList(_: List[EnumListItem], EnumFormat()), (pos, blocks) => EnumListItem(blocks, EnumFormat(), pos))
      }
    }

}
