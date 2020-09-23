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

package laika.parse.markup

import laika.ast.~
import laika.parse.builders._
import laika.parse.implicits._
import laika.parse._

/** Provides base parsers that abstract aspects of block parsing common to most lightweight markup languages.
 *  
 *  A block parser in Laika can always safely assume that it is invoked at
 *  the start of the current line and that the line is not empty.
 * 
 *  @author Jens Halm
 */
trait BlockParsers {
  
  
  private val blankLineEndsBlock: Parser[Any] = failure("Blank line ends this block element")

  /** Parses a full block based on the specified helper parsers.
    *
    * The string result of this parser will not contain the characters consumed by any of the specified prefix
    * parsers.
    *
    *  @param firstLinePrefix parser that recognizes the start of the first line of this block
    *  @param linePrefix parser that recognizes the start of subsequent lines that still belong to the same block
    */
  def block (firstLinePrefix: Parser[Any], linePrefix: => Parser[Any]): Parser[BlockSource] =
    block(firstLinePrefix, linePrefix, blankLineEndsBlock)

  /** Parses a full block based on the specified helper parsers.
    *
    * The result of this parser will not contain the characters consumed by any of the specified prefix
    * parsers.
    * The block source returned by this method maintains x-offsets for each individual line so that it can be
    * passed down to recursive block or span parsers without losing position tracking.
    *
    * @param firstLinePrefix parser that recognizes the start of the first line of this block
    * @param linePrefix parser that recognizes the start of subsequent lines that still belong to the same block
    * @param nextBlockPrefix parser that recognizes whether a line after one or more blank lines still belongs to the same block
    */
  def block (firstLinePrefix: Parser[Any], linePrefix: => Parser[Any], nextBlockPrefix: => Parser[Any]): Parser[BlockSource] = {

    val firstLine = firstLinePrefix ~> restOfLine.line

    lazy val line: Parser[LineSource] = linePrefix ~> restOfLine.line

    lazy val nextBlock: Parser[LineSource] = blankLines.mkLines.line <~ lookAhead(nextBlockPrefix)

    (firstLine ~ (line | nextBlock).rep).map {
      case first ~ rest => BlockSource(first, rest:_*)
    }
  }

  /**  Parses a full block based on the specified helper parsers, expecting an indentation for
    *  all lines except the first. The indentation may vary between the parts of the indented
    *  block, so that this parser only cuts off the minimum indentation shared by all lines,
    *  but each line must have at least the specified minimum indentation.
    *
    *  @param minIndent the minimum indentation that each line in this block must have
    *  @param linePredicate parser that recognizes the start of subsequent lines that still belong to the same block
    *  @param endsOnBlankLine indicates whether parsing should end on the first blank line
    *  @param firstLineIndented indicates whether the first line is expected to be indented, too
    *  @param maxIndent the maximum indentation that will get dropped from the start of each line of the result
    *  @return a parser that produces the raw text of the parsed block with the indentation removed
    */
  def indentedBlock (minIndent: Int = 1,
                     linePredicate: => Parser[Any] = success(()),
                     endsOnBlankLine: Boolean = false,
                     firstLineIndented: Boolean = false,
                     maxIndent: Int = Int.MaxValue): Parser[BlockSource] =
    indentedBlockWithLevel(minIndent, linePredicate, endsOnBlankLine, firstLineIndented, maxIndent).map(_._1)

  /**  Parses a full block based on the specified helper parsers, expecting an indentation for
    *  all lines except the first. The indentation may vary between the parts of the indented
    *  block, so that this parser only cuts off the minimum indentation shared by all lines,
    *  but each line must have at least the specified minimum indentation.
    *
    *  @param minIndent the minimum indentation that each line in this block must have
    *  @param linePredicate parser that recognizes the start of subsequent lines that still belong to the same block
    *  @param endsOnBlankLine indicates whether parsing should end on the first blank line
    *  @param firstLineIndented indicates whether the first line is expected to be indented, too
    *  @param maxIndent the maximum indentation that will get dropped from the start of each line of the result
    *  @return a parser that produces the raw text of the parsed block with the indentation removed and the
    *          indentation level (number of whitespace characters removed from the text lines)
    */
  def indentedBlockWithLevel (minIndent: Int = 1,
                              linePredicate: => Parser[Any] = success(()),
                              endsOnBlankLine: Boolean = false,
                              firstLineIndented: Boolean = false,
                              maxIndent: Int = Int.MaxValue): Parser[(BlockSource, Int)] = {

    import scala.math._

    abstract class Line extends Product { 
      def curIndent: Int
      def source: LineSource
    }
    case class BlankLine(curIndent: Int, source: LineSource) extends Line
    case class IndentedLine(curIndent: Int, indent: Int, source: LineSource) extends Line
    case class FirstLine(source: LineSource) extends Line { val curIndent: Int = Int.MaxValue }

    val composedLinePredicate = not(blankLine) ~ linePredicate

    def lineStart (curIndent: Int) = ws.min(minIndent).max(curIndent).count <~ composedLinePredicate

    def textLine (curIndent: Int): Parser[Seq[Line]] = (lineStart(curIndent) ~ ws.count ~ restOfLine.trim.line).map {
      case indent1 ~ indent2 ~ text => List(IndentedLine(min(min(indent1, curIndent), maxIndent), indent1 + indent2, text))
    }

    val blankLines: Parser[Seq[LineSource]] = (not(eof) ~> blankLine.line).rep.min(1)

    def emptyLines (curIndent: Int): Parser[Seq[Line]] = blankLines <~ lookAhead(lineStart(curIndent)) ^^ {
      _.map(line => BlankLine(curIndent, line))
    }

    val firstLine =
      if (firstLineIndented) textLine(Int.MaxValue)
      else restOfLine.line.map(s => List(FirstLine(s)))

    val firstLineGuard = if (firstLineIndented) ws.min(minIndent).count ~ composedLinePredicate else success(())

    def nextLine (prevLines: Seq[Line]) =
      if (endsOnBlankLine) textLine(prevLines.head.curIndent)
      else textLine(prevLines.head.curIndent) | emptyLines(prevLines.head.curIndent)

    def result (lines: Seq[Seq[Line]]): (BlockSource, Int) = {
      val minIndent = lines.last.head.curIndent
      val adjustedLines: Seq[LineSource] = lines.flatten.map {
        case FirstLine(src)             => src
        case BlankLine(_, src)          => src
        case IndentedLine(_,indent,src) =>
          val extraIndent = indent - minIndent
          new LineSource(" " * extraIndent + src.input, src.root.consume(extraIndent * -1), src.offset, src.nestLevel)
      }
      (BlockSource(adjustedLines.head, adjustedLines.tail:_*), minIndent)
    }

    lookAhead(firstLineGuard) ~> firstLine.repWith(nextLine) ^^ result
  }

}

/** Instance that allows to import all block parsers in isolation.
  *
  * Usually it is more convenient to import laika.parse.api._ 
  * to get all parser builders with one import.
  */
object BlockParsers extends BlockParsers
