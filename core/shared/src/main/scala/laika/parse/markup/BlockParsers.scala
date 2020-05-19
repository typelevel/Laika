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
import laika.parse.Parser
import laika.parse.builders._
import laika.parse.implicits._

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
  def block (firstLinePrefix: Parser[Any], linePrefix: => Parser[Any]): Parser[String] =
    block(firstLinePrefix, linePrefix, blankLineEndsBlock)
  
  
  /** Parses a full block based on the specified helper parsers.
    *
    * The string result of this parser will not contain the characters consumed by any of the specified prefix
    * parsers.
    *
    *  @param firstLinePrefix parser that recognizes the start of the first line of this block
    *  @param linePrefix parser that recognizes the start of subsequent lines that still belong to the same block
    *  @param nextBlockPrefix parser that recognizes whether a line after one or more blank lines still belongs to the same block
    */
  def block (firstLinePrefix: Parser[Any], linePrefix: => Parser[Any], nextBlockPrefix: => Parser[Any]): Parser[String] = {
    
    val firstLine = firstLinePrefix ~> restOfLine
    
    lazy val line = linePrefix ~> restOfLine
    
    lazy val nextBlock = (blankLines <~ lookAhead(nextBlockPrefix)).mkLines
    
    (firstLine ~ (line | nextBlock).rep).concat.mkLines
    
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
                     maxIndent: Int = Int.MaxValue): Parser[String] =
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
                              maxIndent: Int = Int.MaxValue): Parser[(String, Int)] = {
    
    import scala.math._
    
    abstract class Line extends Product { def curIndent: Int }
    case class BlankLine(curIndent: Int) extends Line
    case class IndentedLine(curIndent: Int, indent: Int, text: String) extends Line
    case class FirstLine(text: String) extends Line { val curIndent = Int.MaxValue }
    
    val composedLinePredicate = not(blankLine) ~ linePredicate
    
    def lineStart (curIndent: Int) = ws.min(minIndent).max(curIndent).count <~ composedLinePredicate
    
    def textLine (curIndent: Int) = (lineStart(curIndent) ~ ws.count ~ restOfLine).map {
      case indent1 ~ indent2 ~ text => List(IndentedLine(min(min(indent1, curIndent), maxIndent), indent1 + indent2, text.trim)) 
    }
    
    def emptyLines (curIndent: Int) = blankLines <~ lookAhead(lineStart(curIndent)) ^^ {
      res => List.fill(res.length)(BlankLine(curIndent))
    }
    
    val firstLine = 
      if (firstLineIndented) textLine(Int.MaxValue) 
      else restOfLine.map(s => List(FirstLine(s)))
    
    val firstLineGuard = if (firstLineIndented) ws.min(minIndent).count ~ composedLinePredicate else success(())
    
    def nextLine (prevLines: List[Line]) = 
      if (endsOnBlankLine) textLine(prevLines.head.curIndent)
      else textLine(prevLines.head.curIndent) | emptyLines(prevLines.head.curIndent) 
      
    def result (lines: List[List[Line]]): (String, Int) = if (lines.isEmpty) ("", minIndent) else {
      val minIndent = lines.last.head.curIndent
      (lines.flatten map {
        case FirstLine(text)             => text
        case IndentedLine(_,indent,text) => " " * (indent - minIndent) + text
        case BlankLine(_)                => ""  
      } mkString "\n", minIndent)
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
