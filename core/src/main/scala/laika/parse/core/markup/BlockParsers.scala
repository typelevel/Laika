/*
 * Copyright 2013-2016 the original author or authors.
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

package laika.parse.core.markup

import laika.parse.core._
import laika.parse.core.text.TextParsers._
import laika.util.~
  
/** A generic base trait for block parsers. Provides base parsers that abstract
 *  aspects of block parsing common to most lightweight markup languages.
 *  
 *  It contains simple parsers for parsing chunks that make up blocks (text
 *  lines, blank lines, etc.) as well as a convenient `block` helper parser
 *  that simplifies parsing of full blocks.
 *  
 *  Concrete sub-traits can specify the block parsers for a specific markup
 *  format by overriding `prepareBlockParsers` and implementing 
 *  `nonRecursiveBlock`.
 * 
 *  A block parser in Laika can always safely assume that it is invoked at 
 *  the start of the current line and that the line is not empty.
 * 
 *  @author Jens Halm
 */
object BlockParsers {

  
  def mergeLines (p: Parser[Seq[String]]): Parser[String] = p ^^ (_.mkString("\n"))
  def mergeIndentedLines (p: Parser[IndentedBlock]): Parser[String] = p ^^ (_.lines.mkString("\n"))


  /** Parses a full block based on the specified helper parsers.
   * 
   *  @param firstLinePrefix parser that recognizes the start of the first line of this block
   *  @param linePrefix parser that recognizes the start of subsequent lines that still belong to the same block
   *  @param nextBlockPrefix parser that recognizes whether a line after one or more blank lines still belongs to the same block 
   */
  def block (firstLinePrefix: Parser[Any], linePrefix: => Parser[Any], nextBlockPrefix: => Parser[Any]): Parser[List[String]] = {
    
    val firstLine = firstLinePrefix ~> restOfLine
    
    lazy val line = linePrefix ~> restOfLine
    
    lazy val nextBlock = blankLines <~ lookAhead(nextBlockPrefix) ^^ { _.mkString("\n") }
    
    firstLine ~ ( (line | nextBlock)* ) ^^ mkList
    
  }
  
  case class IndentedBlock (minIndent: Int, lines: List[String])
  
  /** Parses a full block based on the specified helper parsers, expecting an indentation for
   *  all lines except the first. The indentation may vary between the parts of the indented
   *  block, so that this parser only cuts off the minimum indentation shared by all lines,
   *  but each line must have at least the specified minimum indentation.
   * 
   *  @param minIndent the minimum indentation that each line in this block must have
   *  @param linePredicate parser that recognizes the start of subsequent lines that still belong to the same block
   *  @param endsOnBlankLine indicates whether parsing should end on the first blank line
   *  @param firstLineIndented indicates whether the first line is expected to be indented, too
   *  @param maxIndent the maximum indentation that will get dropped from the start of each line of the result
   *  @return a parser that produces an instance of IndentedBlock
   */
  def indentedBlock (minIndent: Int = 1,
                     linePredicate: => Parser[Any] = success(()), 
                     endsOnBlankLine: Boolean = false,
                     firstLineIndented: Boolean = false,
                     maxIndent: Int = Int.MaxValue): Parser[IndentedBlock] = {
    
    import scala.math._
    
    abstract class Line extends Product { def curIndent: Int }
    case class BlankLine(curIndent: Int) extends Line
    case class IndentedLine(curIndent: Int, indent: Int, text: String) extends Line
    case class FirstLine(text: String) extends Line { val curIndent = Int.MaxValue }
    
    val composedLinePredicate = not(blankLine) ~ linePredicate
    
    def lineStart (curIndent: Int) = ((ws min minIndent max curIndent) ^^ {_.length}) <~ composedLinePredicate
    
    def textLine (curIndent: Int) = (lineStart(curIndent) ~ (ws ^^ {_.length}) ~ restOfLine) ^^ { 
      case indent1 ~ indent2 ~ text => List(IndentedLine(min(min(indent1, curIndent), maxIndent), indent1 + indent2, text.trim)) 
    }
    
    def emptyLines (curIndent: Int) = blankLines <~ lookAhead(lineStart(curIndent)) ^^ {
      res => Stream.fill(res.length)(BlankLine(curIndent)).toList 
    }
    
    val firstLine = 
      if (firstLineIndented) textLine(Int.MaxValue) 
      else restOfLine ^^ { s => List(FirstLine(s)) }
    
    val firstLineGuard = if (firstLineIndented) ((ws min minIndent) ^^ {_.length}) ~ composedLinePredicate else success(())
    
    def nextLine (prevLines: List[Line]) = 
      if (endsOnBlankLine) textLine(prevLines.head.curIndent)
      else textLine(prevLines.head.curIndent) | emptyLines(prevLines.head.curIndent) 
      
    def result (lines: List[List[Line]]): (Int, List[String]) = if (lines.isEmpty) (minIndent,Nil) else {
      val minIndent = lines.last.head.curIndent
      (minIndent, lines.flatten map {
        case FirstLine(text)             => text
        case IndentedLine(_,indent,text) => " " * (indent - minIndent) + text
        case BlankLine(_)                => ""  
      })
    }

    lookAhead(firstLineGuard) ~> rep(firstLine, nextLine) ^^ { parsed =>
      val (minIndent, lines) = result(parsed)
      IndentedBlock(minIndent, lines)
    }
  }
  
  
}
