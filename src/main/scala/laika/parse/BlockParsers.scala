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

package laika.parse
  
import laika.tree.Elements.{Block,Document}
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.input.Reader
  
/** A generic base trait for block parsers. Provides base parsers that abstract
 *  aspects of block parsing common to most lightweight markup languages.
 *  
 *  It contains simple parsers for parsing chunks that make up blocks (text
 *  lines, blank lines, etc.) as well as a convenient `block` helper parser
 *  that simplifies parsing of full blocks.
 *  
 *  The only abstract members are the `topLevelBlock`, `nestedBlock` and
 *  `nonRecursiveBlock` parsers that the other parsers delegate to.
 * 
 *  A block parser in Laika can always safely assume that it is invoked at 
 *  the start of the current line and that the line is not empty.
 * 
 *  @author Jens Halm
 */
trait BlockParsers extends MarkupParsers {

  
  /** The maximum level of block nesting. Some block types like lists
   *  and blockquotes contain nested blocks. To protect against malicious
   *  input or accidentally broken markup, the level of nesting is restricted.
   */
  val maxNestLevel: Int = 12
  
   
  /** Parses any kind of top-level block supported by a concrete markup language.
   */
  def topLevelBlock: Parser[Block]
 
  /** Parses any kind of nested block supported by a concrete markup language.
   */
  def nestedBlock: Parser[Block]
 
  /** Parses reStructuredText blocks, except for blocks that allow nesting of blocks. 
   *  Only used in rare cases when the maximum nest level allowed had been reached
   */
  def nonRecursiveBlock: Parser[Block]

  
  /** Parses a full document, delegating most of the work to the `topLevelBlock` parser.
   */
  def document: Parser[Document] = opt(blankLines) ~> blockList(topLevelBlock) ^^ Document
  
  /** Fully parses the input from the specified reader and returns the document tree. 
   *  This function is expected to always succeed, errors would be considered a bug
   *  of this library, as the parsers treat all unknown or malformed markup as regular
   *  text.
   */
  def parseDocument (reader: Reader[Char]): Document = parseMarkup(document, reader)
   
  
  /** Parses all nested blocks for the specified input and nest level.
   *  Delegates to the abstract `nestedBlock` parser that sub-traits need to define.
   *  The nest level is primarily used as a protection against malicious
   *  input that forces endless recursion.
   * 
   *  @param lines the input to parse
   *  @param nestLevel the level of nesting with 0 being the outermost level
   *  @return the parser result as a list of blocks
   */
  def parseNestedBlocks (lines: List[String], nestLevel: Int): List[Block] = {
    val parser = if (nestLevel < maxNestLevel) nestedBlock else nonRecursiveBlock 
    val reader = new NestedCharSequenceReader(nestLevel + 1, lines mkString "\n")
    val blocks = blockList(parser) 
    
    parseMarkup(opt(blankLines) ~> blocks, reader)
  }
  
  /** Parses all nested blocks inside the specified indented block.
   */
  def parseNestedBlocks (block: IndentedBlock): List[Block] = 
    parseNestedBlocks(block.lines, block.nestLevel)
  
  /** Builds a parser for a list of blocks based on the parser for a single block.
   */
  def blockList (p: => Parser[Block]): Parser[List[Block]] = (p <~ opt(blankLines))*
  
  /** Reader implementation that keeps the current nest level in case
   *  of recursive parsing of block-level elements.
   * 
   *  @param nestLevel the nest level of the parser this reader is used with, 0 being the outermost
   *  @param src the character source to read from
   *  @param off the offset position this reader reads from
   */
  class NestedCharSequenceReader (val nestLevel: Int, 
                                  src: java.lang.CharSequence,
                                  off: Int) extends CharSequenceReader(src, off) {
    
    def this (nestLevel: Int, src: java.lang.CharSequence) = this(nestLevel, src, 0)
    
    override def rest: CharSequenceReader =
      if (offset < source.length) new NestedCharSequenceReader(nestLevel, source, offset + 1)
      else this
      
    override def drop(n: Int): CharSequenceReader =
      new NestedCharSequenceReader(nestLevel, source, offset + n)  
      
  }

  /** Creates a new parser that produces a tuple containing the current nest
   *  level as well as the result from the specified parser.
   * 
   *  The nest level is usually only used to prevent endless recursion of nested blocks. 
   */
  def withNestLevel [T] (p: => Parser[T]) = Parser { in =>
    p(in) match {
      case Success(res, next) => Success((nestLevel(next), res), next)
      case ns: NoSuccess      => ns
    }
  }
  
  /** Returns the current nest level from the specified input or 0 if it cannot be determined.
   * 
   *  The nest level is usually only used to prevent endless recursion of nested blocks. 
   */
  def nestLevel (reader: Input) = reader match {
    case nested: NestedCharSequenceReader => nested.nestLevel
    case _ => 0
  }
  
  /** Parses a single text line from the current input offset (which may not be at the
   *  start of the line. Fails for blank lines. Does not include the eol character(s).
   */
  val textLine: Parser[String] = not(blankLine) ~> restOfLine
  
  /** Parses a blank line from the current input offset (which may not be at the
   *  start of the line). Fails for lines that contain any non-whitespace character.
   *  Does always produce an empty string as the result, discarding any whitespace
   *  characters found in the line. 
   *  
   *  Since it also succeeds at the end of the input
   *  it should never be used in the form of `(blankLine *)` or `(blankLine +)`. Use
   *  the `blankLines` parser instead in these cases.
   */
  val blankLine: Parser[String] = ws ~ eol ^^^ ""
  
  /** Parses one or more blanklines, producing a list of empty strings corresponding
   *  to the number of blank lines consumed.
   */
  val blankLines = (not(eof) ~> blankLine)+

  /** Parses the rest of the line from the current input offset no matter whether
   *  it consist of whitespace only or some text. Does not include the eol character(s).
   */
  def restOfLine: Parser[String] = anyBut('\n','\r') <~ eol
  
  
  /** Parses a full block based on the specified helper parsers.
   * 
   *  @param firstLinePrefix parser that recognizes the start of the first line of this block
   *  @param linePrefix parser that recognizes the start of subsequent lines that still belong to the same block
   *  @param nextBlockPrefix parser that recognizes whether a line after one or more blank lines still belongs to the same block 
   */
  def block (firstLinePrefix: Parser[Any], linePrefix: => Parser[Any], nextBlockPrefix: => Parser[Any]): Parser[List[String]] = {
    
    val firstLine = firstLinePrefix ~> restOfLine
    
    lazy val line = linePrefix ~> restOfLine
    
    lazy val nextBlock = blankLines <~ guard(nextBlockPrefix) ^^ { _.mkString("\n") }
    
    firstLine ~ ( (line | nextBlock)* ) ^^ mkList
    
  }
  
  case class IndentedBlock (nestLevel: Int, minIndent: Int, lines: List[String])
  
  /** Parses a full block based on the specified helper parsers, expecting an indentation for
   *  all lines except the first. The indentation may vary between the parts of the indented
   *  block, so that this parser only cuts off the minimum indentation shared by all lines,
   *  but each line must have at least the specified minimum indentation.
   * 
   *  @param minIndent the minimum indentation that each line in this block must have
   *  @param linePredicate parser that recognizes the start of subsequent lines that still belong to the same block
   *  @param endsOnBlankLine indicates whether parsing should end on the first blank line
   *  @param firstLineIndented indicates whether the first line is expected to be indented, too
   *  @return a parser that produces an instance of IndentedBlock
   */
  def indentedBlock (minIndent: Int = 1,
                     linePredicate: => Parser[Any] = success(), 
                     endsOnBlankLine: Boolean = false,
                     firstLineIndented: Boolean = false): Parser[IndentedBlock] = {
    
    import scala.math._
    
    abstract class Line extends Product { def curIndent: Int }
    case class BlankLine(curIndent: Int) extends Line
    case class IndentedLine(curIndent: Int, indent: Int, text: String) extends Line
    case class FirstLine(text: String) extends Line { val curIndent = Int.MaxValue }
    
    val composedLinePredicate = not(blankLine) ~ linePredicate
    
    def lineStart (curIndent: Int) = ((ws min minIndent max curIndent) ^^ {_.length}) <~ composedLinePredicate
    
    def textLine (curIndent: Int) = (lineStart(curIndent) ~ (ws ^^ {_.length}) ~ restOfLine) ^^ { 
      case indent1 ~ indent2 ~ text => List(IndentedLine(min(indent1, curIndent), indent1 + indent2, text.trim)) 
    }
    
    def emptyLines (curIndent: Int) = blankLines <~ guard(lineStart(curIndent)) ^^ {
      res => Stream.fill(res.length)(BlankLine(curIndent)).toList 
    }
    
    val firstLine = 
      if (firstLineIndented) textLine(Int.MaxValue) 
      else restOfLine ^^ { s => List(FirstLine(s)) }
    
    val firstLineGuard = if (firstLineIndented) ((ws min minIndent) ^^ {_.length}) ~ composedLinePredicate else success()
    
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

    guard(firstLineGuard) ~> withNestLevel(rep(firstLine, nextLine)) ^^ { case (nestLevel,parsed) => {
      val (minIndent, lines) = result(parsed)
      IndentedBlock(nestLevel, minIndent, lines)
    }}
  }
  
  
}
