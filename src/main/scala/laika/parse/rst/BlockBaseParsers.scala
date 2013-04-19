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
import scala.annotation.tailrec
import scala.util.parsing.input.CharSequenceReader

/** Base parsers used by all of the various block-level parser traits.
 * 
 *  @author Jens Halm
 */
trait BlockBaseParsers extends laika.parse.BlockParsers {

  
  /** The maximum level of block nesting. Some block types like lists
   *  and blockquotes contain nested blocks. To protect against malicious
   *  input or accidentally broken markup, the level of nesting is restricted.
   */
  val maxNestLevel: Int = 12
  
  
  /** Parses reStructuredText blocks, except for blocks that allow nesting of blocks. 
   *  Only used in rare cases when the maximum nest level allowed had been reached
   */
  def nonRecursiveBlock: Parser[Block]
  
  
  override def ws = anyOf(' ') // other whitespace has been replaced with spaces by preprocessor

  
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
  
  
  /** Parses all nested blocks inside the specified indented block.
   */
  def parseNestedBlocks (block: IndentedBlock): List[Block] = 
    parseNestedBlocks(block.lines, block.nestLevel)
  
  /** Parses all nested blocks for the specified input and nest level.
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
    val block = parser <~ opt(blankLines) 
    
    parseMarkup(opt(blankLines) ~> (block *), reader) match {
      case Paragraph(content) :: Nil => FlowContent(content) :: Nil
      case other => other
    }
  }
  
    
  /** Parses a full block based on the specified helper parsers, expecting an indentation for
   *  all lines except the first. The indentation must be as specified by the first parameter
   *  for all lines of these blocks.
   * 
   *  @param fixedIndent the indentation that each line in this block must have
   *  @param linePrefix parser that recognizes the start of subsequent lines that still belong to the same block
   *  @param nextBlockPrefix parser that recognizes whether a line after one or more blank lines still belongs to the same block 
   *  @return a parser that produces an instance of IndentedBlock
   */
  def fixedIndentedBlock (fixedIndent: Int = 1,
                     linePrefix: => Parser[Any] = not(blankLine), 
                     nextBlockPrefix: => Parser[Any] = not(blankLine)): Parser[IndentedBlock] = {
    
    lazy val line = (ws take fixedIndent) ~ linePrefix ~> restOfLine
    
    lazy val nextBlock = blankLines <~ guard(nextBlockPrefix) ^^ { _.mkString("\n") }
    
    withNestLevel {
      restOfLine ~ ( (line | nextBlock)* ) ^^ { res => (fixedIndent, mkList(res)) }
    } ^^ { case (nestLevel, (indent, lines)) => IndentedBlock(nestLevel, indent, lines) }
  }
  
  case class IndentedBlock (nestLevel: Int, minIndent: Int, lines: List[String])
  
  /** Parses a full block based on the specified helper parsers, expecting an indentation for
   *  all lines except the first. The indentation may vary between the parts of the indented
   *  block, so that this parser only cuts off the minimum indentation shared by all lines,
   *  but each line must have at least the specified minimum indentation.
   * 
   *  @param minIndent the minimum indentation that each line in this block must have
   *  @param linePrefix parser that recognizes the start of subsequent lines that still belong to the same block
   *  @param nextBlockPrefix parser that recognizes whether a line after one or more blank lines still belongs to the same block 
   *  @param firstLineIndented indicates whether the first line is expected to be indented, too
   *  @return a parser that produces an instance of IndentedBlock
   */
  def varIndentedBlock (minIndent: Int = 1,
                     linePrefix: => Parser[Any] = not(blankLine), 
                     nextBlockPrefix: => Parser[Any] = not(blankLine),
                     firstLineIndented: Boolean = false): Parser[IndentedBlock] = {
    
    lazy val line = (((ws min minIndent) ^^ (_.length)) ~ 
                    (linePrefix ~> restOfLine)) ^^ { case indent ~ text => (indent, text.trim) }

    val firstLine = if (firstLineIndented) line else restOfLine ^^ { (-1, _) }
    
    lazy val nextBlock = blankLines <~ guard(nextBlockPrefix) ^^ { res => (-1, res.mkString("\n")) }
    
    withNestLevel {
      firstLine ~ ( (line | nextBlock)* ) ^^ { res => 
        val lines = mkList(res)
        val indents = lines map (_._1) filter (_ != -1)
        val minIndent = if (indents.isEmpty) 0 else indents min;
        (minIndent, lines map (line => if (line._1 == -1) line._2 else " " * (line._1 - minIndent) + line._2))
      }
    } ^^ { case (nestLevel, (indent, lines)) => IndentedBlock(nestLevel, indent, lines) }
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
  
  
}