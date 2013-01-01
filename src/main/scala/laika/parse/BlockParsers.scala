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
import scala.util.parsing.input.Reader
  
/** A generic base trait for block parsers. Provides base parsers that abstract
 *  aspects of block parsing common to most lightweight markup languages.
 *  
 *  It contains simple parsers for parsing chunks that make up blocks (text
 *  lines, blank lines, etc.) as well as a convenient `block` helper parser
 *  that simplifies parsing of full blocks.
 *  
 *  The only abstract members are the `topLevelBlock` and `nestedBlock` parsers
 *  that the other parsers delegate to.
 * 
 *  @author Jens Halm
 */
trait BlockParsers extends MarkupParsers {

   
  /** Parses any kind of top-level block supported by a concrete markup language.
   */
  def topLevelBlock: Parser[Block]
 
  /** Parses any kind of nested block supported by a concrete markup language.
   */
  def nestedBlock: Parser[Block]
  
  
  /** Parses a full document, delegating most of the work to the `topLevelBlock` parser.
   */
  def document: Parser[Document] = {
    val block = topLevelBlock <~ opt(blankLines)
  	opt(blankLines) ~> (block *) ^^ { Document(_) }
  }
  
  /** Fully parses the input from the specified reader and returns the document tree. 
   *  This function is expected to always succeed, errors would be considered a bug
   *  of this library, as the parsers treat all unknown or malformed markup as regular
   *  text.
   */
  def parseDocument (reader: Reader[Char]): Document = parseMarkup(document, reader)
   
  /** Fully parses the list of lines and returns a list of blocks, delegating to the
   *  abstract `nestedBlock` parser that sub-traits need to define.
   *  
   *  This function is expected to always succeed, errors would be considered a bug
   *  of this library, as the parsers treat all unknown or malformed markup as regular
   *  text.
   */
  def parseNestedBlocks (lines: List[String]): List[Block] = {
    parseNestedBlocks(lines, nestedBlock)
  }
  
  /** Fully parses the list of lines and returns a list of blocks, using the specified
   *  parser for parsing individual blocks instead of the default `nestedBlock` parser.
   *  
   *  This function is expected to always succeed, errors would be considered a bug
   *  of this library, as the parsers treat all unknown or malformed markup as regular
   *  text.
   */
  def parseNestedBlocks (lines: List[String], parser: Parser[Block]): List[Block] = {
    val block = parser <~ opt(blankLines)
    parseMarkup(block *, lines mkString "\n")
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
   *  it should never be used in the form of (blankLine *) or (blankLine +). Use
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
  def block (firstLinePrefix: Parser[Any], linePrefix: Parser[Any], nextBlockPrefix: Parser[Any]): Parser[List[String]] = {
    
    val firstLine = firstLinePrefix ~> restOfLine
    
    val line = linePrefix ~> restOfLine
    
    val nextBlock = blankLines <~ guard(nextBlockPrefix) ^^ { _.mkString("\n") }
    
    firstLine ~ ( (line | nextBlock)* ) ^^ { mkList(_) }
    
  }
  
  
}
