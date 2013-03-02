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

/**
 * @author Jens Halm
 */
trait BlockBaseParsers extends laika.parse.BlockParsers {

  
  /** The maximum level of block nesting. Some block types like lists
   *  and blockquotes contain nested blocks. To protect against malicious
   *  input or accidentally broken markup, the level of nesting is restricted.
   */
  val maxNestLevel: Int = 12
  
  
  /** Parses all of the standard reStructuredText blocks, except normal paragraphs. 
   * 
   *  @param pos the current parsing position 
   */
  def standardRstBlock: Parser[Block]

  /** Parses reStructuredText blocks, except normal paragraphs
   *  and blocks that allow nesting of blocks. Only used in rare cases when the maximum
   *  nest level allowed had been reached
   */
  def nonRecursiveRstBlock: Parser[Block]
  
  def paragraph: Parser[Paragraph]
  
  
  override def ws = anyOf(' ') // other whitespace has been replaced with spaces by preprocessor
  
  
  /** Parses a full block based on the specified helper parser. When the parser for
   *  the first line succeeds, this implementation assumes that for any subsequent
   *  lines or blocks the only requirement is that they are not empty and indented
   *  like the first line.
   * 
   *  @param firstLinePrefix parser that recognizes the start of the first line of this block
   *  @param pos the current parsing position 
   */
  def indentedBlock (firstLinePrefix: Parser[Int]): Parser[(List[String],Int)] = {
    indentedBlock(firstLinePrefix, not(blankLine), not(blankLine))
  }
  
  /** Parses a full block based on the specified helper parsers. It expects an indentation for
   *  all subsequent lines based on the length of the prefix of the first line plus any whitespace
   *  immediately following it.
   * 
   *  @param firstLinePrefix parser that recognizes the start of the first line of this block
   *  @param linePrefix parser that recognizes the start of subsequent lines that still belong to the same block
   *  @param nextBlockPrefix parser that recognizes whether a line after one or more blank lines still belongs to the same block
   *  @param pos the current parsing position 
   */
  def indentedBlock (firstLinePrefix: Parser[Int], 
                     linePrefix: => Parser[Any], 
                     nextBlockPrefix: => Parser[Any]): Parser[(List[String],Int)] = {
    firstLinePrefix >> { width => (ws min 1) >> { firstIndent =>
      val indentParser = ws take (width + firstIndent.length)
      block(success( () ), indentParser ~> linePrefix, indentParser ~> nextBlockPrefix) ^^ { lines => 
        (lines, firstIndent.length)   
      } 
    }}  
  }
  
  def lineAndIndentedBlock (firstLinePrefix: Parser[Int], 
                            linePrefix: => Parser[Any], 
                            nextBlockPrefix: => Parser[Any]): Parser[(List[String],Int)] = {
    restOfLine ~ (ws min 1) >> { case firstLine ~ leftIndent =>
      val indentParser = ws min leftIndent.length
      block(success( () ), indentParser ~> linePrefix, indentParser ~> nextBlockPrefix) ^^ { lines => 
        (lines, leftIndent.length)   
      } 
    }
  }
  
  
}