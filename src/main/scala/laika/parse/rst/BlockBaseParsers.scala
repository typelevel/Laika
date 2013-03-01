package laika.parse.rst

import laika.tree.Elements._
import scala.annotation.tailrec

trait BlockBaseParsers extends laika.parse.BlockParsers {

  
  /** The maximum level of block nesting. Some block types like lists
   *  and blockquotes contain nested blocks. To protect against malicious
   *  input or accidentally broken markup, the level of nesting is restricted.
   */
  val maxNestLevel: Int = 12
  
  
  case class BlockPosition (nestLevel: Int, column: Int) {
    
    //def nextNestLevel = new BlockPosition(nestLevel + 1, column)
    
    def indent (value: Int) = new BlockPosition(nestLevel + 1, column + value)
    
  }
  
  
  /** Parses all of the standard reStructuredText blocks, except normal paragraphs. 
   * 
   *  @param pos the current parsing position 
   */
  def standardRstBlock (pos: BlockPosition): Parser[Block]

  /** Parses reStructuredText blocks, except normal paragraphs
   *  and blocks that allow nesting of blocks. Only used in rare cases when the maximum
   *  nest level allowed had been reached
   */
  def nonRecursiveRstBlock: Parser[Block]
  
  def paragraph: Parser[Paragraph]
  
  
  override def ws = anyOf(' ','\t', '\f', '\u000b') // 0x0b: vertical tab
  
  
  def indent (current: Int, expect: Int = 0) = Parser { in =>
    
    val source = in.source
    val eof = source.length

    val finalIndent = if (expect > 0) current + expect else Int.MaxValue
    
    def result (offset: Int, indent: Int) = Success(indent - current, in.drop(offset - in.offset))
    
    @tailrec
    def parse (offset: Int, indent: Int): ParseResult[Int] = {
      if (offset == eof) Failure("unecpected end of input", in)
      else if (indent == finalIndent) result(offset, indent)
      else {
        source.charAt(offset) match {
          case ' ' => parse(offset + 1, indent + 1)
          case _   => if (indent < finalIndent && expect > 0) Failure("Less than expected indentation", in) else result(offset, indent)
        }
        
      }
    }
  
    parse(in.offset, current)
  }
  
  
  def minIndent (current: Int, min: Int) = indent(current) ^? { case i if i >= min => i }
    
  
  /** Parses a full block based on the specified helper parser. When the parser for
   *  the first line succeeds, this implementation assumes that for any subsequent
   *  lines or blocks the only requirement is that they are not empty and indented
   *  like the first line.
   * 
   *  @param firstLinePrefix parser that recognizes the start of the first line of this block
   *  @param pos the current parsing position 
   */
  def indentedBlock (firstLinePrefix: Parser[Int], pos: BlockPosition): Parser[(List[String],BlockPosition)] = {
    indentedBlock(firstLinePrefix, not(blankLine), not(blankLine), pos)
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
                     nextBlockPrefix: => Parser[Any], 
                     pos: BlockPosition): Parser[(List[String],BlockPosition)] = {
    firstLinePrefix >> { width => minIndent(pos.column + width, 1) >> { firstIndent =>
      val indentParser = indent(pos.column, width + firstIndent)
      block(success( () ), indentParser ~> linePrefix, indentParser ~> nextBlockPrefix) ^^ { lines => 
        (lines, pos.indent(firstIndent))   
      } 
    }}  
  }
  
  def lineAndIndentedBlock (firstLinePrefix: Parser[Int], 
                            linePrefix: => Parser[Any], 
                            nextBlockPrefix: => Parser[Any], 
                            pos: BlockPosition): Parser[(List[String],BlockPosition)] = {
    restOfLine ~ minIndent(pos.column, 1) >> { case firstLine ~ leftIndent =>
      val indentParser = indent(pos.column, leftIndent)
      block(success( () ), indentParser ~> linePrefix, indentParser ~> nextBlockPrefix) ^^ { lines => 
        (lines, pos.indent(leftIndent))   
      } 
    }
  }
  
  
}