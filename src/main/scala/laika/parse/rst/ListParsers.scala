package laika.parse.rst

import laika.tree.Elements._
import laika.parse.InlineParsers
import scala.annotation.tailrec

trait ListParsers extends laika.parse.BlockParsers { self: InlineParsers => // TODO - probably needs to be rst.InlineParsers {

  
  // TODO - several members need to be extracted to a base trait once more features get added
  
  
  val tabStops = 8
  
  
  /** The maximum level of block nesting. Some block types like lists
   *  and blockquotes contain nested blocks. To protect against malicious
   *  input or accidentally broken markup, the level of nesting is restricted.
   */
  val maxNestLevel: Int = 12
  
  
  case class BlockPosition (nestLevel: Int, column: Int) {
    
    //def nextNestLevel = new BlockPosition(nestLevel + 1, column)
    
    def indent (value: Int) = new BlockPosition(nestLevel + 1, column + value)
    
  }
  
  
  /** Parses blocks that may appear inside a list item.
   * 
   *  @param pos the current parsing position 
   */
  def listItemBlocks (pos: BlockPosition) = 
    if (pos.nestLevel < maxNestLevel) (standardRstBlock(pos) | paragraph) *
    else (nonRecursiveRstBlock | paragraph) *
   
  
  /** Parses all of the standard reStructuredText blocks, except normal paragraphs. 
   * 
   *  @param pos the current parsing position 
   */
  def standardRstBlock (pos: BlockPosition): Parser[Block] = unorderedList(pos)

  /** Parses reStructuredText blocks, except normal paragraphs
   *  and blocks that allow nesting of blocks. Only used in rare cases when the maximum
   *  nest level allowed had been reached
   */
  def nonRecursiveRstBlock: Parser[Block]
  
  def paragraph: Parser[Paragraph]
  
  /*
   * unordered list:
   * 
   * - items start with '*', '+', '-' in column 1 (TODO - 3 more non-ASCII characters)
   * - text left-aligned and indented with one or more spaces
   * - sublist indented like text in top-list
   * - blank lines required before and after lists (including sub-list)
   * - blank lines between list items optional
   * - unindenting without blank line ends list with a warning (TODO)
   */
  
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
          case ' ' | '\f' | '\u000b' => parse(offset + 1, indent + 1)
          case '\t' => val newIndent = (indent / tabStops + 1) * tabStops
                       if (newIndent > finalIndent) result(offset, indent) else parse(offset + 1, newIndent)
          case _    => if (indent < finalIndent && expect > 0) Failure("Less than expected indentation", in) else result(offset, indent)
        }
        
      }
    }
  
    parse(in.offset, current)
  }
    
  
  /** Parses a full block based on the specified helper parsers.
   * 
   *  @param firstLinePrefix parser that recognizes the start of the first line of this block
   *  @param linePrefix parser that recognizes the start of subsequent lines that still belong to the same block
   *  @param nextBlockPrefix parser that recognizes whether a line after one or more blank lines still belongs to the same block
   *  @param pos the current parsing position 
   */
  def indentedBlock (firstLinePrefix: Parser[Int], linePrefix: => Parser[Any], nextBlockPrefix: => Parser[Any], pos: BlockPosition): Parser[(List[String],BlockPosition)] = {
    firstLinePrefix >> { width => indent(pos.column + width) >> { firstIndent =>
      val indentParser = indent(pos.column, firstIndent)
      block(success( () ), indentParser ~> linePrefix, indentParser ~> nextBlockPrefix) ^^ { lines => 
        (lines, pos.indent(firstIndent))   
      } 
    }}  
  }
  
  
  /** Parses a list based on the specified helper parsers.
   * 
   *  @param itemStart parser that recognizes the start of a list item, result will be discarded
   *  @param newList function that produces a block element for the document tree
   *  @param pos the current parsing position 
   */
  def list [T <: Block] (itemStart: Parser[String], newList: List[ListItem] => T, pos: BlockPosition) = {
    guard(itemStart) >> { symbol =>
      ((listItem(symbol, pos)) *) ^^ { x => newList(x) }
    } 
  }
  
  /** Parses a single list item.
   * 
   *  @param itemStart parser that recognizes the start of a list item, result will be discarded
   *  @param pos the current parsing position 
   */
  def listItem (itemStart: Parser[String], pos: BlockPosition): Parser[ListItem] = {
    indentedBlock(itemStart ^^ { res => res.length }, not(blankLine), not(blankLine), pos) ^^
          { case (lines,pos) => ListItem(parseMarkup(listItemBlocks(pos), lines mkString "\n")) }
  }
 
  
  /** Parses the start of an unordered list item.
   */
  val unorderedListItemStart = anyOf('*','-','+').take(1)
  
  /** Parses an unordered list.
   * 
   *  @param pos the current parsing position 
   */
  def unorderedList (pos: BlockPosition): Parser[UnorderedList] = list(unorderedListItemStart, UnorderedList, pos)
    
  
}