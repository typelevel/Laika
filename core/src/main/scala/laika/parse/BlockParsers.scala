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

package laika.parse
  
import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}
import laika.rewrite.TreeUtil
import laika.tree.Documents.Document
import laika.tree.Elements.{Block, ConfigValue, InvalidBlock, RootElement}
import laika.tree.Paths.Path
import laika.parse.core._
  
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
trait BlockParsers extends MarkupParsers {

  
  /** The maximum level of block nesting. Some block types like lists
   *  and blockquotes contain nested blocks. To protect against malicious
   *  input or accidentally broken markup, the level of nesting is restricted.
   */
  val maxNestLevel: Int = 12
  
   
  /** Parses any kind of top-level block supported by a concrete markup language.
   */
  final lazy val topLevelBlock: Parser[Block] = asChoice(prepareBlockParsers(false))
 
  /** Parses any kind of nested block supported by a concrete markup language.
   */
  final lazy val nestedBlock: Parser[Block] = asChoice(prepareBlockParsers(true))
 
  /** Parses reStructuredText blocks, except for blocks that allow nesting of blocks. 
   *  Only used in rare cases when the maximum nest level allowed had been reached
   */
  def nonRecursiveBlock: Parser[Block]

  
  /** Parses a full document, delegating most of the work to the `topLevelBlock` parser.
   */
  def rootElement: Parser[RootElement] = opt(blankLines) ~> blockList(topLevelBlock) ^^ RootElement
  
  /** Fully parses the input from the specified reader and returns the document tree. 
   *  This function is expected to always succeed, errors would be considered a bug
   *  of this library, as the parsers treat all unknown or malformed markup as regular
   *  text.
   */
  def parseDocument (reader: ParserContext, path: Path): Document = {
    val (config, root) = parseConfigAndRoot(reader, path)
    Document(path, root, TreeUtil.extractFragments(root.content), config)
  }

  def config (path: Path): Parser[Either[InvalidBlock,Config]] = failure("configuration sections not enabled")
  
  /** Fully parses the input from the specified reader and returns the configuration and root element. 
   */
  protected def parseConfigAndRoot (reader: ParserContext, path: Path): (Config,RootElement) = {
    def assembleConfig (config: Config, root: RootElement) = {
      import scala.collection.JavaConverters._
      val values = root.content collect { case ConfigValue(name, value, _) => (name, value match {
        case m: Map[_,_]      => m.asJava
        case it: Iterable[_]  => it.asJava
        case other            => other
      })}
      ((config /: values) { case (config, (name, value)) =>
        config.withValue(name, ConfigValueFactory.fromAnyRef(value))
      }, root)
    }
    val parser = opt(config(path)) ~ rootElement ^^ {
      case Some(Right(config)) ~ root => assembleConfig(config, root)
      case Some(Left(block)) ~ root   => assembleConfig(ConfigFactory.empty(), root.copy(content = block +: root.content))
      case None ~ root                => assembleConfig(ConfigFactory.empty(), root)
    }
    parseMarkup(parser, reader)
  }
  
  
  /** Extension hook for assembling the block parsers for a particular markup format.
   *  
   *  @param nested true if these are parsers for nested blocks, false if they are for top level blocks
   *  @return a list of block parsers which later will be interpreted as choices in the specified order
   */
  protected def prepareBlockParsers (nested: Boolean): List[Parser[Block]]
  
  private def asChoice (parsers: List[Parser[Block]]): Parser[Block] = 
    if (parsers.isEmpty) failure("No block parsers specified")
    else parsers.reduceLeft(_ | _)
  
  /** Parses all nested blocks for the specified input and nest level.
   *  Delegates to the abstract `nestedBlock` parser that sub-traits need to define.
   *  The nest level is primarily used as a protection against malicious
   *  input that forces endless recursion.
   * 
   *  @param lines the input to parse
   *  @param nestLevel the level of nesting with 0 being the outermost level
   *  @return the parser result as a list of blocks
   */
  def parseNestedBlocks (lines: List[String], nestLevel: Int): List[Block] =
    parseNestedBlocks(lines mkString "\n", nestLevel)
    
  /** Parses all nested blocks for the specified input and nest level.
   *  Delegates to the abstract `nestedBlock` parser that sub-traits need to define.
   *  The nest level is primarily used as a protection against malicious
   *  input that forces endless recursion.
   * 
   *  @param input the input to parse
   *  @param nestLevel the level of nesting with 0 being the outermost level
   *  @return the parser result as a list of blocks
   */
  def parseNestedBlocks (input: String, nestLevel: Int): List[Block] = {
    val parser = if (nestLevel < maxNestLevel) nestedBlock else nonRecursiveBlock 
    val reader = ParserContext(input, nestLevel + 1)
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
  
  /** Creates a new parser that produces a tuple containing the current nest
   *  level as well as the result from the specified parser.
   * 
   *  The nest level is usually only used to prevent endless recursion of nested blocks. 
   */
  def withNestLevel [T] (p: => Parser[T]): Parser[(Int, T)] = Parser { in =>
    p(in) match {
      case Success(res, next) => Success((next.nestLevel, res), next)
      case f: Failure         => f
    }
  }
  
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
  val blankLines: Parser[List[String]] = (not(eof) ~> blankLine)+

  /** Parses a single text line from the current input offset (which may not be at the
    *  start of the line. Fails for blank lines. Does not include the eol character(s).
    */
  val textLine: Parser[String] = not(blankLine) ~> restOfLine

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
    
    def emptyLines (curIndent: Int) = blankLines <~ guard(lineStart(curIndent)) ^^ {
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

    guard(firstLineGuard) ~> withNestLevel(rep(firstLine, nextLine)) ^^ { case (nestLevel,parsed) =>
      val (minIndent, lines) = result(parsed)
      IndentedBlock(nestLevel, minIndent, lines)
    }
  }
  
  
}
