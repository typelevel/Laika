package laika.asciidoc

import laika.bundle.BlockParserBuilder
import laika.bundle.BlockParser
import laika.parse.implicits._
import laika.parse.builders._
import laika.ast._
import laika.parse.Parser
import laika.parse.BlockSource
import laika.parse.text.PrefixedParser

import laika.parse.markup.RecursiveParsers
import laika.bundle.BlockPosition
import laika.parse.LineSource
import laika.parse.text.WhitespacePreprocessor


object BlockParsers {


    val insignificantSpaces: Parser[Unit] = anyOf(' ').void


    
  //note: section starts with section title and consists of List[block].
  
  // == section 1  |
  // Paragraph    section 1
  //    ...        |
  // Paragraph     
  // == section 2  |
  // Paragraph     section 1
  //    ...        |
  // Paragraph     

  /** Parses a section title, a line that starts with 1 to 6 `'='` characters,
   *  with the number of '=' characters corresponding to the level of the header.
   *  AsciiDoc also allows to decorate the line with trailing `'='` characters which
   *  this parser will remove if the number of '=' characters matches level.
   */
  val sectionTitle: BlockParserBuilder = BlockParser.recursive { recParsers =>
    val sym = '='
    
    val level = someOf(sym).max(6).count

    val stripDecoration = (text:String,level:Int)=> text.trim.stripSuffix(" "+sym*level).trim
    
    val text = (level:Int) => recParsers.recursiveSpans(restOfLine.map{str => stripDecoration(str,level)}.line)
    for {
      l <- level
      title <- not(blankLine) ~ ws ~> text(l) 
    } yield Header(l,title)
  }

    /** Parses a plain paragraph
    * 
    * Only used for root level blocks where lists starting in the middle of a paragraph are not allowed.
    */
  lazy val rootParagraph: BlockParserBuilder = 
    BlockParser.recursive(paragraph(_, BlockPosition.RootOnly)).rootOnly


  private def paragraph (recParsers: RecursiveParsers, pos: BlockPosition) : Parser[Block] = {

    val interruptions = recParsers.paragraphInterruptions(pos)
    val line = not(blankLine) ~> restOfLine.line
    val lineAndCond = interruptions.map(res => (Nil, Some(res))) | line.repUntil(interruptions)

    
    def paragraphInternal (firstLine: LineSource, restLines: Seq[LineSource]): Paragraph =
      Paragraph(recParsers.recursiveSpans.parseAndRecover(BlockSource(firstLine, restLines:_*)))

    (textLine.line ~ lineAndCond).map {
      case firstLine ~ ((restLines, None))       => paragraphInternal(firstLine, restLines)
      case firstLine ~ ((restLines, Some(list))) => BlockSequence(paragraphInternal(firstLine, restLines), list)        
    }
  }

    /** Parses a literal block, text indented by 1 or more tab(s) or space(s).
     * if each line starts with the same number of space or tab, it
   */
  val literalBlocks: BlockParserBuilder = BlockParser.standalone {
    val wsPreProcessor = new WhitespacePreprocessor
    PrefixedParser(' ','\t') {
      block(not(blankLine),not(blankLine)).map{lines => LiteralBlock(wsPreProcessor(lines.input.trim))}
    }
  }

  /** Parses a quoted block, a paragraph starting with a `"> "`,
   *  with subsequent lines optionally starting with a `''>'` and any number of `' '`.
   */
  val quotedBlock: BlockParserBuilder = BlockParser.recursive { recParsers =>
    PrefixedParser('>') {
      val quoteBlockStart = ">" ~ ws.take(1).void
      recParsers
        .recursiveBlocks(block(quoteBlockStart, quoteBlockStart | not(blankLine), literal("> ")))
        .map(QuotedBlock(_, Nil))
    }
  }

  /** Parses a horizontal rule, a line only decorated with three or more `'*'`, `'-'` or `'` and 
   *  characters with optional spaces between them. The number of spaces between decoration characters must be match.
   * 
   *  For example, `-ws-ws-` is valid, but `-wsws-ws-` is not.
   * 
   * To avoid conflicts with AsciiDoc’s block delimiter syntax, 
   * only 3 repeating characters (- or *) are recognized as horizontal rule. 
   * 
   * 
   * see [[https://docs.asciidoctor.org/asciidoc/latest/syntax-quick-reference/#breaks]]
   * 
   */
  val rules: BlockParserBuilder = BlockParser.standalone {
    val decoChar = oneOf('*', '-', '\'')
    val pattern = decoChar ~ (anyOf(' ').count >> { cnt =>
      decoChar ~ anyOf(' ').take(cnt) ~ decoChar
    })
    //val pattern = ( decoChar ~ (anyOf(' ').void ~ decoChar).rep.min(2)).as(Rule())
    pattern.as(Rule()) <~ wsEol
  }
    /** Parses just a plain paragraph after the maximum nest level has been reached.
    * This is necessary as a separate parser as the default markdown paragraph parser
    * is combined with potentially nested lists which makes that parser recursive.
    */
  val fallbackParagraph: BlockParserBuilder = BlockParser.withSpans { spanParsers =>
    val block = textLine.rep.min(1).map(_.mkString).line
    spanParsers.recursiveSpans(block).map(Paragraph(_))
  }.nestedOnly.withLowPrecedence
}
