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
import laika.parse.rst.Elements.DoctestBlock
import laika.parse.rst.Elements.SectionHeader
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

/**
 * Provides all the block level parsers for reStructuredText that do not fit
 * into any of the subcategories supported by other traits, like lists or tables,
 * as well as the general high level block parsers that combine the others.
 * 
 * @author Jens Halm
 */
trait BlockParsers extends BlockBaseParsers 
                      with ListParsers 
                      with TableParsers 
                      with ExplicitBlockParsers { self: InlineParsers =>

                        
  override def document = super.document ^^ { doc =>
    val levelMap = scala.collection.mutable.Map.empty[(Char,Boolean),Int]
    val levelIt = Stream.from(1).iterator
    doc rewrite {
      case SectionHeader(char, overline, content) => 
        Some(Header(levelMap.getOrElseUpdate((char,overline), levelIt.next), content))
    }
  }                      
                        
  val punctuationChar = 
    anyOf('!','"','#','$','%','&','\'','(',')','[',']','{','}','*','+',',','-','.',':',';','/','<','>','=','?','@','\\','^','_','`','|','~')
  
  val transition = (punctuationChar min 4) ~ ws ~ eol ~ guard(blankLine) ^^^ Rule  
    
  /** Parses a single paragraph. Everything between two blank lines that is not
   *  recognized as a special reStructuredText block type will be parsed as a regular paragraph.
   */
  def paragraph: Parser[Paragraph] = 
      ((not(blankLine) ~> restOfLine) +) ^^ { lines => Paragraph(parseInline(lines mkString "\n")) }
  
  
  def headerWithOverline: Parser[Block] = {
    (punctuationChar take 1) >> { start =>
      val char = start.charAt(0)
      anyOf(char) >> { deco =>
        val len = deco.length + 1
        (ws ~ eol ~> (anyBut('\n') max len) <~ 
         ws ~ eol ~ (anyOf(char) take len) ~
         ws ~ eol) ^^ { title => SectionHeader(char, true, parseInline(title.trim)) }
      }
    }
  }
  
  def headerWithUnderline: Parser[Block] = {
    (anyBut(' ') take 1) ~ restOfLine >> { case char ~ rest =>
      val title = (char + rest).trim
      (punctuationChar take 1) >> { start =>
        val char = start.charAt(0)
        ((anyOf(char) min (title.length - 1)) ~
         ws ~ eol) ^^ { _ => SectionHeader(char, false, parseInline(title)) }
      }
    }
  }
  
  
  def doctest: Parser[Block] = ">>> " ~> restOfLine ~ 
      ((not(blankLine) ~> restOfLine) *) ^^ { case first ~ rest => DoctestBlock((first :: rest) mkString "\n") }
  
  
  def blockQuote: Parser[Block] = {
    
    /* The implementation of this parser is complex as we cannot use the varIndentedBlock parser
     * used in all other parsers for indented blocks because the attribution needs to be detected
     * as flushed left which depends on the current minimum indentation. Parts of the code below
     * are basically a dynamic re-implementation of the varIndentedBlock parser.
     */
    def attributionStart (minIndent: Int) = (ws take minIndent) ~ ("---" | "--" | '\u2014') // em dash
        
    def attribution (indent: Int) = attributionStart(indent) ~> 
      fixedIndentedBlock(indent, success(()), failure("block ends with blank line")) ^^ { block => 
      parseInline(block.lines mkString "\n")
    }
    
    guard(ws take 1) ~> (Parser { in =>
    
      val curNestLevel = nestLevel(in)
      
      val indent = ((ws min 1) ^^ {_.length})
      
      def line (minIndent: Int) = ((not(attributionStart(minIndent)) ~> indent ~ restOfLine) ^^ 
        { case indent ~ text => (indent, text.trim) }) | (not(eof) ~ blankLine ^^^ (Int.MaxValue, ""))
  
      def nextParser (oldParser: Parser[(Int,String)], oldIndent: Int, newIndent: Int) = 
        if (newIndent < oldIndent) (newIndent, line(newIndent))
        else (oldIndent, oldParser)
        
      def result (indent: Int, lines: List[(Int,String)]): List[String] = 
        lines map (line => if (line._1 == Int.MaxValue) line._2 else " " * (line._1 - indent) + line._2)
        
      val elems = new ListBuffer[(Int,String)]
  
      @tailrec def parse (in0: Input, p: Parser[(Int,String)], minIndent: Int): ParseResult[IndentedBlock] =
        p(in0) match {
          case Success((indent, text), rest) => 
            elems += ((indent, text))
            val (newMin, newParser) = nextParser(p, minIndent, indent)
            parse(rest, newParser, newMin)
          case ns: NoSuccess => Success(IndentedBlock(curNestLevel, minIndent, result(minIndent, elems toList)), in0)
        }
  
      parse(in, line(Int.MaxValue), Int.MaxValue)
      
    } >> { block =>
      opt(attribution(block.minIndent)) ^^ { spans => QuotedBlock(parseNestedBlocks(block), spans.getOrElse(Nil)) }
    }) 
  }
  
  
  /** Builds a parser for a list of blocks based on the parser for a single block. 
   * 
   *  Overridden to check each Paragraph for a double colon ending which has influence
   *  on the parsing of the following block (turning it into a literal block).  
   */
  override def blockList (parser: => Parser[Block]): Parser[List[Block]] = Parser { in =>
    val defaultBlock = parser <~ opt(blankLines)
    val litBlock = literalBlock | defaultBlock 
    val elems = new ListBuffer[Block]

    def processLiteralMarker (par: Paragraph) = {
      par.content.lastOption match {
        case Some(Text(text)) if text.trim.endsWith("::") => 
          val drop = if (text.length > 2 && text.charAt(text.length-3) == ' ') 3 else 1
          val spans = par.content.init.toList ::: List(Text(text.dropRight(drop)))
          (Paragraph(spans), litBlock)
        case _ => (par, defaultBlock) 
      }
    }
    
    @tailrec 
    def parse (p: Parser[Block], in: Input): ParseResult[List[Block]] = p(in) match {
      case Success(Paragraph(Text(txt) :: Nil), rest) if txt.trim == "::" => parse(litBlock, rest)
      case Success(p: Paragraph, rest) => 
        val (paragraph, parser) = processLiteralMarker(p)
        elems += paragraph
        parse(parser, rest)
      case Success(x, rest) => elems += x; parse(defaultBlock, rest)
      case _                => Success(elems.toList, in)
    }

    parse(defaultBlock, in)
  }
  
  def literalBlock: Parser[Block] = {
    val indented = varIndentedBlock(testFirstLine = true) ^^ 
      { block => CodeBlock(block.lines mkString "\n") }
    val quoted = block(guard(punctuationChar min 1), guard(punctuationChar min 1), failure("blank line always ends quoted block")) ^^ 
      { lines => CodeBlock(lines mkString "\n") }  
    indented | quoted
  }
  
  def nonRecursiveBlock: Parser[Block] = comment | paragraph
  
  def topLevelBlock: Parser[Block] = unorderedList | 
                                     orderedList | 
                                     fieldList | 
                                     lineBlock |
                                     optionList | 
                                     explicitBlockItem |
                                     gridTable |
                                     simpleTable |
                                     doctest |
                                     blockQuote |
                                     headerWithOverline |
                                     transition |
                                     headerWithUnderline |
                                     definitionList |
                                     paragraph
 
  def nestedBlock: Parser[Block] = topLevelBlock
  
  
}