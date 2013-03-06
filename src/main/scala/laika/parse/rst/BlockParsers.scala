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
import laika.parse.InlineParsers
import laika.parse.rst.Elements.DoctestBlock
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
                      with ExplicitBlockParsers { self: InlineParsers => // TODO - probably needs to be rst.InlineParsers

  
  /** Parses a single paragraph. Everything between two blank lines that is not
   *  recognized as a special reStructuredText block type will be parsed as a regular paragraph.
   */
  def paragraph: Parser[Paragraph] = 
      ((not(blankLine) ~> restOfLine) +) ^^ { lines => Paragraph(parseInline(lines mkString "\n")) }
  
  def doctest: Parser[Block] = ">>> " ~> restOfLine ~ 
      ((not(blankLine) ~> restOfLine) *) ^^ { case first ~ rest => DoctestBlock((first :: rest) mkString "\n") }
  
  
  def blockQuote: Parser[Block] = {
    
    def attributionStart (minIndent: Int) = (ws take (minIndent)) ~ "---" | "--" // TODO - em dash
        
    def attribution (indent: Int) = attributionStart(indent) ~> 
      fixedIndentedBlock(indent, success(()), failure("block ends with blank line")) ^^ { block => 
      parseInline(block.lines mkString "\n")
    }
    
    guard(ws take 1) ~> (Parser { in =>
    
      val curNestLevel = nestLevel(in)
      
      val indent = ((ws min 1) ^^ {_.length})
      
      def line (minIndent: Int) = ((not(attributionStart(minIndent)) ~> indent ~ restOfLine) ^^ 
        { case indent ~ text => (indent, text.trim) }) | blankLine ^^^ (Int.MaxValue, "")
  
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
  

  
  def nonRecursiveBlock: Parser[Block] = comment | paragraph
  
  def topLevelBlock: Parser[Block] = unorderedList | 
                                     orderedList | 
                                     definitionList | 
                                     fieldList | 
                                     optionList |
                                     explicitBlockItem |
                                     gridTable |
                                     simpleTable |
                                     doctest |
                                     paragraph
 
  def nestedBlock: Parser[Block] = topLevelBlock
  
  
}