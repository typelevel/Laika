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

package laika.parse.rst

import laika.parse.core._
import laika.parse.core.markup.BlockParsers._
import laika.parse.core.markup.{RecursiveParsers, InlineParsers => CoreInlineParsers}
import laika.parse.core.text.Characters
import laika.parse.core.text.TextParsers._
import laika.parse.rst.BaseParsers._
import laika.parse.rst.Elements._
import laika.tree.Elements._
import laika.util.~

/** Provides the parsers for all types of block-level elements of reStructuredText. 
 *  It merges the individual traits that provide implementations for list, tables, etc. and 
 *  adds the remaining block level parsers that do not fit into any of the subcategories 
 *  supported by the other traits.
 * 
 *  Block parsers are only concerned with splitting the document into 
 *  (potentially nested) blocks. They are used in the first phase of parsing,
 *  while delegating to inline parsers for the 2nd phase.
 * 
 * @author Jens Halm
 */
class BlockParsers (recParsers: RecursiveParsers) {


  import recParsers._

  
  val ws: Characters[String] = anyOf(' ') // other whitespace has been replaced with spaces by preprocessor
                        

  /** Parses a transition (rule).
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#transitions]].
   */  
  val transition: Parser[Rule] = (punctuationChar min 4) ~ wsEol ~ lookAhead(blankLine) ^^^ Rule()
    
  /** Parses a single paragraph. Everything between two blank lines that is not
   *  recognized as a special reStructuredText block type will be parsed as a regular paragraph.
   */
  lazy val paragraph: Parser[Paragraph] =
    recursiveSpans(((not(blankLine) ~> restOfLine) +) ^^ (_.mkString("\n"))) ^^ { Paragraph(_) }


  /** Parses a section header with both overline and underline.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#sections]].
   */
  lazy val headerWithOverline: Parser[Block] = (punctuationChar take 1) >> { start =>
    val char = start.charAt(0)
    anyOf(char) >> { deco =>
      val len = deco.length + 1
      val text = recursiveSpans((anyBut('\n') max len) ^^ (_.trim))
      val decoLine = anyOf(char) take len

      (wsEol ~> text <~ wsEol ~ decoLine ~ wsEol) ^^ {
        title => DecoratedHeader(OverlineAndUnderline(char), title)
      }
    }
  }
  
  /** Parses a section header with an underline, but no overline.
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#sections]].
   */
  lazy val headerWithUnderline: Parser[Block] = {
    (anyBut(' ') take 1) ~ restOfLine >> { case char ~ rest =>
      val title = (char + rest).trim
      (punctuationChar take 1) >> { start =>
        val char = start.charAt(0)
        withRecursiveSpanParser((anyOf(char) min (title.length - 1)) ~ wsEol) ^^ {
          case (recParser, _) => DecoratedHeader(Underline(char), recParser(title))
        }
      }
    }
  }
  
  /** Parses a doctest block. This is a feature which is very specific to the
   *  world of Python where reStructuredText originates. Therefore the resulting
   *  `DoctestBlock` tree element is not part of the standard Laika tree model.
   *  When this block type is used the corresponding special renderers must 
   *  be enabled (e.g. the `ExtendedHTML` renderer for HTML).
   *  
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#doctest-blocks]]
   */
  val doctest: Parser[Block] = ">>> " ~> restOfLine ~
      ((not(blankLine) ~> restOfLine) *) ^^ { case first ~ rest => DoctestBlock((first :: rest) mkString "\n") }
  
  
  /** Parses a block quote with an optional attribution. 
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#block-quotes]]
   */
  lazy val blockQuote: Parser[Block] = {
    
    val attributionStart = "---" | "--" | '\u2014' // em dash
        
    def attribution (indent: Int) = (ws take indent) ~ attributionStart ~ (ws max 1) ~> 
      recursiveSpans(indentedBlock(minIndent = indent, endsOnBlankLine = true))
      
    lookAhead(ws take 1) ~> withRecursiveBlockParser(indentedBlockWithLevel(
        firstLineIndented = true, linePredicate = not(attributionStart))) >> {
      case (recParser, (block, minIndent)) => opt(opt(blankLines) ~> attribution(minIndent)) ^^ {
        spans => QuotedBlock(recParser(block), spans.getOrElse(Nil))
      }
    }
  }
  
  /** Parses a literal block, either quoted or indented.
   *  Only used when the preceding block ends with a double colon (`::`).
   * 
   *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#literal-blocks]]
   */
  val literalBlock: Parser[Block] = {
    val indented = indentedBlock(firstLineIndented = true) ^^ { LiteralBlock(_) }

    val quotedLine = lookAhead(punctuationChar min 1)
    val quoted = block(quotedLine, quotedLine, failure("blank line always ends quoted block")) ^^ { LiteralBlock(_) }

    indented | quoted
  }
  

}
