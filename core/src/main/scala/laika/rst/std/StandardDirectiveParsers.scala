/*
 * Copyright 2012-2020 the original author or authors.
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

package laika.rst.std

import laika.ast._
import laika.parse.Parser
import laika.parse.markup.RecursiveParsers
import laika.parse.text.CharGroup
import laika.parse.text.TextParsers._
import laika.rst.BaseParsers.simpleRefName
import laika.rst.TableParsers
import laika.rst.ast.ReferenceName

/** Defines the custom argument and body parsers for the standard directives.
 *  Most of these delegate to the default block or inline parsers for `reStructuredText`,
 *  but often do only except one specific block type like `Table` or `QuotedBlock` whereas
 *  the default block parser usually accepts any of the blocks.  
 * 
 *  @author Jens Halm
 */
object StandardDirectiveParsers {

  
  /** Utility method to be used by custom parsers for directive argument or body.
    *  It translates a `Success` into a `Right` and a `NoSuccess` into a `Left`.
    */
  def parseDirectivePart [T] (parser: Parser[T], source: String): Either[String,T] =
    consumeAll(parser).parse(source.trim).toEither


  /** Parses all standard inline markup supported by `reStructuredText`.
   *  
   *  @param p the standard inline parsers including all registered directives for recursive use
   *  @param input the input to parse
   *  @return `Right` in case of parser success and `Left` in case of failure, to adjust to the Directive API
   */
  def standardSpans (p: RecursiveParsers)(input: String): Either[String,Seq[Span]] =
    parseDirectivePart(p.recursiveSpans, input.trim)

  /** Parses one of the two table types supported by `reStructuredText`.
   *  
   *  @param p the standard block parsers including all registered directives for recursive use
   *  @param input the input to parse
   *  @return `Right` in case of parser success and `Left` in case of failure, to adjust to the Directive API
   */
  def table (p: RecursiveParsers)(input: String): Either[String, Block] = {
    val gridTable = TableParsers.gridTable.createParser(p).parser
    val simpleTable = TableParsers.simpleTable.createParser(p).parser
    parseDirectivePart(gridTable | simpleTable, input)
  }

  /** Parses a caption (a single paragraph) and a legend (one or more blocks), both being optional.
   *  
   *  @param p the standard block parsers including all registered directives for recursive use
   *  @param input the input to parse
   *  @return `Right` in case of parser success and `Left` in case of failure, to adjust to the Directive API
   */
  def captionAndLegend (p: RecursiveParsers)(input: String): Either[String,(Seq[Span],Seq[Block])] = {
    val caption = p.recursiveSpans((textLine *) ^^ (_.mkString("\n")))
    val parser = p.withRecursiveBlockParser(caption) >> {
      case (parserF, captionSpans) => opt(blankLines) ~> (any ^^ { text =>
        val legendBlocks = parserF(text.trim)
        (captionSpans, legendBlocks)
      })
    }
    parseDirectivePart(parser, input)
  }
  
  /** Parses a target which might be a simple reference, a phrase reference or an uri.
   *  
   *  @param input the input to parse
   *  @return `Right` in case of parser success and `Left` in case of failure, to adjust to the Directive API
   */
  def target (p: RecursiveParsers)(input: String): Either[String,Span] = {
    val phraseLinkRef = {
      val refName = p.escapedText(delimitedBy('`','<').keepDelimiter) ^^ ReferenceName
      "`" ~> refName <~ "`_" ~ ws ~ eof ^^ {
        refName => LinkReference(Nil, refName.normalized, s"`${refName.original}`_") 
      }
    }
    val simpleLinkRef = {
      simpleRefName <~ "_" ~ ws ~ eof ^^ {
        refName => LinkReference(Nil, refName, s"${refName}_")
      }
    }
    val uri = any ^^ {
      uri => ExternalLink(Nil, uri)
    }
    parseDirectivePart(phraseLinkRef | simpleLinkRef | uri, input)
  }
  
  /** Parses unicode values in various notations intertwined with normal text.
   *  
   *  @param input the input to parse
   *  @return `Right` in case of parser success and `Left` in case of failure, to adjust to the Directive API
   */
  def unicode (input: String): Either[String,String] = {
    val hexNum = anyOf(CharGroup.hexDigit)
    val hex = ((("0x" | "x" | "\\x" | "U+" | "u" | "\\u") ~> hexNum) | ("&#x" ~> hexNum <~ ";")) ^^ { Integer.parseInt(_,16) }
    val dec = someOf(CharGroup.digit) ^^ { Integer.parseInt }
    val unicode = (hex | dec) ^^ { int => new String(Character.toChars(int)) }
    val text = anyBut(' ') min 1
    val parser = (((unicode | text) <~ ws)*) ^^ { _.mkString(" ") }
    parseDirectivePart(parser, input)
  }
  
}
