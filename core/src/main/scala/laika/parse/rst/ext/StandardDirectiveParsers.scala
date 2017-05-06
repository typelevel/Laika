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
  
package laika.parse.rst.ext

import laika.parse.core.{Failure, Parser, Success}
import laika.parse.core.combinator.Parsers._
import laika.parse.core.text.DelimitedBy
import laika.parse.rst.Directives.DirectivePart
import laika.parse.rst.{BlockParsers, InlineParsers}
import laika.parse.rst.TextRoles.RoleDirectivePart
import laika.tree.Elements._
import laika.util.~

/** Defines the custom argument and body parsers for the standard directives.
 *  Most of these delegate to the default block or inline parsers for `reStructuredText`,
 *  but often do only except one specific block type like `Table` or `QuotedBlock` whereas
 *  the default block parser usually accepts any of the blocks.  
 * 
 *  @author Jens Halm
 */
trait StandardDirectiveParsers extends BlockParsers with InlineParsers {

  
  def blockDirective (name: String): Option[DirectivePart[Block]] = None
  def spanDirective (name: String): Option[DirectivePart[Span]] = None
  def textRole (name: String): Option[RoleDirectivePart[String => Span]] = None


  /** Utility method to be used by custom parsers for directive argument or body.
    *  It translates a `Success` into a `Right` and a `NoSuccess` into a `Left`.
    */
  def parseDirectivePart [T] (parser: Parser[T], source: String): Either[String,T] = {
    consumeAll(parser).parse(source.trim) match {
      case Success(result,_) => Right(result)
      case Failure(msg, in) => Left(msg.message(in))
    }
  }


  /** Parses all standard inline markup supported by `reStructuredText`.
   *  
   *  @param p the standard inline parsers including all registered directives for recursive use
   *  @param input the input to parse
   *  @return `Right` in case of parser success and `Left` in case of failure, to adjust to the Directive API
   */
  def standardSpans (p: InlineParsers)(input: String): Either[String,Seq[Span]] =
    parseDirectivePart(p.recursiveSpans, input.trim)

  /** Parses a quoted block with nested blocks.
   *  
   *  @param p the standard block parsers including all registered directives for recursive use
   *  @param input the input to parse
   *  @return `Right` in case of parser success and `Left` in case of failure, to adjust to the Directive API
   */
  def quotedBlock (p: BlockParsers, style: String)(input: String): Either[String,Block] = {
    parseDirectivePart(p.blockList(p.nestedBlock), input).right.map { blocks =>
      blocks.lastOption match {
        case Some(p @ Paragraph(Text(text, opt) :: _, _)) if text startsWith "-- " => 
          val attr = Text(text.drop(3), opt + Styles("attribution")) +: p.content.tail
          QuotedBlock(blocks.init, attr, Styles(style))
        case _ => 
          QuotedBlock(blocks, Nil, Styles(style))
      }
    }
  }

  /** Parses one of the two table types supported by `reStructuredText`.
   *  
   *  @param p the standard block parsers including all registered directives for recursive use
   *  @param input the input to parse
   *  @return `Right` in case of parser success and `Left` in case of failure, to adjust to the Directive API
   */
  def table (p: BlockParsers)(input: String): Either[String, Table] = 
    parseDirectivePart(p.gridTable | p.simpleTable, input)
  
  /** Parses a caption (a single paragraph) and a legend (one or more blocks), both being optional.
   *  
   *  @param p the standard block parsers including all registered directives for recursive use
   *  @param input the input to parse
   *  @return `Right` in case of parser success and `Left` in case of failure, to adjust to the Directive API
   */
  def captionAndLegend (p: BlockParsers)(input: String): Either[String,(Seq[Span],Seq[Block])] = {
    val parser = opt(p.paragraph) ~ opt(p.blankLines ~> p.blockList(p.nestedBlock)) ^^ {
      case ~(Some(caption), Some(legend)) => (caption.content, legend)
      case ~(Some(caption), None)         => (caption.content, Nil)
      case _                              => (Nil, Nil)
    } 
    parseDirectivePart(parser, input)
  }
  
  /** Parses a target which might be a simple reference, a phrase reference or an uri.
   *  
   *  @param input the input to parse
   *  @return `Right` in case of parser success and `Left` in case of failure, to adjust to the Directive API
   */
  def target (input: String): Either[String,Span] = {
    val phraseLinkRef = {
      val refName = escapedText(DelimitedBy('`','<').keepDelimiter) ^^ ReferenceName
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
    val hexNum = anyIn('0' to '9', 'A' to 'F', 'a' to 'f') 
    val hex = ((("0x" | "x" | "\\x" | "U+" | "u" | "\\u") ~> hexNum) | ("&#x" ~> hexNum <~ ";")) ^^ { Integer.parseInt(_,16) }
    val dec = (anyIn('0' to '9') min 1) ^^ { Integer.parseInt }
    val unicode = (hex | dec) ^^ { int => new String(Character.toChars(int)) }
    val text = anyBut(' ') min 1
    val parser = (((unicode | text) <~ ws)*) ^^ { _.mkString(" ") }
    parseDirectivePart(parser, input)
  }
  
}
