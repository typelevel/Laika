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
import laika.parse.{ BlockSource, Parser, SourceFragment }
import laika.parse.markup.{ RecursiveParsers, RecursiveSpanParser }
import laika.parse.text.CharGroup
import laika.parse.builders._
import laika.parse.implicits._
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
  def parseDirectivePart[T](parser: Parser[T], source: SourceFragment): Either[String, T] =
    consumeAll(parser).parse(source).toEither

  /** Utility method to be used by custom parsers for directive argument or body.
    *  It translates a `Success` into a `Right` and a `NoSuccess` into a `Left`.
    */
  def parseDirectivePart(
      parser: RecursiveSpanParser,
      source: SourceFragment
  ): Either[String, Seq[Span]] =
    parser.parse(source).toEither

  /** Parses all standard inline markup supported by `reStructuredText`.
    *
    *  @param p the standard inline parsers including all registered directives for recursive use
    *  @param input the input to parse
    *  @return `Right` in case of parser success and `Left` in case of failure, to adjust to the Directive API
    */
  def standardSpans(p: RecursiveParsers)(input: SourceFragment): Either[String, Seq[Span]] =
    parseDirectivePart(p.recursiveSpans, input)

  /** Parses one of the two table types supported by `reStructuredText`.
    *
    *  @param p the standard block parsers including all registered directives for recursive use
    *  @param input the input to parse
    *  @return `Right` in case of parser success and `Left` in case of failure, to adjust to the Directive API
    */
  def table(p: RecursiveParsers)(input: SourceFragment): Either[String, Block] = {
    val gridTable   = TableParsers.gridTable.createParser(p).parser
    val simpleTable = TableParsers.simpleTable.createParser(p).parser
    parseDirectivePart(gridTable | simpleTable, input)
  }

  /** Parses a caption (a single paragraph) and a legend (one or more blocks), both being optional.
    *
    *  @param p the standard block parsers including all registered directives for recursive use
    *  @param input the input to parse
    *  @return `Right` in case of parser success and `Left` in case of failure, to adjust to the Directive API
    */
  def captionAndLegend(
      p: RecursiveParsers
  )(input: SourceFragment): Either[String, (Seq[Span], Seq[Block])] = {
    val captionParser = p.recursiveSpans(textLine.rep.mkLines.line)
    val legendParser  = p.recursiveBlocks(anyChars.trim.line.map(BlockSource(_)))
    val parser = (captionParser ~ (opt(blankLines) ~> legendParser)).map { case caption ~ legend =>
      (caption, legend)
    }
    parseDirectivePart(parser, input)
  }

  /** Parses a target which might be a simple reference, a phrase reference or an uri.
    *
    *  @param input the input to parse
    *  @return `Right` in case of parser success and `Left` in case of failure, to adjust to the Directive API
    */
  def target(p: RecursiveParsers)(input: SourceFragment): Either[String, Span] = {
    val phraseLinkRef = {
      val refName = p.escapedText(delimitedBy('`', '<').keepDelimiter).map(ReferenceName.apply)
      ("`" ~> refName <~ "`_" ~ ws ~ eof).withCursor.map { case (name, src) =>
        LinkIdReference(Nil, name.normalized, src)
      }
    }
    val simpleLinkRef = {
      (simpleRefName <~ "_" ~ ws ~ eof).withCursor.map { case (refName, src) =>
        LinkIdReference(Nil, refName, src)
      }
    }
    val uri           = anyChars.withCursor.map { case (res, source) =>
      ParsedTarget.forLink(Nil, res, source)
    }

    parseDirectivePart(phraseLinkRef | simpleLinkRef | uri, input)
  }

  /** Parses unicode values in various notations intertwined with normal text.
    *
    *  @param input the input to parse
    *  @return `Right` in case of parser success and `Left` in case of failure, to adjust to the Directive API
    */
  def unicode(input: SourceFragment): Either[String, String] = {
    val hexNum  = anyOf(CharGroup.hexDigit)
    val hex     = ((("0x" | "x" | "\\x" | "U+" | "u" | "\\u") ~> hexNum) |
      ("&#x" ~> hexNum <~ ";")).map(Integer.parseInt(_, 16))
    val dec     = someOf(CharGroup.digit).map(Integer.parseInt)
    val unicode = (hex | dec).map { int => new String(Character.toChars(int)) }
    val text    = someNot(' ')
    val parser  = ((unicode | text) <~ ws).rep.map(_.mkString(" "))
    parseDirectivePart(parser, input)
  }

}
