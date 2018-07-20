/*
 * Copyright 2013-2017 the original author or authors.
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

import laika.api.ext._
import laika.parse.core.markup.RootParserBase
import laika.parse.core.text.TextParsers._
import laika.parse.core.{Parsed, Parser, ParserContext, Success}
import laika.parse.rst.Elements.ReferenceName
import laika.rewrite.TreeUtil
import laika.tree.Elements._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/** The main parser for reStructuredText, combining the individual parsers for block and inline elements,
  * and adding functionality like directives depending on configuration.
  *
  * @author Jens Halm
  */
class RootParser(val blockParserExtensions: Seq[BlockParserBuilder] = Nil,
                 val spanParserExtensions: Seq[SpanParserBuilder] = Nil) extends RootParserBase {


  /** Parses an escaped character. For most characters it produces the character
    *  itself as the result with the only exception being an escaped space character
    *  which is removed from the output in reStructuredText.
    *
    *  See [[http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#escaping-mechanism]].
    */
  override lazy val escapedChar: Parser[String] = (" " ^^^ "") | (any take 1)


  val mainBlockParsers = Seq(
    ListParsers.bulletList,
    ListParsers.enumList,
    ListParsers.fieldList,
    ListParsers.lineBlock,
    ListParsers.optionList,
    ExplicitBlockParsers.allBlocks,
    ExplicitBlockParsers.shortAnonymousLinkTarget,
    TableParsers.gridTable,
    TableParsers.simpleTable,
    BlockParsers.doctest,
    BlockParsers.blockQuote,
    BlockParsers.headerWithOverline,
    BlockParsers.transition,
    BlockParsers.headerWithUnderline,
    ListParsers.definitionList,
    BlockParsers.paragraph
  )

  val mainSpanParsers = Seq(
    InlineParsers.strong,
    InlineParsers.em,
    InlineParsers.inlineLiteral,
    InlineParsers.phraseLinkRef,
    InlineParsers.simpleLinkRef,
    InlineParsers.footnoteRef,
    InlineParsers.citationRef,
    InlineParsers.substitutionRef,
    InlineParsers.internalTarget,
    InlineParsers.interpretedTextWithRolePrefix,
    InlineParsers.uri,
    InlineParsers.email,
    SpanParser.forStartChar('\\').standalone(escapedChar ^^ { Text(_) }).withLowPrecedence // TODO - extract
  )

  /** Builds a parser for a list of blocks based on the parser for a single block.
    *
    *  Overridden to add the processing required for cases where a block has influence
    *  on the parsing or processing of the subsequent block.
    *
    *  This includes checking each Paragraph for a double colon ending which turns
    *  the following block into a literal block as well as processing internal
    *  link targets and section headers.
    *
    *  @param parser the parser for a single block element
    *  @return a parser for a list of blocks
    */
  override def blockList (parser: => Parser[Block]): Parser[Seq[Block]] = Parser { in =>
    case object Mock extends Block { val options = NoOpt }

    val defaultBlock = parser <~ opt(blankLines)
    val litBlock = (BlockParsers.literalBlock | defaultBlock) <~ opt(blankLines)
    val elems = new ListBuffer[Block]
    elems += Mock

    def processLiteralMarker (par: Paragraph) = {
      par.content.lastOption match {
        case Some(Text(text,opt)) if text.trim.endsWith("::") =>
          val drop = if (text.length > 2 && text.charAt(text.length-3) == ' ') 3 else 1
          val spans = par.content.init.toList ::: List(Text(text.dropRight(drop),opt))
          (Paragraph(spans,par.options), litBlock)
        case _ => (par, defaultBlock)
      }
    }
    def toLinkId (h: DecoratedHeader) = ReferenceName(TreeUtil.extractText(h.content)).normalized

    def result = {
      elems += Mock
      val processed = elems.toList.sliding(3).foldLeft(new ListBuffer[Block]()) {
        case (buffer, _ :: (InternalLinkTarget(Id(id1))) :: (InternalLinkTarget(Id(id2))) :: Nil) =>
          buffer += LinkAlias(id1, id2)
        case (buffer, _ :: (InternalLinkTarget(Id(id))) :: (et: ExternalLinkDefinition) :: Nil) =>
          buffer += et.copy(id = id)
        case (buffer, _ :: (it: InternalLinkTarget) :: (h: DecoratedHeader) :: Nil) => buffer
        case (buffer, _ :: (it: InternalLinkTarget) :: (c: Customizable) :: Nil) =>
          if (c.options.id.isDefined) buffer += it else buffer
        case (buffer, _ :: (it: InternalLinkTarget) :: _ :: Nil) => buffer += it

        case (buffer, (it: InternalLinkTarget) :: (h @ DecoratedHeader(_,_,oldOpt)) :: _) =>
          buffer += h.copy(options = oldOpt + Id(toLinkId(h)), content = it +: h.content)
        case (buffer, _ :: (h @ DecoratedHeader(_,_,oldOpt)) :: _) =>
          buffer += h.copy(options = oldOpt + Id(toLinkId(h)))

        case (buffer, (InternalLinkTarget(Id(id))) :: (et: ExternalLinkDefinition) :: _ :: Nil) =>
          buffer += et
        case (buffer, (InternalLinkTarget(Id(id))) :: (c: Customizable) :: _ :: Nil) if c.options.id.isEmpty =>
          buffer += TreeUtil.setId(c, id)

        case (buffer, _ :: _ :: Nil)   => buffer // only happens for empty results (with just the 2 mocks)
        case (buffer, _ :: other :: _) => buffer += other
        case (buffer, _)          => buffer
      }
      processed.toList
    }

    // TODO - could use repWith
    @tailrec
    def parse (p: Parser[Block], in: ParserContext): Parsed[List[Block]] = p.parse(in) match {
      case Success(Paragraph(Text(txt,_) :: Nil,_), rest) if txt.trim == "::" => parse(litBlock, rest)
      case Success(p: Paragraph, rest) =>
        val (paragraph, parser) = processLiteralMarker(p)
        elems += paragraph
        parse(parser, rest)
      case Success(x, rest) => elems += x; parse(defaultBlock, rest)
      case _                => Success(result, in)
    }

    parse(defaultBlock, in)
  }


}
