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

package laika.internal.rst

import cats.data.NonEmptyChain
import laika.ast._
import laika.parse.builders._
import laika.parse.syntax._
import laika.parse.text.Characters
import laika.parse.{
  BlockSource,
  LineSource,
  Parsed,
  Parser,
  SourceCursor,
  SourceFragment,
  Success
}
import BaseParsers._
import laika.api.bundle.BlockParserBuilder
import laika.internal.rst.ast.{ DoctestBlock, OverlineAndUnderline, Underline }

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

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
private[laika] object BlockParsers {

  // other whitespace has been replaced with spaces by preprocessor
  val ws: Characters[String] = anyOf(' ')

  val transition: BlockParserBuilder = BlockParserBuilder.standalone {
    (punctuationChar.min(4) ~ wsEol ~ lookAhead(blankLine)).as(Rule())
  }

  lazy val paragraph: BlockParserBuilder = BlockParserBuilder.recursive { recParsers =>
    val interruptions = recParsers.paragraphInterruptions()
    val lines         = textLine.line.repUntil(interruptions)

    lines.evalMap { case (block, interruption) =>
      NonEmptyChain.fromSeq(block) match {
        case None      => Left("empty paragraph")
        case Some(res) =>
          val par = Paragraph(recParsers.recursiveSpans.parseAndRecover(BlockSource(res)))
          Right(interruption.fold[Block](par)(BlockSequence(par, _)))
      }
    }
  }

  private def stripTrailingNewline(source: SourceFragment): SourceFragment =
    if (source.input.lastOption.contains('\n')) LineSource(source.input.dropRight(1), source.root)
    else source

  lazy val headerWithOverline: BlockParserBuilder = BlockParserBuilder.withSpans { spanParsers =>
    val spanParser = punctuationChar.take(1) >> { start =>
      val char = start.charAt(0)
      anyOf(char) >> { deco =>
        val len      = deco.length + 1
        val text     = spanParsers.recursiveSpans(anyNot('\n').max(len).trim.line)
        val decoLine = anyOf(char).take(len)

        (wsEol ~ ws ~> text <~ wsEol ~ decoLine ~ wsEol).map { title =>
          (deco.head, title)
        }
      }
    }
    spanParser.withCursor.map { case ((decoChar, title), source) =>
      DecoratedHeader(OverlineAndUnderline(decoChar), title, stripTrailingNewline(source))
    }
  }

  lazy val headerWithUnderline: BlockParserBuilder = BlockParserBuilder.withSpans { spanParsers =>
    val spanParser = nextNot(' ') ~ not(eof) ~> restOfLine.trim.line >> { title =>
      punctuationChar.take(1) >> { start =>
        val char = start.charAt(0)
        (anyOf(char).min(title.length - 1).line ~ wsEol).map { _ =>
          (char, title)
        }
      }
    }
    spanParser.withCursor.map { case ((decoChar, title), source) =>
      DecoratedHeader(
        Underline(decoChar),
        spanParsers.recursiveSpans.parseAndRecover(title),
        stripTrailingNewline(source)
      )
    }
  }

  val doctest: BlockParserBuilder = BlockParserBuilder.standalone {
    val lineParser = restOfLine.rep(not(blankLine)).min(1)
    ">>> " ~> lineParser.mkLines.map(DoctestBlock(_))
  }

  lazy val blockQuote: BlockParserBuilder = BlockParserBuilder.recursive { recParsers =>
    val attributionStart = "---" | "--" | "\u2014" // em dash

    def attribution(indent: Int) = ws.take(indent) ~ attributionStart ~ ws.max(1) ~>
      recParsers.recursiveSpans(indentedBlock(minIndent = indent, endsOnBlankLine = true))

    nextIn(' ') ~> indentedBlockWithLevel(
      firstLineIndented = true,
      linePredicate = not(attributionStart)
    ) >> { case (block, minIndent) =>
      opt(opt(blankLines) ~> attribution(minIndent)).map { spans =>
        QuotedBlock(recParsers.recursiveBlocks.parse(block).getOrElse(Nil), spans.getOrElse(Nil))
      }
    }
  }

  val literalBlock: Parser[Block] = {
    val indented = indentedBlock(firstLineIndented = true).map(src => LiteralBlock(src.input))

    val quotedLine = nextIn(punctuationChars)
    val quoted     = block(quotedLine, quotedLine, failure("blank line always ends quoted block"))
      .map(src => LiteralBlock(src.input))

    indented | quoted
  }

  /** Builds a parser for a list of blocks based on the parser for a single block.
    *
    * Adds the processing required for cases where a block has influence
    * on the parsing or processing of the subsequent block.
    *
    * This includes checking each Paragraph for a double colon ending which turns
    * the following block into a literal block as well as processing internal
    * link targets and section headers.
    *
    * @param blockParser the parser for a single block element
    * @return a parser for a list of blocks
    */
  def createBlockListParser(blockParser: Parser[Block]): Parser[Seq[Block]] = Parser { in =>
    val defaultBlock = blockParser <~ opt(blankLines)
    val litBlock     = (BlockParsers.literalBlock | defaultBlock) <~ opt(blankLines)
    val elems        = new ListBuffer[Block]

    def processLiteralMarker(par: Paragraph) = {
      par.content.lastOption match {
        case Some(Text(text, opt)) if text.trim.endsWith("::") =>
          val drop  = if (text.length > 2 && text.charAt(text.length - 3) == ' ') 3 else 1
          val spans = par.content.init.toList ::: List(Text(text.dropRight(drop), opt))
          (Paragraph(spans, par.options), litBlock)
        case _                                                 => (par, defaultBlock)
      }
    }

    @tailrec
    def parse(p: Parser[Block], in: SourceCursor): Parsed[Seq[Block]] = p.parse(in) match {
      case Success(Paragraph(Text(txt, _) :: Nil, _), rest) if txt.trim == "::" =>
        parse(litBlock, rest)
      case Success(p: Paragraph, rest)                                          =>
        val (paragraph, parser) = processLiteralMarker(p)
        elems += paragraph
        parse(parser, rest)
      case Success(x, rest) => elems += x; parse(defaultBlock, rest)
      case _ => Success(elems.toList, in)
    }

    parse(defaultBlock, in)
  }

}
