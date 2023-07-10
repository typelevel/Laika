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

package laika.markdown.github

import cats.data.NonEmptyChain
import laika.ast.{ CodeBlock, LiteralBlock, Span, Text }
import laika.bundle.BlockParserBuilder
import laika.parse.builders._
import laika.parse.implicits._
import laika.parse.{ BlockSource, Failure, Parser, Success }

/** Parser for fenced code blocks as defined by GitHub Flavored Markdown and CommonMark.
  *
  * For the spec see [[https://github.github.com/gfm/#fenced-code-blocks]].
  *
  * @author Jens Halm
  */
object FencedCodeBlocks {

  private def reverse(offset: Int, p: => Parser[String]): Parser[String] = Parser { in =>
    p.parse(in.reverse.consume(offset)) match {
      case Success(result, _) => Success(result.reverse, in)
      case Failure(msg, _, _) => Failure(msg, in)
    }
  }

  /** Creates a parser for a fenced code block with the specified fence character.
    */
  def codeBlock(fenceChar: Char): BlockParserBuilder = BlockParserBuilder.recursive { recParsers =>
    val infoString                       = restOfLine.map(
      Some(_)
        .filter(_.nonEmpty)
        .flatMap(_.trim.split(" ").headOption)
    )
    def indent(offset: Int): Parser[Int] = reverse(offset, anyOf(' ')).map(_.length)
    val openingFence                     = someOf(fenceChar).min(3).count >> { fence =>
      (indent(fence) ~ infoString).mapN((fence, _, _))
    }

    openingFence >> { case (fenceLength, indent, info) =>
      val closingFence = anyOf(' ').max(3) ~ anyOf(fenceChar).min(fenceLength) ~ wsEol
      val lines        = (not(closingFence | eof) ~ anyOf(' ').max(indent) ~> restOfLine.line).rep

      (lines <~ opt(closingFence)).evalMap { lines =>
        val trimmedLines =
          if (lines.lastOption.exists(_.input.trim.isEmpty)) lines.dropRight(1) else lines
        val codeSource   = NonEmptyChain.fromSeq(trimmedLines).map(BlockSource(_))
        (info, codeSource) match {
          case (Some(lang), Some(src)) =>
            recParsers.getSyntaxHighlighter(lang).fold[Either[String, Seq[Span]]](
              Right(Seq(Text(src.input)))
            ) { highlighter =>
              highlighter.parse(src).toEither
            }.map { CodeBlock(lang, _) }
          case _                       =>
            Right(LiteralBlock(trimmedLines.map(_.input).mkString("\n")))
        }
      }
    }
  }.interruptsParagraphWith(oneOf(fenceChar))

  /** Parsers for fenced code blocks delimited by any of the two fence characters
    * defined by GitHub Flavored Markdown (tilde and backtick).
    */
  val parsers: Seq[BlockParserBuilder] = Seq('~', '`').map(codeBlock)

}
