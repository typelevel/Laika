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

import laika.ast.{Block, CodeBlock, LiteralBlock, Text, ~}
import laika.bundle.{BlockParser, BlockParserBuilder}
import laika.parse.text.TextParsers._

/** Parser for fenced code blocks as defined by GitHub Flavored Markdown and CommonMark.
  *
  * For the spec see [[https://github.github.com/gfm/#fenced-code-blocks]].
  *
  * This implementation differs from the spec in one aspect: a fenced code block must be preceded by a blank line.
  * This is due to technical reasons of fenced code blocks being an extension and the default paragraph parser
  * not knowing about the installed extensions and which of them are allowed to interrupt a paragraph.
  * Later version might support the exact spec, but that would require changes in how parser extensions
  * are registered and combined with the native parsers.
  *
  * @author Jens Halm
  */
object FencedCodeBlocks {

  /** Creates a parser for a fenced code block with the specified fence character.
    */
  def codeBlock (fenceChar: Char): BlockParserBuilder = BlockParser.forStartChar(fenceChar).recursive { recParsers =>

    val infoString = restOfLine.map(Some(_)
      .filter(_.nonEmpty)
      .flatMap(_.trim.split(" ").headOption)
    )
    val openingFence = anyOf(fenceChar).min(2).count ~ infoString

    openingFence >> { case charCount ~ info =>

      val closingFence = anyOf(fenceChar).min(charCount + 1) ~ wsEol

      (not(closingFence | eof) ~> restOfLine).rep <~ opt(closingFence) ^^? { lines =>
        val trimmedLines = if (lines.lastOption.exists(_.trim.isEmpty)) lines.dropRight(1) else lines
        val code = trimmedLines.mkString("\n")
        info.fold[Either[String, Block]](Right(LiteralBlock(code))) { lang =>
          val highlighter = recParsers.getSyntaxHighlighter(lang).getOrElse(any ^^ { txt => Seq(Text(txt)) })
          val blockParser = highlighter ^^ { codeSpans => CodeBlock(lang, codeSpans) }
          blockParser.parse(code).toEither
        }
      }
    }
  }

  /** Parsers for fenced code blocks delimited by any of the two fence characters
    * defined by GitHub Flavored Markdown (tilde and backtick).
    */
  val parsers: Seq[BlockParserBuilder] = Seq('~','`').map(codeBlock)

}
