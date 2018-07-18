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

package laika.parse.markdown

import laika.api.ext._
import laika.parse.core.Parser
import laika.parse.core.markup.RootParserBase
import laika.parse.core.text.TextParsers.anyOf
import laika.rewrite.TreeUtil
import laika.tree.Elements._

/** The main parser for Markdown, combining the individual parsers for block and inline elements,
  * and adding functionality like directives or verbatim HTML depending on configuration.
  *
  * @author Jens Halm
  */
class RootParser (val blockParserExtensions: Seq[BlockParserBuilder] = Nil,
                  val spanParserExtensions: Seq[SpanParserBuilder] = Nil,
                  isStrict: Boolean = false) extends RootParserBase {

  /** Parses a single escaped character, only recognizing the characters the Markdown syntax document
    *  specifies as escapable.
    *
    *  Note: escaping > is not mandated by the official syntax description, but by the official test suite.
    */
  override lazy val escapedChar: Parser[String] = anyOf('\\', '`', '*', '_', '{', '}', '[', ']', '(', ')', '#', '+', '-', '.', '!', '>') take 1

  val mainBlockParsers = Seq(
    BlockParsers.atxHeader,
    BlockParsers.linkTarget,
    BlockParsers.quotedBlock,
    BlockParsers.rootHeaderOrParagraph,
    BlockParsers.nestedHeaderOrParagraph
  ) ++
    BlockParsers.literalBlocks ++
    BlockParsers.rules ++
    ListParsers.enumLists ++
    ListParsers.bulletLists

  val mainSpanParsers = Seq(
    InlineParsers.enclosedByAsterisk,
    InlineParsers.enclosedByUnderscore,
    InlineParsers.literalSpan,
    InlineParsers.image,
    InlineParsers.link,
    InlineParsers.simpleLink,
    InlineParsers.lineBreak,
    SpanParser.forStartChar('\\').standalone(escapedChar ^^ { Text(_) }).withLowPrecedence // TODO - extract
  )

  // TODO - could be rewrite rule - don't use in strict mode - remove strict flag from this class
  override def blockList (parser: => Parser[Block]): Parser[List[Block]] =
    if (isStrict) super.blockList(BlockParsers.insignificantSpaces ~> parser)
    else super.blockList(BlockParsers.insignificantSpaces ~> parser) ^^ {
      _ map { case h: Header =>
        h.copy(options = h.options + Id(TreeUtil.extractText(h.content).replaceAll("[\n ]+", " ").toLowerCase))
      case other => other
      }
    }

}
