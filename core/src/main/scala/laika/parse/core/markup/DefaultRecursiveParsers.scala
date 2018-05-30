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

package laika.parse.core.markup

import laika.parse.core._
import laika.parse.core.text.TextParsers._
import laika.tree.Elements.{Block, Error, InvalidBlock, Paragraph, SystemMessage, Text}

/** Default implementation for parsing inline markup and blocks recursively.
  *
  * @author Jens Halm
  */
trait DefaultRecursiveParsers extends RecursiveParsers with DefaultRecursiveSpanParsers {


  /** The maximum level of block nesting. Some block types like lists
    * and blockquotes contain nested blocks. To protect against malicious
    * input or accidentally broken markup, the level of nesting is restricted.
    */
  val maxNestLevel: Int = 12

  /** Parses any kind of top-level block supported by a concrete markup language.
    */
  protected def topLevelBlock: Parser[Block]

  /** Parses any kind of nested block supported by a concrete markup language.
    */
  protected def nestedBlock: Parser[Block]

  /** Parses blocks, excluding blocks that allow nesting.
    *  Only used in rare cases when the maximum nest level allowed had been reached
    */
  protected def nonRecursiveBlock: Parser[Block]


  private class RecursiveBlockParser {

    lazy val recursive    = consumeAll(opt(blankLines) ~> blockList(nestedBlock))
    lazy val nonRecursive = consumeAll(opt(blankLines) ~> blockList(nonRecursiveBlock))

    def parse (source: String, nestLevel: Int): Parsed[List[Block]] = {
      val p = if (nestLevel < maxNestLevel) recursive else nonRecursive
      p.parse(ParserContext(source, nestLevel + 1))
    }

  }

  private val recursiveBlockParser: RecursiveBlockParser = new RecursiveBlockParser


  def recursiveBlocks (p: Parser[String]): Parser[Seq[Block]] = Parser { ctx =>
    p.parse(ctx) match {
      case Success(str, next) =>
        recursiveBlockParser.parse(str, ctx.nestLevel) match {
          case Success(blocks, _) => Success(blocks, next)
          case f: Failure => f
        }
      case f: Failure => f
    }
  }

  def withRecursiveBlockParser [T] (p: Parser[T]): Parser[(String => List[Block], T)] = Parser { ctx =>
    p.parse(ctx) match {
      case Success(res, next) =>
        val recParser: String => List[Block] = { source: String =>
          recursiveBlockParser.parse(source, next.nestLevel) match {
            case Success(blocks, _) => blocks
            case Failure(msg, next) =>
              val message = SystemMessage(Error, msg.message(next))
              val fallback = Paragraph(Seq(Text(source)))
              List(InvalidBlock(message, fallback))
          }
        }
        Success((recParser, res), next)
      case f: Failure => f
    }
  }

  /** Builds a parser for a list of blocks based on the parser for a single block.
    */
  def blockList (p: => Parser[Block]): Parser[List[Block]] = (p <~ opt(blankLines))*

}
