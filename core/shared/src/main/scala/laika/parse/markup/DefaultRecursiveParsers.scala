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

package laika.parse.markup

import laika.ast._
import laika.parse._
import laika.parse.builders._

/** Default implementation for parsing inline markup and blocks recursively.
  *
  * @author Jens Halm
  */
private[parse] trait DefaultRecursiveParsers extends RecursiveParsers
    with DefaultRecursiveSpanParsers {

  /** The maximum level of block nesting. Some block types like lists
    * and blockquotes contain nested blocks. To protect against malicious
    * input or accidentally broken markup, the level of nesting is restricted.
    */
  private val maxNestLevel: Int = 12

  /** Parses any kind of top-level block supported by a concrete markup language.
    */
  protected def rootBlock: Parser[Block]

  /** Parses any kind of nested block supported by a concrete markup language.
    */
  protected def nestedBlock: Parser[Block]

  /**  Parses blocks, excluding blocks that allow nesting.
    *  Only used in rare cases when the maximum nest level allowed had been reached
    */
  protected def fallbackBlock: Parser[Block]

  /** Builds a parser for a list of blocks based on the parser for a single block.
    */
  protected def blockList(p: => Parser[Block]): Parser[Seq[Block]]

  private class InternalRecursiveBlockParser {

    lazy val recursive    = consumeAll(opt(blankLines) ~> blockList(nestedBlock))
    lazy val nonRecursive = consumeAll(opt(blankLines) ~> blockList(fallbackBlock))

    def parse(source: SourceCursor): Parsed[Seq[Block]] = {
      val p = if (source.nestLevel < maxNestLevel) recursive else nonRecursive
      p.parse(source.nextNestLevel)
    }

  }

  private val recursiveBlockParser: InternalRecursiveBlockParser = new InternalRecursiveBlockParser

  def recursiveBlocks(p: Parser[BlockSource]): Parser[Seq[Block]] = Parser { in =>
    p.parse(in) match {
      case Success(str, next) =>
        recursiveBlockParser.parse(str) match {
          case Success(blocks, _) => Success(blocks, next)
          case f: Failure         => f
        }
      case f: Failure         => f
    }
  }

  def recursiveBlocks: RecursiveBlockParser = new RecursiveBlockParser {

    def parse(in: SourceFragment): Parsed[Seq[Block]] = recursiveBlockParser.parse(in)

    def parseAndRecover(in: SourceFragment): Seq[Block] = recursiveBlockParser.parse(in) match {
      case Success(blocks, _) => blocks
      case f: Failure         => List(InvalidBlock(f.message, in))
    }

  }

}
