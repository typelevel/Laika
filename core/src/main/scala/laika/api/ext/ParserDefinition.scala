/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.api.ext

import laika.parse.core.Parser
import laika.parse.core.text.TextParsers.char
import laika.tree.Elements.{Block, Element, Span}

/**
  * @author Jens Halm
  */
sealed trait ParserDefinition[E <: Element] {

  def parser: Parser[E]

  def isRecursive: Boolean

  def precedence: Precedence

}

case class BlockParserDefinition (startChar: Option[Char],
                                  parser: Parser[Block],
                                  isRecursive: Boolean,
                                  position: BlockPosition,
                                  precedence: Precedence) extends ParserDefinition[Block] {
  val fullParser = startChar.fold(parser)(_ ~> parser)
}

case class SpanParserDefinition (startChar: Char,
                                 parser: Parser[Span],
                                 isRecursive: Boolean,
                                 precedence: Precedence) extends ParserDefinition[Span]

sealed trait Precedence
object Precedence {
  object High extends Precedence
  object Low extends Precedence
}

sealed trait BlockPosition
object BlockPosition {
  case object Any extends BlockPosition
  case object RootOnly extends BlockPosition
  case object NestedOnly extends BlockPosition
}
