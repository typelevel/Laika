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

package laika.bundle

import cats.data.NonEmptySet
import laika.ast.{ Block, Element, Span }
import laika.parse.Parser
import laika.parse.text.PrefixedParser

/** Defines a parser for a single kind of text markup,
  * like a literal text span or a bullet list for example.
  *
  * @author Jens Halm
  */
sealed trait ParserDefinition[E <: Element] {

  /** The parser for the block or span element.
    */
  def parser: Parser[E]

  /** Indicates whether this parser produces child elements
    * by recursively applying the parsers for the host language.
    */
  def isRecursive: Boolean

  /** Indicates whether the parser should be applied before the base parsers of
    * the host language (high precedence) or after them.
    */
  def precedence: Precedence

}

/** Defines a parser for a single kind of block element,
  * like a quoted block or a bullet list for example.
  *
  * @param startChars the optional start characters that can start this block (allows performance optimizations when defined)
  * @param parser the parser for the block element
  * @param isRecursive indicates whether this parser produces child elements by recursively applying the parsers for the host language
  * @param position indicates whether this parser is responsible for root or nested elements only, or for both
  * @param precedence indicates whether the parser should be applied before the base parsers of
  *                   the host language (high precedence) or after them
  * @param paragraphLineCheck a test for the start of each line in plain paragraphs that indicates whether the line might
  *                           be the start of a block identified by this parser
  */
class BlockParserDefinition(
    val startChars: Set[Char],
    val parser: Parser[Block],
    val isRecursive: Boolean,
    val position: BlockPosition,
    val precedence: Precedence,
    val paragraphLineCheck: Option[PrefixedParser[Any]] = None
) extends ParserDefinition[Block]

/** Defines a parser for a single kind of span element,
  * like a literal text span or a link reference for example.
  *
  * @param startChars all start characters that can start this span (allows performance optimizations)
  * @param parser the parser for the span element
  * @param isRecursive indicates whether this parser produces child elements by recursively applying the parsers for the host language
  * @param precedence indicates whether the parser should be applied before the base parsers of
  * the host language (high precedence) or after them
  */
class SpanParserDefinition(
    val startChars: NonEmptySet[Char],
    val parser: Parser[Span],
    val isRecursive: Boolean,
    val precedence: Precedence
) extends ParserDefinition[Span]

/** Indicates whether a parser should be applied before the base parsers of
  * the host language (high precedence) or after them (low precedence).
  */
sealed trait Precedence

object Precedence {

  implicit val ordering: Ordering[Precedence] = new Ordering[Precedence] {

    def compare(x: Precedence, y: Precedence): Int = (x, y) match {
      case (Precedence.High, Precedence.Low) => 1
      case (Precedence.Low, Precedence.High) => -1
      case _                                 => 0
    }

  }

  object High extends Precedence
  object Low  extends Precedence
}

/** Specifies the position a block element is allowed to appear in.
  *
  * `RootOnly` elements can only appear on the top level of the document hierarchy,
  * while `NestedOnly` can only appear nested within other elements, e.g. as part
  * of a list item or within a quoted block. `Any` allows block elements in any position.
  */
sealed trait BlockPosition

object BlockPosition {
  case object Any        extends BlockPosition
  case object RootOnly   extends BlockPosition
  case object NestedOnly extends BlockPosition
}
