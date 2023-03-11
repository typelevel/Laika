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

package laika.ast

import scala.annotation.tailrec

/** The position of an element within a document tree.
  *
  * @author Jens Halm
  */
class TreePosition private (private val positions: Option[Seq[Int]]) extends Ordered[TreePosition] {

  /** The positions (one-based) of each nesting level of this instance
    * (an empty sequence for the root or orphan positions).
    */
  def toSeq: Seq[Int] = positions.getOrElse(Nil)

  override def toString: String = positions match {
    case None      => "TreePosition.orphan"
    case Some(Nil) => "TreePosition.root"
    case Some(pos) => s"TreePosition(${pos.mkString(".")})"
  }

  /** This tree position as a span that can get rendered
    * as part of a numbered title for example.
    */
  def toSpan: Span = SectionNumber(toSeq)

  /** The depth (or nesting level) of this position within the document tree.
    */
  def depth: Int = toSeq.size

  /** Creates a position instance for a child of this element.
    */
  def forChild(childPos: Int) = TreePosition(toSeq :+ childPos)

  def compare(other: TreePosition): Int = {

    @tailrec
    def compare(pos1: Seq[Int], pos2: Seq[Int]): Int = (pos1.headOption, pos2.headOption) match {
      case (Some(a), Some(b)) =>
        a.compare(b) match {
          case 0       => compare(pos1.tail, pos2.tail)
          case nonZero => nonZero
        }
      case _                  => 0
    }

    val maxLen = Math.max(toSeq.length, other.toSeq.length)
    compare(toSeq.padTo(maxLen, 0), other.toSeq.padTo(maxLen, 0))
  }

  override def hashCode(): Int = positions.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case tp: TreePosition => tp.positions == positions
  }

}

object TreePosition {
  def apply(pos: Seq[Int]) = new TreePosition(Some(pos))
  val root                 = new TreePosition(Some(Nil))

  /** A document has an orphaned position assigned when it is processed on its own,
    * not as part of a tree of documents.
    */
  val orphan = new TreePosition(None)
}
