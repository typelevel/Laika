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

package laika.internal.rst.bundle

import laika.ast._

import scala.collection.mutable.ListBuffer

/** Processes link targets in a sequence of blocks.
  *
  * In reStructuredText internal link targets provide the id of subsequent block items.
  * This functions gets applied recursively to all block lists.
  *
  * @author Jens Halm
  */
private[laika] object LinkTargetProcessor extends (Seq[Block] => Seq[Block]) {

  def apply(blocks: Seq[Block]): Seq[Block] = {

    case object Mock extends Block {
      val options: Options = Options.empty
      type Self = Mock.type
      def withOptions(options: Options): Mock.type = this
    }

    // TODO - simplify
    (Mock +: blocks :+ Mock).sliding(3).foldLeft(new ListBuffer[Block]()) {

      case (buffer, _ :: InternalLinkTarget(Id(id1)) :: InternalLinkTarget(Id(id2)) :: Nil) =>
        buffer += LinkAlias(id1, id2)
      case (buffer, _ :: InternalLinkTarget(Id(id)) :: (ld: LinkDefinition) :: Nil)         =>
        buffer += ld.copy(id = id)
      case (buffer, _ :: (_: InternalLinkTarget) :: (_: DecoratedHeader) :: Nil) => buffer
      case (buffer, _ :: (it: InternalLinkTarget) :: elem :: Nil)                =>
        if (elem.hasId) buffer += it else buffer

      case (buffer, (it: InternalLinkTarget) :: (h: DecoratedHeader) :: _) =>
        buffer += h.copy(content = it +: h.content)

      case (buffer, InternalLinkTarget(Id(_)) :: (ld: LinkDefinition) :: _ :: Nil) => buffer += ld
      case (buffer, InternalLinkTarget(Id(id)) :: elem :: _ :: Nil) if !elem.hasId =>
        buffer += elem.withId(id)

      case (buffer, _ :: _ :: Nil) =>
        buffer // only happens for empty results (with just the 2 mocks)
      case (buffer, _ :: other :: _) => buffer += other
      case (buffer, _)               => buffer
    }
  }.toList

}
