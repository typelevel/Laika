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

package laika.parse.rst

import laika.parse.rst.Elements.ReferenceName
import laika.rewrite.TreeUtil
import laika.tree.Elements._

import scala.collection.mutable.ListBuffer

/**
  * @author Jens Halm
  */
object LinkTargetProcessor extends (Seq[Block] => Seq[Block]) {

  def apply (blocks: Seq[Block]): Seq[Block] = {

    case object Mock extends Block { val options = NoOpt }

    def toLinkId (h: DecoratedHeader) = ReferenceName(h.extractText).normalized

    // TODO - simplify
    (Mock +: blocks :+ Mock).sliding(3).foldLeft(new ListBuffer[Block]()) {

      case (buffer, _ :: (InternalLinkTarget(Id(id1))) :: (InternalLinkTarget(Id(id2))) :: Nil) =>
        buffer += LinkAlias(id1, id2)
      case (buffer, _ :: (InternalLinkTarget(Id(id))) :: (et: ExternalLinkDefinition) :: Nil) =>
        buffer += et.copy(id = id)
      case (buffer, _ :: (_: InternalLinkTarget)  :: (_: DecoratedHeader) :: Nil) => buffer
      case (buffer, _ :: (it: InternalLinkTarget) :: (c: Customizable) :: Nil) =>
        if (c.options.id.isDefined) buffer += it else buffer
      case (buffer, _ :: (it: InternalLinkTarget) :: _ :: Nil) => buffer += it

      case (buffer, (it: InternalLinkTarget) :: (h @ DecoratedHeader(_,_,oldOpt)) :: _) =>
        buffer += h.copy(options = oldOpt + Id(toLinkId(h)), content = it +: h.content)
      case (buffer, _ :: (h @ DecoratedHeader(_,_,oldOpt)) :: _) =>
        buffer += h.copy(options = oldOpt + Id(toLinkId(h)))

      case (buffer, (InternalLinkTarget(Id(_))) :: (et: ExternalLinkDefinition) :: _ :: Nil) =>
        buffer += et
      case (buffer, (InternalLinkTarget(Id(id))) :: (c: Customizable) :: _ :: Nil) if c.options.id.isEmpty =>
        buffer += TreeUtil.setId(c, id)

      case (buffer, _ :: _ :: Nil)   => buffer // only happens for empty results (with just the 2 mocks)
      case (buffer, _ :: other :: _) => buffer += other
      case (buffer, _)          => buffer
    }
  }.toList

}
