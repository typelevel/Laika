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

import laika.ast.Path.Root

import scala.annotation.tailrec

/** Generically builds a tree structure out of a flat sequence of elements with a `Path` property that
  * signifies the position in the tree. Essentially factors recursion out of the tree building process.
  *
  * @author Jens Halm
  */
object TreeBuilder {

  /** Builds a tree structure from the specified leaf elements, using the given builder function.
    * The function will be invoked for each node recursively, with the path for the node to build
    * and the child nodes that are immediate children of the node to build.
    */
  def build[C <: Navigatable, T <: C](content: Seq[C], buildNode: (Path, Seq[C]) => T): T = {

    def toMap(items: Iterable[C]): Map[Path, C] = items.map(c => (c.path, c)).toMap

    def buildNodes(depth: Int, contentMap: Map[Path, C]): Seq[T] = {

      @tailrec def parent(p: Path, levels: Int): Path =
        if (levels <= 0) p else parent(p.parent, levels - 1)

      val newNodes = content
        .filter(_.path.parent.depth >= depth)
        .map(p => parent(p.path, p.path.depth - depth - 1))
        .distinct
        .groupBy(_.parent)
        .map { case (parent, contentPaths) =>
          buildNode(parent, contentPaths.map(contentMap))
        }

      if (depth == 0) newNodes.toSeq
      else buildNodes(depth - 1, contentMap ++ toMap(newNodes))
    }

    if (content.isEmpty) buildNode(Root, Nil)
    else {
      val maxPathLength = content.map(_.path.parent.depth).max
      buildNodes(maxPathLength, toMap(content)).head
    }
  }

}
