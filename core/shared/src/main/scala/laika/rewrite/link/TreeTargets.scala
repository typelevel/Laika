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

package laika.rewrite.link

import laika.ast.Path.Root
import laika.ast.{DocumentTreeRoot, Path}
import laika.rewrite.link.LinkTargets.{PathSelector, ReferenceResolver, Selector, TargetReplacer, TargetResolver, UniqueSelector}

/** Collects all elements from a document tree that can be referenced from other elements, 
  * like images, footnotes, citations and other inline targets. 
  *
  * @author Jens Halm
  */
class TreeTargets (root: DocumentTreeRoot, slugBuilder: String => String) {

  private val targetMap: Map[(Path, Selector), TargetResolver] = {
    
    def allPaths (path: Path): Seq[Path] =
      if (path == Root) Seq(Root)
      else allPaths(path.parent) :+ path
    
    val targets: Seq[((Path, Selector), TargetResolver)] = root.allDocuments.flatMap { doc =>
      val targets = new DocumentTargets(doc)
      val global = if (doc.path == Root) Nil else for {
        path   <- allPaths(doc.path.parent)
        target <- targets.global // TODO - should be a single Seq, not two Maps
      } yield ((path, target._1), target._2)
      val local = targets.local.map(target => ((doc.path, target._1), target._2))
      global ++ local
    }
    def staticTarget (path: Path) =
      TargetResolver.create(PathSelector(path), ReferenceResolver.internalLink(path), TargetReplacer.removeTarget)
    val static = root.staticDocuments.map(path => ((Root, PathSelector(path)), staticTarget(path)))
    
    (targets ++ static).groupBy(_._1).collect {
      case (key, Seq((_, target))) => (key, target)
      case ((path, selector: UniqueSelector), dupTargets) => 
        ((path, selector), TargetResolver.forDuplicateSelector(selector, path, dupTargets.map(_._2)))
    }
  }

  /** Selects a target resolver with the specified selector within the scope
    * of the given path. A resolver can be looked up in any scope from the document
    * it occurred in or any of its parent trees up to the root tree. Some selectors,
    * like `TargetIdSelector` can be used to define a globally reachable target.
    * Others, like those for auto-numbered footnotes will only be looked up locally,
    * with the path of the current document.
    */
  def select (scope: Path, selector: Selector): Option[TargetResolver] = {
    targetMap.get((scope, selector))
  }
  
}
