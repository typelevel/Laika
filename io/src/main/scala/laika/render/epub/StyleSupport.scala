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

package laika.render.epub

import laika.ast.{DocumentTreeRoot, Path}
import laika.io.model.RenderedTreeRoot

/** Processes CSS inputs for EPUB containers.
  *
  * @author Jens Halm
  */
object StyleSupport {

  /** Path for the fallback styles that will be inserted
    * when the user has not added any CSS documents to the input tree.
    */
  val fallbackStylePath: Path = Path.Root / "styles" / "fallback.css"

  /** Collects all CSS inputs (recursively) in the provided document tree.
    * CSS inputs are recognized by file suffix).
    */
  def collectStylePaths[F[_]] (root: RenderedTreeRoot[F]): Seq[Path] = 
    root.staticDocuments.collect { case doc if doc.path.suffix.contains("css") => doc.path }

  /** Verifies that the specified document tree contains at least one CSS file
    * (determined by file suffix). If this is the case the tree is returned unchanged,
    * otherwise a new tree with a minimal fallback CSS inserted into the root is returned instead.
    */
  def ensureContainsStyles (root: DocumentTreeRoot): DocumentTreeRoot = {
    if (root.staticDocuments.exists(_.suffix.contains("css"))) root
    else root.copy(staticDocuments = root.staticDocuments :+ fallbackStylePath)
  }

}
