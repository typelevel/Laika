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

package laika.epub.internal

import laika.ast.Path
import laika.io.model.RenderedTreeRoot

/** Processes CSS inputs for EPUB containers.
  *
  * @author Jens Halm
  */
private[epub] object StyleSupport {

  /** Collects all CSS inputs (recursively) in the provided document tree.
    * CSS inputs are recognized by file suffix).
    */
  def collectStylePaths[F[_]](root: RenderedTreeRoot[F]): Seq[Path] =
    root.staticDocuments.collect { case doc if doc.path.suffix.contains("css") => doc.path }

}
