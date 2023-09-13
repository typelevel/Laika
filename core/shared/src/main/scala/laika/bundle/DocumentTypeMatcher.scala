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

import laika.ast.{ DocumentType, Path }
import laika.rewrite.nav.TargetFormats

/** The default implementations for determining the document type
  * of the input based on its path.
  *
  *  @author Jens Halm
  */
object DocumentTypeMatcher {

  import DocumentType.*

  /* These are "fallback formats" that can be overridden by users or theme authors
   * with 'laika.targetFormats = [...]' in `directory.conf`. */
  private def staticTargetFormats(path: Path): TargetFormats = path.suffix match {
    case Some("css") | Some("js") => TargetFormats.Selected("html", "epub", "xhtml")
    case _                        => TargetFormats.All
  }

  /** The base matcher that recognizes all file types known to Laika
    * except text markup documents, which depend on the installed parsers
    * and need to be created separately.
    */
  val base: PartialFunction[Path, DocumentType] = { case path: Path =>
    (path.basename, path.suffix) match {
      case ("directory", Some("conf"))                   => Config
      case (base, Some(_)) if base.endsWith(".template") => Template
      case (base, Some("css")) if base.endsWith(".fo")   => StyleSheet("fo")
      case _                                             => Static(staticTargetFormats(path))
    }
  }

  /** Creates a document type matcher that recognizes all input files
    * with one of the specified file suffixes as a markup document.
    */
  def forMarkup(fileSuffixes: Set[String]): PartialFunction[Path, DocumentType] = {
    case path if path.suffix.exists(fileSuffixes.contains) => Markup
  }

}
