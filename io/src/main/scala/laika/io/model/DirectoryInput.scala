/*
 * Copyright 2012-2022 the original author or authors.
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

package laika.io.model

import cats.effect.Async
import fs2.io.file.Files
import laika.ast.{ DocumentType, Path }
import laika.ast.Path.Root
import laika.bundle.DocumentTypeMatcher

import scala.io.Codec

/** A directory in the file system containing input documents for a tree transformation.
  *
  * The specified `docTypeMatcher` is responsible for determining the type of input
  * (e.g. text markup, template, etc.) based on the (virtual) document path.
  */
case class DirectoryInput(
    directories: Seq[FilePath],
    codec: Codec,
    docTypeMatcher: Path => DocumentType = DocumentTypeMatcher.base,
    fileFilter: FileFilter = DirectoryInput.hiddenFileFilter,
    mountPoint: Path = Root
)

object DirectoryInput {

  /** A filter that selects files that are hidden in a platform-dependent way.
    */
  val hiddenFileFilter: FileFilter = new FileFilter {
    def filter[F[_]: Async](file: FilePath) = Files.forAsync[F].isHidden(file.toFS2Path)
  }

  /** Creates a new instance using the library's defaults for the `docTypeMatcher` and
    * `fileFilter` properties.
    */
  def apply(directory: FilePath)(implicit codec: Codec): DirectoryInput =
    DirectoryInput(Seq(directory), codec)

  /** Creates a file filter that filters all files in the specified directory.
    */
  def filterDirectory(dir: FilePath): FileFilter = FileFilter.lift { file =>
    file.toString.startsWith(dir.toString)
  }

}
