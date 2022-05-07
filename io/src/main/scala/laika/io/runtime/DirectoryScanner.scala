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

package laika.io.runtime

import cats.effect.Sync
import cats.effect.kernel.Async
import cats.implicits._
import fs2.io.file.Files
import laika.ast.DocumentType.Static
import laika.ast.{Path, TextDocumentType}
import laika.io.model._

/** Scans a directory in the file system and transforms it into a generic InputCollection
  * that can serve as input for parallel parsers or transformers.
  * 
  * @author Jens Halm
  */
object DirectoryScanner {

  /** Scans the specified directory passing all child paths to the given function.
    */
  def scanDirectory[F[_]: Async, A] (directory: FilePath)(f: Seq[FilePath] => F[A]): F[A] = 
    fs2.io.file.Files[F]
      .list(directory.toFS2Path)
      .map(FilePath.fromFS2Path)
      .compile
      .toList
      .flatMap(f)
  
  /** Scans the specified directory and transforms it into a generic InputCollection.
    */
  def scanDirectories[F[_]: Async] (input: DirectoryInput): F[InputTree[F]] = {
    join(input.directories.map(d => scanDirectory[F, InputTree[F]](d)(asInputCollection(input.mountPoint, input))))
      .map(_.copy(sourcePaths = input.directories))
  }
  
  private def join[F[_]: Sync] (collections: Seq[F[InputTree[F]]]): F[InputTree[F]] = collections
    .toVector
    .sequence
    .map(_.reduceLeftOption(_ ++ _).getOrElse(InputTree.empty))

  private def asInputCollection[F[_]: Async] (path: Path, input: DirectoryInput)(entries: Seq[FilePath]): F[InputTree[F]] = {

    def toCollection (filePath: FilePath): F[InputTree[F]] = {

      val childPath = path / filePath.name

      input.fileFilter.filter(filePath).ifM(
        InputTree.empty[F].pure[F],
        Files[F].isDirectory(filePath.toFS2Path).ifM(
          scanDirectory(filePath)(asInputCollection(childPath, input)),
          input.docTypeMatcher(childPath) match {
            case docType: TextDocumentType => InputTree[F](Seq(TextInput.fromFile(childPath, docType, filePath, input.codec)), Nil, Nil).pure[F]
            case Static(formats)           => InputTree[F](Nil, Seq(BinaryInput.fromFile(childPath, filePath, formats)), Nil).pure[F]
            case _                         => InputTree.empty[F].pure[F]
          }
        )
      )
    }

    join(entries.map(toCollection))
  }
  
}
