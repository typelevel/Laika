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

import java.nio.file.{Files, Path => JPath}

import cats.effect.{Sync, Resource}
import cats.implicits._
import laika.ast.DocumentType.Static
import laika.ast.{Path, TextDocumentType}
import laika.io.model._

import scala.collection.{AbstractIterator, Iterator}

/** Scans a directory in the file system and transforms it into a generic InputCollection
  * that can serve as input for parallel parsers or transformers.
  * 
  * @author Jens Halm
  */
object DirectoryScanner {

  /** Scans the specified directory passing all child paths to the given function.
    */
  def scanDirectory[F[_]: Sync, A] (directory: JPath)(f: Seq[JPath] => F[A]): F[A] =
    Resource
      .fromAutoCloseable(Sync[F].delay(Files.newDirectoryStream(directory)))
      .use(str => f(JIteratorWrapper(str.iterator).toSeq))
  
  /** Scans the specified directory and transforms it into a generic InputCollection.
    */
  def scanDirectories[F[_]: Sync] (input: DirectoryInput): F[InputTree[F]] = {
    val sourcePaths: Seq[String] = input.directories map (_.getAbsolutePath)
    join(input.directories.map(d => scanDirectory[F, InputTree[F]](d.toPath)(asInputCollection(input.mountPoint, input))))
      .map(_.copy(sourcePaths = sourcePaths))
  }
  
  private def join[F[_]: Sync] (collections: Seq[F[InputTree[F]]]): F[InputTree[F]] = collections
    .toVector
    .sequence
    .map(_.reduceLeftOption(_ ++ _).getOrElse(InputTree.empty))

  private def asInputCollection[F[_]: Sync] (path: Path, input: DirectoryInput)(entries: Seq[JPath]): F[InputTree[F]] = {

    def toCollection (filePath: JPath): F[InputTree[F]] = {

      val childPath = path / filePath.getFileName.toString

      if (input.fileFilter(filePath.toFile)) InputTree.empty[F].pure[F]
      else if (Files.isDirectory(filePath)) scanDirectory(filePath)(asInputCollection(childPath, input))
      else input.docTypeMatcher(childPath) match {
        case docType: TextDocumentType => InputTree[F](Seq(TextInput.fromFile(childPath, docType, filePath.toFile, input.codec)), Nil, Nil).pure[F]
        case Static(formats)           => InputTree[F](Nil, Seq(BinaryInput.fromFile(childPath, filePath.toFile, formats)), Nil).pure[F]
        case _                         => InputTree.empty[F].pure[F]
      }
    }

    join(entries.map(toCollection))
  }
  
  // copied from SDK source to avoid having either a dependency to scala-compat or a warning with 2.13
  // this is literally the one place in the Laika source where we need to deal with a Java collection
  case class JIteratorWrapper[A](underlying: java.util.Iterator[A]) extends AbstractIterator[A] with Iterator[A] {
    def hasNext: Boolean = underlying.hasNext
    def next(): A = underlying.next
  }
  
}
