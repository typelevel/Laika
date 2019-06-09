/*
 * Copyright 2012-2019 the original author or authors.
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

package laika.runtime

import java.io.File
import java.nio.file.{DirectoryStream, Files, Path => JPath}

import cats.effect.{Async, Resource}
import cats.implicits._
import laika.ast.DocumentType.Static
import laika.ast.Path.Root
import laika.ast.{Path, TextDocumentType}
import laika.io._

import scala.collection.JavaConverters

/**
  * @author Jens Halm
  */
object DirectoryScanner {
  
  def scanDirectories[F[_]: Async] (input: DirectoryInput): F[InputCollection] = {
    val sourcePaths: Seq[String] = input.directories map (_.getAbsolutePath)
    join(input.directories.map(d => scanDirectory(d.toPath, input))).map(_.copy(sourcePaths = sourcePaths))
  }
  
  def scanDirectory[F[_]: Async] (path: JPath, input: DirectoryInput): F[InputCollection] =
    scanDirectory(Root, path, input)

  private def scanDirectory[F[_]: Async] (vPath: Path, filePath: JPath, input: DirectoryInput): F[InputCollection] =
    scannerResource(filePath).use(asInputCollection(vPath, input)(_))
  
  def scannerResource[F[_]: Async] (path: JPath): Resource[F, DirectoryStream[JPath]] =
    Resource.fromAutoCloseable(Async[F].delay(Files.newDirectoryStream(path)))

  case class Directory (path: Path, file: File)
  
  private def join[F[_]: Async] (collections: Seq[F[InputCollection]]): F[InputCollection] = collections
    .toVector
    .sequence
    .map(_.reduceLeftOption(_ ++ _).getOrElse(InputCollection.empty))

  def asInputCollection[F[_]: Async] (path: Path, input: DirectoryInput)(directoryStream: DirectoryStream[JPath]): F[InputCollection] = {

    def toCollection (filePath: JPath): F[InputCollection] = {
      
      val childPath = path / filePath.getFileName.toString

      if (input.fileFilter(filePath.toFile)) Async[F].pure(InputCollection.empty)
      else if (Files.isDirectory(filePath)) scanDirectory(childPath, filePath, input)
      else input.docTypeMatcher(childPath) match {
        case docType: TextDocumentType => Async[F].pure(InputCollection(TextFileInput(filePath.toFile, docType, childPath, input.codec)))
        case Static => Async[F].pure(InputCollection(Nil, Seq(BinaryFileInput(filePath.toFile, childPath))))
        case _ => Async[F].pure(InputCollection.empty)
      }
    }

    val collections = for {
      path <- JavaConverters.asScalaIterator(directoryStream.iterator).toSeq
    } yield toCollection(path)

    join(collections)
  }
  
}
