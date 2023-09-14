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

import cats.syntax.all._
import cats.{ Applicative, Functor }
import cats.effect.{ Async, Concurrent }
import fs2.io.file.Files
import laika.ast.Path.Root
import laika.ast.{ DocumentType, Navigatable, Path, TextDocumentType }
import laika.parse.markup.DocumentParser.DocumentInput

import java.io.InputStream
import scala.io.Codec
import scala.reflect.ClassTag

/** Character input for the various parsers of this library and its virtual path within the input tree.
  *
  * @param input      The character input
  * @param path The full virtual path of this input (does not represent the filesystem path in case of file I/O),
  *                   the point within the virtual tree of inputs (usually a `DocumentTree`)
  *                   this resource should be linked into.
  * @param docType    Indicates the type of the document, to distinguish between text markup, templates, configuration
  *                   and style sheets, which all have a different kind of parser
  * @param sourceFile The source file from the file system, empty if this does not represent a file system resource
  */
class TextInput[F[_]: Functor] private (
    val input: F[String],
    val path: Path,
    val docType: TextDocumentType,
    val sourceFile: Option[FilePath] = None
) extends Navigatable {

  private[laika] lazy val asDocumentInput: F[DocumentInput] = input.map(DocumentInput(path, _))
}

object TextInput {

  private def readAll[F[_]: Concurrent](input: fs2.Stream[F, Byte], codec: Codec): F[String] =
    input.through(fs2.text.decodeWithCharset(codec.charSet)).compile.string

  def fromString[F[_]: Applicative](
      input: String,
      mountPoint: Path = Root / "doc",
      docType: TextDocumentType = DocumentType.Markup
  ): TextInput[F] =
    new TextInput[F](Applicative[F].pure(input), mountPoint, docType)

  def fromBinaryStream[F[_]: Concurrent](
      stream: fs2.Stream[F, Byte],
      mountPoint: Path = Root / "doc",
      docType: TextDocumentType = DocumentType.Markup
  ): TextInput[F] =
    new TextInput(
      stream.through(fs2.text.utf8.decode).compile.string,
      mountPoint,
      docType
    )

  def fromTextStream[F[_]: Concurrent](
      stream: fs2.Stream[F, String],
      mountPoint: Path = Root / "doc",
      docType: TextDocumentType = DocumentType.Markup
  ): TextInput[F] =
    new TextInput(stream.compile.string, mountPoint, docType)

  def fromFile[F[_]: Async](
      file: FilePath,
      mountPoint: Path = Root / "doc",
      docType: TextDocumentType = DocumentType.Markup
  )(implicit codec: Codec): TextInput[F] = {

    val input = readAll(Files.forAsync[F].readAll(file.toFS2Path), codec)
    new TextInput[F](input, mountPoint, docType, Some(file))
  }

  def fromInputStream[F[_]: Async](
      stream: F[InputStream],
      mountPoint: Path = Root / "doc",
      docType: TextDocumentType = DocumentType.Markup,
      autoClose: Boolean = true
  )(implicit codec: Codec): TextInput[F] = {

    val input = readAll(fs2.io.readInputStream(stream, 64 * 1024, autoClose), codec)
    new TextInput[F](input, mountPoint, docType)
  }

  def fromClassResource[F[_]: Async, T: ClassTag](
      resource: String,
      mountPoint: Path = Root / "doc",
      docType: TextDocumentType = DocumentType.Markup
  )(implicit codec: Codec): TextInput[F] = {
    val input = readAll(fs2.io.readClassResource[F, T](resource), codec)
    new TextInput(input, mountPoint, docType)
  }

  def fromClassLoaderResource[F[_]: Async](
      resource: String,
      mountPoint: Path = Root / "doc",
      docType: TextDocumentType = DocumentType.Markup,
      classLoader: ClassLoader = getClass.getClassLoader
  )(implicit codec: Codec): TextInput[F] = {
    val input = readAll(fs2.io.readClassLoaderResource(resource, classLoader = classLoader), codec)
    new TextInput[F](input, mountPoint, docType)
  }

}
