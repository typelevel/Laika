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

import cats.effect.{ Async, Sync }
import fs2.io.file.Files
import laika.ast.{ Navigatable, Path }
import laika.ast.Path.Root
import laika.rewrite.nav.TargetFormats

import java.io.InputStream
import scala.reflect.ClassTag

/** A binary input stream and its virtual path within the input tree.
  *
  * @param input      The binary input
  * @param path       The full virtual path of this input (does not represent the filesystem path in case of file I/O),
  *                   the point within the virtual tree of inputs (usually a `DocumentTree`)
  *                   this resource should be linked into.
  * @param formats    Indicates the output formats this binary input should be included in;
  *                   by default binary resources are included in all output formats, but it can be restricted
  *                   if necessary (e.g. to only include it in HTML output, but omit it from PDF or EPUB)
  * @param sourceFile The source file from the file system, empty if this does not represent a file system resource
  */
case class BinaryInput[F[_]: Sync](
    input: fs2.Stream[F, Byte],
    path: Path,
    formats: TargetFormats = TargetFormats.All,
    sourceFile: Option[FilePath] = None
) extends Navigatable

object BinaryInput {

  def fromString[F[_]: Sync](
      input: String,
      mountPoint: Path = Root / "doc",
      targetFormats: TargetFormats = TargetFormats.All
  ): BinaryInput[F] = {
    val stream = fs2.Stream.emit(input).through(fs2.text.utf8.encode)
    BinaryInput(stream, mountPoint, targetFormats)
  }

  def fromFile[F[_]: Async](
      file: FilePath,
      mountPoint: Path = Root / "doc",
      targetFormats: TargetFormats = TargetFormats.All
  ): BinaryInput[F] = {
    val stream = Files.forAsync[F].readAll(file.toFS2Path)
    BinaryInput(stream, mountPoint, targetFormats, Some(file))
  }

  def fromInputStream[F[_]: Sync](
      stream: F[InputStream],
      mountPoint: Path = Root / "doc",
      autoClose: Boolean = true,
      targetFormats: TargetFormats = TargetFormats.All
  ): BinaryInput[F] = {
    val input = fs2.io.readInputStream(stream, 64 * 1024, autoClose)
    BinaryInput(input, mountPoint, targetFormats)
  }

  def fromClassResource[F[_]: Async, T: ClassTag](
      resource: String,
      mountPoint: Path = Root / "doc",
      targetFormats: TargetFormats = TargetFormats.All
  ): BinaryInput[F] = {
    val stream = fs2.io.readClassResource[F, T](resource)
    BinaryInput(stream, mountPoint, targetFormats)
  }

  def fromClassLoaderResource[F[_]: Async](
      resource: String,
      mountPoint: Path = Root / "doc",
      targetFormats: TargetFormats = TargetFormats.All,
      classLoader: ClassLoader = getClass.getClassLoader
  ): BinaryInput[F] = {
    val stream = fs2.io.readClassLoaderResource(resource, classLoader = classLoader)
    BinaryInput(stream, mountPoint, targetFormats)
  }

}
