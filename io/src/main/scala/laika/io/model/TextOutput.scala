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

import cats.Applicative
import cats.effect.{ Async, Concurrent }
import fs2.io.file.Files
import laika.ast.Path.Root
import laika.ast._

import java.io.OutputStream
import scala.io.Codec

/** Character output for the various renderers of this library
  *
  * @param writer     Handler for the character output (a function `String => F[Unit]`)
  * @param path       The full virtual path of this output (does not represent the filesystem path in case of file I/O)
  * @param targetFile The target file in the file system, empty if this does not represent a file system resource
  */
case class TextOutput[F[_]](
    writer: TextOutput.Writer[F],
    path: Path,
    targetFile: Option[FilePath] = None
)

object TextOutput {

  type Writer[F[_]] = String => F[Unit]

  private def writeAll[F[_]: Concurrent](
      outPipe: fs2.Pipe[F, Byte, Nothing],
      codec: Codec
  ): Writer[F] =
    output =>
      fs2.Stream
        .emit(output)
        .through(fs2.text.encode(codec.charSet))
        .through(outPipe)
        .compile
        .drain

  def noOp[F[_]: Applicative](path: Path = Root / "doc"): TextOutput[F] =
    TextOutput[F](_ => Applicative[F].unit, path)

  def forFile[F[_]: Async](file: FilePath, path: Path = Root / "doc")(implicit
      codec: Codec
  ): TextOutput[F] =
    TextOutput[F](writeAll(Files[F].writeAll(file.toFS2Path), codec), path, Some(file))

  def forStream[F[_]: Async](
      stream: F[OutputStream],
      path: Path = Root / "doc",
      autoClose: Boolean = true
  )(implicit codec: Codec): TextOutput[F] =
    TextOutput[F](writeAll(fs2.io.writeOutputStream(stream, autoClose), codec), path)

}
