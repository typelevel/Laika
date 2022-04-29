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

package laika.io.model

import cats.Applicative
import cats.effect.{Async, Concurrent, Resource, Sync}
import fs2.io.file.Files
import laika.ast._

import java.io.{File, OutputStream}
import scala.io.Codec

/** Character output for the various renderers of this library
  *
  * @param path       The full virtual path of this input (does not represent the filesystem path in case of file I/O)
  * @param writer     Handler for the character output (a function `String => F[Unit]`)
  * @param targetFile The target file in the file system, empty if this does not represent a file system resource
  */
case class TextOutput[F[_]] (path: Path, writer: TextOutput.Writer[F], targetFile: Option[File] = None)

object TextOutput {
  
  type Writer[F[_]] = String => F[Unit]

  private def writeAll[F[_]: Concurrent] (outPipe: fs2.Pipe[F, Byte, Nothing], codec: Codec): Writer[F] =
    output => fs2.Stream
      .emit(output)
      .through(fs2.text.encode(codec.charSet))
      .through(outPipe)
      .compile
      .drain
  
  def noOp[F[_]: Applicative] (path: Path): TextOutput[F] =
    TextOutput[F](path, _ => Applicative[F].unit)

  def forFile[F[_]: Async] (path: Path, file: File, codec: Codec): TextOutput[F] =
    TextOutput[F](path, writeAll(Files[F].writeAll(fs2.io.file.Path.fromNioPath(file.toPath)), codec), Some(file))
    
  def forStream[F[_]: Async] (path: Path, stream: F[OutputStream], codec: Codec, autoClose: Boolean): TextOutput[F] =
    TextOutput[F](path, writeAll(fs2.io.writeOutputStream(stream, autoClose), codec))
}

/** A resource for binary output.
  *
  * Most renderers write character data, but formats like PDF or EPUB
  * require a binary stream to write to.
  * 
  * This is the only I/O type not expressed through a generic `F[_]` or an `fs2.Stream` or `fs2.Pipe`
  * as Laika's binary output needs to work with Java libraries for its EPUB and PDF output.
  */
case class BinaryOutput[F[_]] (path: Path, resource: Resource[F, OutputStream], targetFile: Option[File] = None)

object BinaryOutput {

  def forFile[F[_]: Sync] (path: Path, file: File): BinaryOutput[F] = {
    val resource = Resource.fromAutoCloseable(Sync[F].blocking(
      new java.io.BufferedOutputStream(new java.io.FileOutputStream(file)))
    )
    BinaryOutput(path, resource)
  }

  def forStream[F[_]: Sync] (path: Path, stream: F[OutputStream], autoClose: Boolean): BinaryOutput[F] = {
    val resource = if (autoClose) Resource.fromAutoCloseable(stream) else Resource.eval(stream)
    BinaryOutput(path, resource)
  }

}

/** A (virtual) tree of output documents.
  */
sealed trait TreeOutput

/** A directory as a target for a rendering operation of a document tree.
  * 
  * The specified codec will be used for writing all character output.
  */
case class DirectoryOutput (directory: File, codec: Codec) extends TreeOutput

/** Instructs the renderer to produce an in-memory representation of the
  * tree of rendered outputs.
  */
case object StringTreeOutput extends TreeOutput
