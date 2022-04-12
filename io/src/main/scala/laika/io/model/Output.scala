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

import java.io._

import cats.Applicative
import cats.effect.{Sync, Resource}
import laika.ast._
import laika.config.Config
import laika.io.runtime.OutputRuntime

import scala.io.Codec
import cats.effect.kernel.Async
import fs2.io.file
import cats.data.Kleisli

sealed trait OutputWriter extends Product with Serializable
case object PureWriter extends OutputWriter
case class SinkWriter[F[_]:Async](sink: fs2.Pipe[F,String,Unit]) extends OutputWriter
case class StreamWriter(output: Writer) extends OutputWriter

/** Character output for the various renderers of this library
  *
  * @param path    The full virtual path of this input (does not represent the filesystem path in case of file I/O)
  * @param resource   The resource to write the character output to
  * @param targetFile The target file in the file system, empty if this does not represent a file system resource
  */
case class TextOutput[F[_]] (path: Path, resource: Resource[F, OutputWriter], targetFile: Option[File] = None)
/** Character output for the various renderers of this library
  *
  * @param path    The full virtual path of this input (does not represent the filesystem path in case of file I/O)
  * @param sink   The sink to write the character output to
  * @param targetFile The target file in the file system, empty if this does not represent a file system resource
  */
case class TextOutput2[F[_]] (path: Path, sink: fs2.Pipe[F, String,fs2.INothing], targetFile: Option[File] = None)

object TextOutput2{
  def forFile[F[_]:Async](path:Path,file:File) :TextOutput2[F]= {
    val sink:fs2.Pipe[F,String,fs2.INothing] =  _.through(fs2.text.utf8.encode).through(fs2.io.file.Files[F].writeAll(fs2.io.file.Path.fromNioPath(file.toPath()))) 
    TextOutput2(path,sink)
  }
  def forOutputStream[F[_]:Sync](path:Path,stream:F[OutputStream],codec:Codec,autoClose:Boolean):TextOutput2[F] = {
    val sink:fs2.Pipe[F,String,fs2.INothing] = _.through(fs2.text.utf8.encode).through(fs2.io.writeOutputStream(stream,autoClose))
    TextOutput2(path,sink)
  }
}

object TextOutput {
  def forString[F[_]: Applicative] (path: Path): TextOutput[F] =
    TextOutput[F](path, Resource.pure[F, OutputWriter](PureWriter))
  def forFile[F[_]: Sync] (path: Path, file: File, codec: Codec): TextOutput[F] =
    TextOutput[F](path, OutputRuntime.textFileResource(file, codec).map(StreamWriter.apply), Some(file))
  def forStream[F[_]: Sync] (path: Path, stream: F[OutputStream], codec: Codec, autoClose: Boolean): TextOutput[F] =
    TextOutput[F](path, OutputRuntime.textStreamResource(stream, codec, autoClose).map(StreamWriter.apply))
  def forSink[F[_]:Async](path: Path,sink:fs2.Pipe[F,String,Unit]) :TextOutput[F] = {
    TextOutput[F](path,Resource.pure(SinkWriter(sink)))
  }
  def forFile[F[_]:Async](path: Path,dest:file.Path,codec:fs2.Pipe[F,String,Byte] = fs2.text.utf8.encode[F]) :TextOutput[F] = {
    val p:fs2.Pipe[F,String,Unit] = _.through(codec).through(file.Files[F].writeAll(dest))
    forSink(path,p)
  }
}


object BinaryOutput2 {
  def forFile[F[_]:Async](path:Path,file:File):BinaryOutput2[F] = {
    val sink = fs2.io.file.Files[F].writeAll(fs2.io.file.Path.fromNioPath(file.toPath()))
    BinaryOutput2(path,sink)
  }
}

/** A resource for binary output.
  *
  * Most renderers write character data, but formats like PDF or EPUB
  * require a binary stream to write to.
  */
case class BinaryOutput[F[_]] (path: Path, resource: Resource[F, OutputStream], targetFile: Option[File] = None)
case class BinaryOutput2[F[_]] (path: Path, sink: fs2.Pipe[F,Byte,Unit], targetFile: Option[File] = None)

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
