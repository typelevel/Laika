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

import java.io._

import cats.effect.{Async, Resource}
import cats.implicits._
import laika.io.model.{PureWriter, _}

import scala.io.Codec

/** Internal runtime for creating and writing to OutputStreams.
  * 
  * @author Jens Halm
  */
object OutputRuntime {

  /** Creates a Writer for the specified output model and writes the given string to it.
    */
  def write[F[_]: Async: Runtime] (result: String, output: TextOutput[F]): F[Unit] = {
    output.resource.use {
      case PureWriter => Async[F].unit
      case StreamWriter(writer) => Async[F].delay {
        writer.write(result)
        writer.flush()
      }
    }
  }

  /** Creates a directory for the specified file, including parent directories
    * of that file if they do not exist yet.
    */
  def createDirectory[F[_]: Async] (file: File): F[Unit] = 
    Async[F].delay(file.exists || file.mkdirs()).flatMap(if (_) Async[F].unit 
    else Async[F].raiseError(new IOException(s"Unable to create directory ${file.getAbsolutePath}")))

  def textFileResource[F[_]: Async] (file: File, codec: Codec): Resource[F, Writer] = Resource.fromAutoCloseable(Async[F].delay {
    new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), codec.charSet))
  })

  def textStreamResource[F[_]: Async] (stream: F[OutputStream], codec: Codec, autoClose: Boolean): Resource[F, Writer] = {
    val resource = if (autoClose) Resource.fromAutoCloseable(stream) else Resource.liftF(stream)
    resource.map(out => new BufferedWriter(new OutputStreamWriter(out, codec.charSet)))
  }
  
  def binaryFileResource[F[_]: Async] (file: File): Resource[F, OutputStream] = 
    Resource.fromAutoCloseable(Async[F].delay(new BufferedOutputStream(new FileOutputStream(file))))

  def binaryStreamResource[F[_]: Async] (stream: F[OutputStream], autoClose: Boolean): Resource[F, OutputStream] =
    if (autoClose) Resource.fromAutoCloseable(stream) else Resource.liftF(stream)

}
