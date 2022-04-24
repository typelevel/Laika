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
import cats.effect.{Resource, Sync}
import cats.implicits._

import scala.io.Codec

/** Internal runtime for creating and writing to OutputStreams.
  * 
  * @author Jens Halm
  */
object OutputRuntime {

  /** Creates a Writer for the specified output model and writes the given string to it.
    */
  def write[F[_]: Sync] (result: String, writer: Writer): F[Unit] =
    Sync[F].delay {
      writer.write(result)
      writer.flush()
    }

  /** Creates a directory for the specified file, including parent directories of that file if they do not exist yet.
    */
  def createDirectory[F[_]: Sync] (file: File): F[Unit] = 
    Sync[F].delay(file.exists || file.mkdirs()).flatMap(if (_) Sync[F].unit 
    else Sync[F].raiseError(new IOException(s"Unable to create directory ${file.getAbsolutePath}")))

  def textFileResource[F[_]: Sync] (file: File, codec: Codec): Resource[F, Writer] = Resource.fromAutoCloseable(Sync[F].delay {
    new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), codec.charSet))
  })

  def textStreamResource[F[_]: Sync] (stream: F[OutputStream], codec: Codec, autoClose: Boolean): Resource[F, Writer] = {
    val resource = if (autoClose) Resource.fromAutoCloseable(stream) else Resource.eval(stream)
    resource.map(out => new BufferedWriter(new OutputStreamWriter(out, codec.charSet)))
  }
  
  def binaryFileResource[F[_]: Sync] (file: File): Resource[F, OutputStream] = 
    Resource.fromAutoCloseable(Sync[F].delay(new BufferedOutputStream(new FileOutputStream(file))))

  def binaryStreamResource[F[_]: Sync] (stream: F[OutputStream], autoClose: Boolean): Resource[F, OutputStream] =
    if (autoClose) Resource.fromAutoCloseable(stream) else Resource.eval(stream)

}
