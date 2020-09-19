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

import cats.effect.{Sync, Resource}
import laika.io.model._
import laika.parse.SourceCursor
import laika.parse.markup.DocumentParser.DocumentInput
import cats.implicits._

import scala.io.Codec

/** Internal runtime for creating and reading from InputStreams.
  * 
  * @author Jens Halm
  */
object InputRuntime {

  def readParserInput[F[_]: Sync: Runtime] (doc: TextInput[F]): F[DocumentInput] = doc.input.use {
    case PureReader(input) => 
      Sync[F].pure(DocumentInput(doc.path, SourceCursor(input)))
    case StreamReader(reader, sizeHint) => 
      readAll(reader, sizeHint).map(source => DocumentInput(doc.path, SourceCursor(source)))
  }

  def readAll[F[_]: Sync: Runtime] (reader: Reader, sizeHint: Int): F[String] = Runtime[F].runBlocking {
    
    def read(inBuffer: Array[Char], outBuffer: StringBuilder): F[Unit] = {
      for {
        amount <- Sync[F].delay(reader.read(inBuffer, 0, inBuffer.length))
        _      <- if (amount == -1) Sync[F].unit
                  else Sync[F].delay(outBuffer.appendAll(inBuffer, 0, amount)) >> read(inBuffer, outBuffer)
      } yield ()
    }
    
    for {
      inBuffer  <- Sync[F].delay(new Array[Char](Math.max(sizeHint, 8)))
      outBuffer = new StringBuilder
      _         <- read(inBuffer, outBuffer)
    } yield outBuffer.toString
  }

  def binaryFileResource[F[_]: Sync] (file: File): Resource[F, InputStream] =
    Resource.fromAutoCloseable(Sync[F].delay(new BufferedInputStream(new FileInputStream(file))))

  def textFileResource[F[_]: Sync] (file: File, codec: Codec): Resource[F, Reader] =
    readerResource(Resource.fromAutoCloseable(Sync[F].delay(new FileInputStream(file))), codec)
  
  def textStreamResource[F[_]: Sync] (inputStream: F[InputStream], codec: Codec, autoClose: Boolean): Resource[F, Reader] =
    readerResource(if (autoClose) Resource.fromAutoCloseable(inputStream) else Resource.liftF(inputStream), codec)

  private def readerResource[F[_]: Sync](resource: Resource[F, InputStream], codec: Codec): Resource[F, Reader] =
    resource.map(in => new BufferedReader(new InputStreamReader(in, codec.charSet)))
  
}
