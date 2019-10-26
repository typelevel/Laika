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

package laika.io.runtime

import java.io._

import cats.effect.{Async, Resource}
import laika.io.model._
import laika.parse.ParserContext
import laika.parse.markup.DocumentParser.ParserInput
import cats.implicits._

import scala.io.Codec

/** Internal runtime for creating and reading from InputStreams.
  * 
  * @author Jens Halm
  */
object InputRuntime {

  def readParserInput[F[_]: Async: Runtime] (doc: TextInput[F]): F[ParserInput] = doc.input.use {
    case PureReader(input) => 
      Async[F].pure(ParserInput(doc.path, ParserContext(input)))
    case StreamReader(reader, sizeHint) => 
      readAll(reader, sizeHint).map(source => ParserInput(doc.path, ParserContext(source)))
  }

  private def readAll[F[_]: Async: Runtime] (reader: Reader, sizeHint: Int): F[String] = Runtime[F].runBlocking {
    
    def read(inBuffer: Array[Char], outBuffer: StringBuilder): F[Unit] = {
      for {
        amount <- Async[F].delay(reader.read(inBuffer, 0, inBuffer.length))
        _      <- if (amount == -1) Async[F].unit
                  else Async[F].delay(outBuffer.appendAll(inBuffer, 0, amount)) >> read(inBuffer, outBuffer)
      } yield ()
    }
    
    for {
      inBuffer  <- Async[F].delay(new Array[Char](Math.max(sizeHint, 8)))
      outBuffer = new StringBuilder
      _         <- read(inBuffer, outBuffer)
    } yield outBuffer.toString
  }

  def binaryFileResource[F[_]: Async] (file: File): Resource[F, InputStream] =
    Resource.fromAutoCloseable(Async[F].delay(new BufferedInputStream(new FileInputStream(file))))

  def textFileResource[F[_]: Async] (file: File, codec: Codec): Resource[F, Reader] =
    readerResource(Resource.fromAutoCloseable(Async[F].delay(new FileInputStream(file))), codec)
  
  def textStreamResource[F[_]: Async] (inputStream: F[InputStream], codec: Codec, autoClose: Boolean): Resource[F, Reader] =
    readerResource(if (autoClose) Resource.fromAutoCloseable(inputStream) else Resource.liftF(inputStream), codec)

  private def readerResource[F[_]: Async](resource: Resource[F, InputStream], codec: Codec): Resource[F, Reader] =
    resource.map(in => new BufferedReader(new InputStreamReader(in, codec.charSet)))
  
}
