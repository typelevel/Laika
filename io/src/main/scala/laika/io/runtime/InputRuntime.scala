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
import laika.ast.Path
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

  /** Creates a Reader for the specified input model, reads it into a string and returns
    * it as an instance of `ParserInput` that can serve as input for Laika's parsers.
    */
  def readParserInput[F[_]: Async: Runtime] (input: TextInput): F[ParserInput] = input match {
      
    case StringInput(source, _, path) => 
      Async[F].pure(ParserInput(path, ParserContext(source)))
      
    case TextFileInput(file, _, path, codec) =>
      readParserInput(fileInput(file), path, codec, file.length.toInt)
      
    case CharStreamInput(stream, _, path, autoClose, codec) =>
      val streamF = Async[F].pure(stream)
      val resource = if (autoClose) Resource.fromAutoCloseable(streamF) else Resource.liftF(streamF)
      readParserInput(resource, path, codec, 8096)
  }

  private def fileInput[F[_]: Async] (file: File): Resource[F, InputStream] = 
    Resource.fromAutoCloseable(Async[F].delay(new FileInputStream(file)))
  
  private def readParserInput[F[_]: Async: Runtime] (resource: Resource[F, InputStream], path: Path, codec: Codec, sizeHint: Int): F[ParserInput] =
    resource
      .map(in => new BufferedReader(new InputStreamReader(in, codec.charSet)))
      .use { reader =>
        readAll(reader, 8096)
          .map(source => ParserInput(path, ParserContext(source)))
      }

  /** Reads all input from the specified reader.
    */
  def readAll[F[_]: Async: Runtime] (reader: Reader, sizeHint: Int): F[String] = Runtime[F].runBlocking {
    
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

  def binaryInput[F[_]: Async] (file: File): Resource[F, InputStream] =
    Resource.fromAutoCloseable(Async[F].delay(new BufferedInputStream(new FileInputStream(file))))
  
}
