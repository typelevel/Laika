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

package laika.render.epub

import java.util.zip.{ CRC32, ZipEntry, ZipOutputStream }
import cats.effect.{ Async, Sync }
import cats.effect.kernel.Concurrent
import cats.implicits._
import laika.io.model.{ BinaryInput, BinaryOutput }

import java.io.OutputStream

/** @author Jens Halm
  */
object ZipWriter {

  /** Writes an EPUB Zip file to the specified output.
    * The virtual path of the given inputs will also become the path within
    * the Zip container.
    * The implementation follows the EPUB specification in that the first
    * file (called `mimeType`) is written uncompressed. Hence this is not
    * a generic zip utility as the method name suggests.
    */
  def zipEPUB[F[_]: Async](inputs: Seq[BinaryInput[F]], output: BinaryOutput[F]): F[Unit] = {

    def copyAll(zipOut: ZipOutputStream, pipe: fs2.Pipe[F, Byte, Nothing]): F[Unit] = {

      def writeEntry(
          input: BinaryInput[F],
          prepareEntry: ZipEntry => F[Unit] = _ => Sync[F].unit
      ): F[Unit] = for {
        entry <- Sync[F].delay(new ZipEntry(input.path.relative.toString))
        _     <- prepareEntry(entry)
        _     <- Sync[F].blocking(zipOut.putNextEntry(entry))
        _     <- input.input.through(pipe).compile.drain
        _     <- Sync[F].blocking(zipOut.closeEntry())
      } yield ()

      def prepareUncompressedEntry(entry: ZipEntry): F[Unit] = Sync[F].delay {
        val content = StaticContent.mimeType
        val crc32   = new CRC32
        entry.setMethod(ZipOutputStream.STORED)
        entry.setSize(content.length)
        crc32.update(content.getBytes("UTF-8"))
        entry.setCrc(crc32.getValue)
      }

      writeEntry(inputs.head, prepareUncompressedEntry) >>
        inputs.toList.tail.traverse(writeEntry(_)) >>
        Sync[F].blocking(zipOut.close())
    }

    output.resource.map(new ZipOutputStream(_)).use { zipOut =>
      val outF: F[OutputStream] = Sync[F].pure(zipOut).widen
      val pipe                  = fs2.io.writeOutputStream(outF, closeAfterUse = false)
      copyAll(zipOut, pipe)
    }
  }

}
