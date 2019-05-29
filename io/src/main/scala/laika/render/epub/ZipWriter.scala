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

package laika.render.epub

import java.util.zip.{CRC32, ZipEntry, ZipOutputStream}

import laika.ast.Path
import laika.execute.OutputExecutor
import laika.io.BinaryOutput
import laika.io.IOX.copy

/**
  * @author Jens Halm
  */
object ZipWriter {

  /** Writes an EPUB Zip file to the specified output.
    * The virtual path of the given inputs will also become the path within
    * the Zip container.
    * The implementation follows the EPUB specification in that the first
    * file (called `mimeType`) is written uncompressed. Hence this is not
    * a generic zip utility as the method name suggests.
    */
  def zipEPUB (inputs: Seq[StreamInput], output: BinaryOutput): Unit = { // TODO - 0.12 - StreamInput is a temporary model

    val zip = new ZipOutputStream(OutputExecutor.asStream(output))

    def writeEntry (input: StreamInput, prepareEntry: ZipEntry => Unit = _ => ()): Unit = {

      val entry = new ZipEntry(input.path.relativeTo(Path.Root).toString)

      prepareEntry(entry)
      zip.putNextEntry(entry)

      copy(input.stream, zip)

      zip.closeEntry()
    }

    writeEntry(inputs.head, { entry =>
      entry.setMethod(ZipOutputStream.STORED)
      val content = StaticContent.mimeType
      entry.setSize(content.length)
      val crc32 = new CRC32
      crc32.update(content.getBytes("UTF-8"))
      entry.setCrc(crc32.getValue)
    })

    inputs.tail.foreach(writeEntry(_))

    zip.close()
  }
  
}
