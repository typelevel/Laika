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

package laika.io.ops

import java.io.{File, OutputStream}

import cats.effect.Sync
import laika.ast.Path
import laika.ast.Path.Root
import laika.io.model.BinaryOutput
import laika.io.runtime.OutputRuntime

/** API for specifying the output for a binary format like EPUB or PDF.
  *
  * It allows any class merging in this trait to define all output related operations
  * in terms of the only abstract method `toOutput`.
  *
  * @author Jens Halm
  */
trait BinaryOutputOps[F[_]] {

  type Result

  def F: Sync[F]

  /** Builder step that instructs the runtime to render
    * to the file with the specified name.
    *
    *  @param name the name of the file to write to
    */
  def toFile (name: String): Result = toFile(new File(name))

  /** Builder step that instructs the runtime to render
    * to the specified file.
    *
    *  @param file the file to write to
    */
  def toFile (file: File): Result =
    toOutput(BinaryOutput(Root / file.getName, OutputRuntime.binaryFileResource(file)(F), Some(file)))

  /** Builder step that instructs the runtime to render
    * to the specified output stream.
    * 
    * @param stream the binary stream to render to
    * @param autoClose indicates whether the stream should be closed after all output had been written                 
    */
  def toStream (stream: F[OutputStream], autoClose: Boolean = true): Result =
    toOutput(BinaryOutput(Root, OutputRuntime.binaryStreamResource(stream, autoClose)(F)))

  /** Builder step that instructs the runtime to render
    * to the specified output.
    *
    * This is a generic method based on Laika's IO model that concrete
    * methods delegate to.
    */
  def toOutput (output: BinaryOutput[F]): Result

}
