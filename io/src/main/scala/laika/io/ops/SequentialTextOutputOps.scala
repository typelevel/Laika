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

package laika.io.ops

import java.io.{File, OutputStream}

import cats.effect.Async
import laika.ast.Path
import laika.ast.Path.Root
import laika.io.model.TextOutput

import scala.io.Codec

/** API for specifying a single character output for a rendering operation.
  *
  * It allows any class merging in this trait to define all output related operations
  * in terms of the only abstract method `toOutput`.
  *
  * @author Jens Halm
  */
trait SequentialTextOutputOps[F[_]] {

  type Result

  def F: Async[F]

  /** Builder step that instructs the runtime to render the document to
    * the specified file.
    *
    * @param name the name of the file to write to
    * @param codec the character encoding of the file, if not specified the platform default will be used.
    */
  def toFile (name: String)(implicit codec: Codec): Result = toFile(new File(name))

  /** Builder step that instructs the runtime to render the document to
    * the specified file.
    *
    * @param file the file to write to
    * @param codec the character encoding of the file, if not specified the platform default will be used.
    */
  def toFile (file: File)(implicit codec: Codec): Result =
    toOutput(TextOutput.forFile(Path(file.getName), file, codec)(F))

  /** Builder step that instructs the runtime to render the document to
    * the specified stream.
    *
    * @param stream the stream to render to
    * @param autoClose whether the stream should be closed after all output had been written   
    * @param codec the character encoding of the stream, if not specified the platform default will be used.
    */
  def toStream (stream: F[OutputStream], autoClose: Boolean = true)(implicit codec: Codec): Result =
    toOutput(TextOutput.forStream(Root, stream, codec, autoClose)(F))

  /** Builder step that instructs the runtime to render the document to
    * an in-memory string.
    */
  def toRenderedString: Result = toOutput(TextOutput.forString(Root)(F))

  /** Renders the model to the specified output.
    *
    *  This is a generic method based on Laika's IO abstraction layer that concrete
    *  methods delegate to. Usually not used directly in application code, but
    *  might come in handy for very special requirements.
    */
  def toOutput (output: TextOutput[F]): Result

}
