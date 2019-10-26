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

import java.io.{File, InputStream}

import cats.effect.Async
import laika.ast.Path.Root
import laika.ast.{Path, TextDocumentType}
import laika.io.model.TextInput

import scala.io.Codec


/** API for specifying a single character input for a parsing operation.
  *
  * It allows any class merging in this trait to define all input related operations
  * in terms of the only abstract method `fromInput`.
  *
  * @author Jens Halm
  */
trait SequentialInputOps[F[_]] {

  /** The type of the result returned by all operations of this trait.
    */
  type InputResult


  def F: Async[F]

  /** The type of text document created by this instance.
    */
  def docType: TextDocumentType

  /** Builder step that instructs the runtime to use the
    * specified string as parser input.
    */
  def fromString (str: String): InputResult = fromInput(TextInput.fromString[F](Root, docType, str)(F))

  /** Builder step that instructs the runtime to use the specified file as parser input.
    *
    * @param name the name of the file to parse
    * @param codec the character encoding of the file, if not specified the platform default will be used.
    */
  def fromFile (name: String)(implicit codec: Codec): InputResult = fromFile(new File(name))

  /** Builder step that instructs the runtime to use the specified file as parser input.
    *
    * @param file the file to use as input
    * @param codec the character encoding of the file, if not specified the platform default will be used.
    */
  def fromFile (file: File)(implicit codec: Codec): InputResult =
    fromInput(TextInput.fromFile(Path(file.getName), docType, file, codec)(F))

  /** Builder step that instructs the runtime to use the specified character stream as parser input.
    *
    * @param stream the stream to use as input for the parser
    * @param autoClose whether the stream should be closed after reading all input              
    * @param codec the character encoding of the stream, if not specified the platform default will be used.
    */
  def fromStream (stream: F[InputStream], autoClose: Boolean = true)(implicit codec: Codec): InputResult =
    fromInput(TextInput.fromStream(Root, docType, stream, codec, autoClose)(F))

  /** Builder step that instructs the runtime to use the specified input for the
    * parsing operation.
    *
    * This is a generic method based on Laika's IO model that concrete
    * methods delegate to.
    *
    * @param input the input to parse
    */
  def fromInput (input: TextInput[F]): InputResult

}
