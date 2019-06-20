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
import laika.io.model.{CharStreamInput, StringInput, TextFileInput, TextInput}

import scala.io.Codec


/** API for producing a result from processing various types of input.
  *
  * This is essentially a collection of shortcuts that allow any class
  * merging in this trait to define all input related operations in terms of the only
  * abstract method `fromInput`.
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

  /**  Returns the result from parsing the specified string.
    *  Any kind of input is valid, including an empty string.
    */
  def fromString (str: String): InputResult = fromInput(F.pure(StringInput(str, docType)))

  /** Returns the result from parsing the file with the specified name.
    *  Any kind of character input is valid, including empty files.
    *
    *  @param name the name of the file to parse
    *  @param codec the character encoding of the file, if not specified the platform default will be used.
    */
  def fromFile (name: String)(implicit codec: Codec): InputResult = fromFile(new File(name))

  /** Returns the result from parsing the specified file.
    *  Any kind of character input is valid, including empty files.
    *
    *  @param file the file to use as input
    *  @param codec the character encoding of the file, if not specified the platform default will be used.
    */
  def fromFile (file: File)(implicit codec: Codec): InputResult =
    fromInput(F.pure(TextFileInput(file, docType, Path(file.getName), codec)))

  /** Returns the result from parsing the input from the specified stream.
    *
    *  @param stream the stream to use as input for the parser
    *  @param autoClose whether the stream should be closed after reading all input              
    *  @param codec the character encoding of the stream, if not specified the platform default will be used.
    */
  def fromStream (stream: F[InputStream], autoClose: Boolean = true)(implicit codec: Codec): InputResult =
    fromInput(F.map(stream)(CharStreamInput(_, docType, Root, autoClose, codec)))

  /** Returns the result from parsing the specified input.
    *
    *  This is a generic method based on Laika's IO abstraction layer that concrete
    *  methods delegate to. Usually not used directly in application code, but
    *  might come in handy for very special requirements.
    *
    *  @param input the input for the parser
    */
  def fromInput (input: F[TextInput]): InputResult

}
