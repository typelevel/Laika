/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.io

import java.io.{File, InputStream, Reader}

import laika.config.OperationConfig
import laika.io.InputTree._

import scala.io.Codec

/** API for producing a result from processing various types of input.
  *
  * This is essentially a collection of shortcuts that allow any class
  * merging in this trait to define all input related operations in terms of the only
  * abstract method `fromInput`. Calling `fromFile("foo.md")` for example
  * is only a convenient shortcut for calling `fromInput(Input.fromFile("foo.md")`.
  *
  * @author Jens Halm
  */
trait InputOps {

  /** The type of the result returned by all operations of this trait.
    */
  type InputResult

  /**  Returns the result from parsing a the specified string.
    *  Any kind of input is valid, including an empty string.
    */
  def fromString (str: String): InputResult = fromInput(Input.fromString(str))

  /** Returns the result from parsing the input from the specified reader.
    */
  def fromReader (reader: Reader): InputResult = fromInput(Input.fromReader(reader))

  /** Returns the result from parsing the file with the specified name.
    *  Any kind of character input is valid, including empty files.
    *
    *  @param name the name of the file to parse
    *  @param codec the character encoding of the file, if not specified the platform default will be used.
    */
  def fromFile (name: String)(implicit codec: Codec): InputResult = fromInput(Input.fromFile(name)(codec))

  /** Returns the result from parsing the specified file.
    *  Any kind of character input is valid, including empty files.
    *
    *  @param file the file to use as input
    *  @param codec the character encoding of the file, if not specified the platform default will be used.
    */
  def fromFile (file: File)(implicit codec: Codec): InputResult = fromInput(Input.fromFile(file)(codec))

  /** Returns the result from parsing the input from the specified stream.
    *
    *  @param stream the stream to use as input for the parser
    *  @param codec the character encoding of the stream, if not specified the platform default will be used.
    */
  def fromStream (stream: InputStream)(implicit codec: Codec): InputResult = fromInput(Input.fromStream(stream)(codec))

  /** Returns the result from parsing the specified input.
    *
    *  This is a generic method based on Laika's IO abstraction layer that concrete
    *  methods delegate to. Usually not used directly in application code, but
    *  might come in handy for very special requirements.
    *
    *  @param input the input for the parser
    */
  def fromInput (input: Input): InputResult

}

/** API for producing a result from processing various types of input trees.
  *
  * This is essentially a collection of shortcuts that allow any class
  * merging in this trait to define all input related operations in terms of the only
  * abstract method `fromInputTree`. Calling `fromDirectory("src")` for example
  * is only a convenient shortcut for calling `fromInputTree(InputTree.fromDirectory("src")`.
  *
  * @author Jens Halm
  */
trait InputTreeOps {

  /** The type of the result returned by all operations of this trait.
    */
  type InputTreeResult

  /** The configuration to use for all input operations.
    */
  def config: OperationConfig

  /**  Returns the result obtained by parsing files from the
    *  specified directory and its subdirectories.
    *
    *  @param name the name of the directory to traverse
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectory (name: String)(implicit codec: Codec): InputTreeResult =
    fromDirectory(new File(name), hiddenFileFilter)(codec)

  /**  Returns the result obtained by parsing files from the
    *  specified directory and its subdirectories.
    *
    *  @param name the name of the directory to traverse
    *  @param exclude the files to exclude from processing
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectory (name: String, exclude: FileFilter)(implicit codec: Codec): InputTreeResult =
    fromDirectory(new File(name), exclude)(codec)

  /**  Returns the result obtained by parsing files from the
    *  specified directory and its subdirectories.
    *
    *  @param dir the root directory to traverse
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectory (dir: File)(implicit codec: Codec): InputTreeResult =
    fromDirectory(dir, hiddenFileFilter)(codec)

  /**  Returns the result obtained by parsing files from the
    *  specified directory and its subdirectories.
    *
    *  @param dir the root directory to traverse
    *  @param exclude the files to exclude from processing
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectory (dir: File, exclude: FileFilter)(implicit codec: Codec): InputTreeResult =
    fromDirectories(Seq(dir), exclude)(codec)

  /**  Returns the result obtained by parsing files from the
    *  specified directories and its subdirectories, merging them into
    *  a tree with a single root.
    *
    *  @param roots the root directories to traverse
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectories (roots: Seq[File])(implicit codec: Codec): InputTreeResult =
    fromDirectories(roots, hiddenFileFilter)(codec)

  /**  Returns the result obtained by parsing files from the
    *  specified directories and its subdirectories, merging them into
    *  a tree with a single root.
    *
    *  @param roots the root directories to traverse
    *  @param exclude the files to exclude from processing
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectories (roots: Seq[File], exclude: FileFilter)(implicit codec: Codec): InputTreeResult =
    fromInputTree(InputTree.forRootDirectories(roots, config.docTypeMatcher, exclude)(codec))

  /**  Returns the result obtained by parsing files from the
    *  current working directory.
    *
    *  @param exclude the files to exclude from processing
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDefaultDirectory (exclude: FileFilter = hiddenFileFilter)(implicit codec: Codec): InputTreeResult =
    fromInputTree(InputTree.forWorkingDirectory(config.docTypeMatcher, exclude)(codec))

  /**  Returns the result obtained by parsing files from the
    *  specified input tree builder.
    *
    *  @param builder a builder for the input tree to process
    */
  def fromInputTree(builder: InputTreeBuilder): InputTreeResult =
    fromInputTree(builder.build(config.docTypeMatcher))

  /** Returns the result obtained by parsing files from the
    *  specified input tree.
    *
    *  @param inputTree the input tree to process
    */
  def fromInputTree(inputTree: InputTree): InputTreeResult

}
