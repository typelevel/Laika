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

import cats.effect.Async
import laika.api.builder.OperationConfig
import laika.io.model.{DirectoryInput, FileFilter, FilePath, InputTree, InputTreeBuilder}

import java.io.File
import scala.io.Codec

/** API for specifying the tree of character inputs for a parsing operation.
  *
  * It allows any class merging in this trait to define all input related operations
  * in terms of the only abstract method `fromInput`.
  *
  * @author Jens Halm
  */
trait InputOps[F[_]] {

  def F: Async[F]

  /** The type of the result returned by all operations of this trait.
    */
  type Result

  /** The configuration to use for all input operations.
    */
  def config: OperationConfig

  /**  Builder step that instructs the runtime to parse files from the
    *  specified directory and its subdirectories.
    *
    *  @param name the name of the directory to traverse
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectory (name: String)(implicit codec: Codec): Result =
    fromDirectory(FilePath.parse(name), DirectoryInput.hiddenFileFilter)(codec)

  /**  Builder step that instructs the runtime to parse files from the
    *  specified directory and its subdirectories.
    *
    *  @param name the name of the directory to traverse
    *  @param exclude the files to exclude from processing
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectory (name: String, exclude: FileFilter)(implicit codec: Codec): Result =
    fromDirectory(FilePath.parse(name), exclude)(codec)

  /**  Builder step that instructs the runtime to parse files from the
    *  specified directory and its subdirectories.
    *
    *  @param dir the root directory to traverse
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectory (dir: FilePath)(implicit codec: Codec): Result =
    fromDirectory(dir, DirectoryInput.hiddenFileFilter)(codec)

  @deprecated("use fromDirectory(String) or fromDirectory(FilePath)", "0.19.0")
  def fromDirectory (dir: File)(implicit codec: Codec): Result =
    fromDirectory(FilePath.fromJavaFile(dir), DirectoryInput.hiddenFileFilter)(codec)

  /**  Builder step that instructs the runtime to parse files from the
    *  specified directory and its subdirectories.
    *
    *  @param dir the root directory to traverse
    *  @param exclude the files to exclude from processing
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectory (dir: FilePath, exclude: FileFilter)(implicit codec: Codec): Result =
    fromDirectories(Seq(dir), exclude)(codec)

  @deprecated("use fromDirectory(String, FileFilter) or fromDirectory(FilePath, FileFilter)", "0.19.0")
  def fromDirectory (dir: File, exclude: FileFilter)(implicit codec: Codec): Result =
    fromDirectories(Seq(FilePath.fromJavaFile(dir)), exclude)(codec)

  /**  Builder step that instructs the runtime to parse files from the
    *  specified directories and its subdirectories, merging them into
    *  a tree with a single root.
    *
    *  @param roots the root directories to traverse
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectories (roots: Seq[FilePath])(implicit codec: Codec): Result =
    fromDirectories(roots, DirectoryInput.hiddenFileFilter)(codec)

  /**  Builder step that instructs the runtime to parse files from the
    *  specified directories and its subdirectories, merging them into
    *  a tree with a single root.
    *
    *  @param roots the root directories to traverse
    *  @param exclude the files to exclude from processing
    *  @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def fromDirectories (roots: Seq[FilePath], exclude: FileFilter)(implicit codec: Codec): Result =
    fromInput(InputTree[F](exclude)(F).addDirectories(roots))

  @deprecated("use fromDirectory(String) or fromDirectory(FilePath) using a relative path", "0.19.0")
  def fromWorkingDirectory (exclude: FileFilter = DirectoryInput.hiddenFileFilter)(implicit codec: Codec): Result =
    fromDirectories(Seq(FilePath.parse(System.getProperty("user.dir"))), exclude)

  /** Builder step that instructs the runtime to use the specified input builder for all parsing operations.
    * 
    * This is the most generic way to specify the input as it allows to freely compose inputs from multiple
    * directories, files, streams, the classpath or in-memory inputs.
    * All other methods in this trait are mere shortcuts that delegate to this method.
    *
    *  @param input the input tree to process
    */
  def fromInput(input: InputTreeBuilder[F]): Result
  
}
