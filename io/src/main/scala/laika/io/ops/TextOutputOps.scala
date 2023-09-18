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

import laika.io.internal.model
import laika.io.internal.model.{ InMemoryOutput, TreeOutput }
import laika.io.model.FilePath

import scala.io.Codec

/** API for specifying the tree of character outputs for a rendering operation.
  *
  * It allows any class merging in this trait to define all input related operations
  * in terms of the only abstract method `toOutput`.
  *
  * @author Jens Halm
  */
private[io] trait TextOutputOps[F[_]] {

  type Result

  /** Builder step that instructs the runtime to render the document tree to files
    * in the specified directory and its subdirectories.
    *
    * The virtual paths of the document tree will be translated to a directory structure,
    * with the root of the virtual path being the directory specified with this method.
    *
    * @param name the name of the directory to write to
    * @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def toDirectory(name: String)(implicit codec: Codec): Result = toDirectory(FilePath.parse(name))

  /** Builder step that instructs the runtime to render the document tree to files
    * in the specified directory and its subdirectories.
    *
    * The virtual paths of the document tree will be translated to a directory structure,
    * with the root of the virtual path being the directory specified with this method.
    *
    * @param dir the directory to write to
    * @param codec the character encoding of the files, if not specified the platform default will be used.
    */
  def toDirectory(dir: FilePath)(implicit codec: Codec): Result = toOutput(
    model.DirectoryOutput(dir, codec)
  )

  /** Builder step that instructs the renderer to only produce an in - memory representation of the
    * tree of rendered outputs.
    */
  def toMemory: Result = toOutput(InMemoryOutput)

  /** Builder step that instructs the runtime to render
    * to the specified tree output.
    *
    * This is a generic method based on Laika's IO model that concrete
    * methods delegate to.
    */
  private[io] def toOutput(tree: TreeOutput): Result

}
