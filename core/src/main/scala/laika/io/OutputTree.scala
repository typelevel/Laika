/*
 * Copyright 2013-2016 the original author or authors.
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

import java.io.File
import scala.io.Codec
import laika.tree.Paths.Path
import laika.tree.Paths.Root
import scala.collection.mutable.ListBuffer
import laika.io.Output.Binary
import java.io.Closeable

/** Represents a tree structure of Outputs, abstracting over various types of IO resources. 
 *  
 *  While the default implementations wrap the structure of directories in the file system,
 *  other implementations may build an entirely virtual output tree structure. 
 * 
 *  @author Jens Halm
 */
trait OutputTree {

  /** The local name of the output tree.
   */
  lazy val name: String = path.name

  /** The full path of the output tree.
   *  This path is always an absolute path
   *  from the root of the (virtual) output tree,
   *  therefore does not represent the filesystem
   *  path in case of file I/O.
   */
  def path: Path

  /** Creates a new output with the specified name
   *  on this level of the output hierarchy.
   */
  def newOutput (name: String): Output

  /** Creates a new subtree of outputs with
   *  the specified name.
   */
  def newChild (name: String): OutputTree

  /** Indicates whether static files found in the input tree
    * should be copied over to this output tree.
    */
  def acceptsStaticFiles: Boolean

}


/** Factory methods for creating `OutputTree` instances.
 */
object OutputTree {

  /** An output tree that writes to a directory in the file system.
   */
  class DirectoryOutputTree(val directory: File, val path: Path, codec: Codec) extends OutputTree {

    val acceptsStaticFiles = true

    def newOutput (name: String): Output with Binary with Closeable = {
      val f = new File(directory, name)
      Output.toFile(f, path)(codec)
    }

    def newChild (name: String): OutputTree = {
      val f = new File(directory, name)
      require(!f.exists || f.isDirectory, s"File ${f.getAbsolutePath} exists and is not a directory")
      if (!f.exists && !f.mkdir()) throw new IllegalStateException(s"Unable to create directory ${f.getAbsolutePath}")
      new DirectoryOutputTree(f, path / name, codec)
    }

  }

  /** Creates an OutputTree based on the specified directory, including
   *  all subdirectories.
   *
   *  @param root the root directory of the output tree
   *  @param codec the character encoding of the files, if not specified the platform default will be used
   */
  def forRootDirectory (root: File)(implicit codec: Codec): OutputTree = {
    require(root.isDirectory, s"File ${root.getAbsolutePath} is not a directory")

    new DirectoryOutputTree(root, Root, codec)
  }

  def forWorkingDirectory (implicit codec: Codec): OutputTree =
    forRootDirectory(new File(System.getProperty("user.dir")))

  /** Represent an in-memory result of a rendering operation.
   */
  trait RenderResult {
    def path: Path
  }

  /** The result of rendering a single document.
   */
  case class StringResult (path: Path, result: String) extends RenderResult

  /** The result of rendering a document tree.
   */
  case class ResultTree (path: Path, results: Seq[StringResult], subtrees: Seq[ResultTree]) extends RenderResult {

    private lazy val resultMap = results map (r => (r.path.name, r.result)) toMap
    private lazy val subtreeMap = subtrees map (t => (t.path.name, t)) toMap

    def result (name: String): Option[String] = resultMap.get(name)
    def subtree (name: String): Option[ResultTree] = subtreeMap.get(name)
  }


  /** An output tree that produces a tree of String results.
   */
  class StringOutputTree(val path: Path) extends OutputTree {

    val acceptsStaticFiles = false

    class ResultBuilder (path: Path, sb: StringBuilder) {
      def result = StringResult(path, sb.toString)
    }

    private val results = ListBuffer[ResultBuilder]()
    private val subtrees = ListBuffer[StringOutputTree]()

    def newOutput (name: String): Output = {
      val builder = new StringBuilder
      results += new ResultBuilder(path / name, builder)
      Output.toBuilder(builder, path / name)
    }

    def newChild (name: String): OutputTree = {
      val prov = new StringOutputTree(path / name)
      subtrees += prov
      prov
    }
    
    def result: ResultTree = ResultTree(path, results map (_.result), subtrees map (_.result))
    
  }

}
