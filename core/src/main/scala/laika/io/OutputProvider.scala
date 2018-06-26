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
trait OutputProvider {

  /** The local name of the tree represented by this provider.
   */
  lazy val name: String = path.name
  
  /** The full path of the tree represented by this provider.
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
  def newChild (name: String): OutputProvider
    
    
}


/** Factory methods for creating `OutputProvider` and `OutputConfigBuilder`
 *  instances. The latter offers additional configuration options like
 *  enabling parallel processing before creating the actual `InputProvider`.
 */
object OutputProvider {
  
  /** An output provider that writes to a directory in the file system.
   */
  class DirectoryOutputProvider (val directory: File, val path: Path, codec: Codec) extends OutputProvider {

    def newOutput (name: String): Output with Binary with Closeable = {
      val f = new File(directory, name)
      Output.toFile(f, path)(codec)
    }

    def newChild (name: String): OutputProvider = {
      val f = new File(directory, name)
      require(!f.exists || f.isDirectory, s"File ${f.getAbsolutePath} exists and is not a directory")
      if (!f.exists && !f.mkdir()) throw new IllegalStateException(s"Unable to create directory ${f.getAbsolutePath}")
      new DirectoryOutputProvider(f, path / name, codec)
    }
    
  }
  
  /** Creates an OutputProvider based on the specified directory, including
   *  all subdirectories.
   * 
   *  @param root the root directory of the output tree
   *  @param codec the character encoding of the files, if not specified the platform default will be used
   */
  def forRootDirectory (root: File)(implicit codec: Codec): OutputProvider = {
    require(root.isDirectory, s"File ${root.getAbsolutePath} is not a directory")
    
    new DirectoryOutputProvider(root, Root, codec)
  }
  
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
  

  /** An output provider that produces a tree of String results.
   */
  class StringOutputProvider (val path: Path) extends OutputProvider {
    
    class ResultBuilder (path: Path, sb: StringBuilder) {
      def result = StringResult(path, sb.toString)
    }
    
    private val results = ListBuffer[ResultBuilder]()
    private val subtrees = ListBuffer[StringOutputProvider]()
    
    def newOutput (name: String): Output = {
      val builder = new StringBuilder
      results += new ResultBuilder(path / name, builder)
      Output.toBuilder(builder, path / name)
    }
    
    def newChild (name: String): OutputProvider = {
      val prov = new StringOutputProvider(path / name)
      subtrees += prov
      prov
    }
    
    def result: ResultTree = ResultTree(path, results map (_.result), subtrees map (_.result))
    
  }
  
  /** The configuration for an output tree, consisting of the actual provider for
   *  all outputs and some flags that control the behaviour of the renderer.
   */
  case class OutputConfig (provider: OutputProvider, copyStaticFiles: Boolean)
  
  /** Responsible for building new OutputProviders based
   *  on the specified codec.
   */
  trait ProviderBuilder {
    def build (codec: Codec): OutputProvider
  }
  
  private[OutputProvider] class DirectoryProviderBuilder (root: File) extends ProviderBuilder {
    def build (codec: Codec): OutputProvider = 
      OutputProvider.forRootDirectory(root)(codec)
  }

  /** API for configuring an output tree.
   *  Gives access to all relevant aspects of traversing and writing to
   *  a tree of outputs.
   */
  class OutputConfigBuilder (provider: ProviderBuilder, codec: Codec) {
    
    /** Builds the final configuration for this output tree.
     */
    def build: OutputConfig = OutputConfig(provider.build(codec), copyStaticFiles = true)
  }
  
  /** Creates OutputConfigBuilder instances for a specific root directory in the file system.
   */
  object Directory {
    def apply (name: String)(implicit codec: Codec): OutputConfigBuilder = new OutputConfigBuilder(new DirectoryProviderBuilder(new File(name)), codec)
    def apply (file: File)(implicit codec: Codec): OutputConfigBuilder = new OutputConfigBuilder(new DirectoryProviderBuilder(file), codec)
  }
  
  /** Creates OutputConfigBuilder instances using the current working directory as its root.
   */
  object DefaultDirectory {
    def apply (implicit codec: Codec): OutputConfigBuilder = Directory(System.getProperty("user.dir"))(codec)
  }
  
}
