/*
 * Copyright 2013 the original author or authors.
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
import laika.tree.Documents.Path
import laika.tree.Documents.Root

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
  
  private class DirectoryOutputProvider (dir: File, val path: Path, codec: Codec) extends OutputProvider {
    
    def newOutput (name: String) = {
      val f = new File(dir, name)
      Output.toFile(f)
    }
    
    def newChild (name: String) = {
      val f = new File(dir, name)
      require(!f.exists || f.isDirectory, "File "+f.getAbsolutePath+" exists and is not a directory")
      if (!f.exists && !f.mkdir()) throw new IllegalStateException("Unable to create directory "+f.getAbsolutePath)
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
    require(root.isDirectory, "File "+root.getAbsolutePath()+" is not a directoy")
    
    new DirectoryOutputProvider(root, Root, codec)
  }
  
  /** The configuration for an output tree, consisting of the actual provider for
   *  all outputs and a flag whether rendering and writing results should be performed
   *  in parallel.
   */
  case class OutputConfig (provider: OutputProvider, parallel: Boolean)
  
  /** Responsible for building new OutputProviders based
   *  on the specified codec.
   */
  trait ProviderBuilder {
    def build (codec: Codec): OutputProvider
  }
  
  private[OutputProvider] class DirectoryProviderBuilder (root: File) extends ProviderBuilder {
    def build (codec: Codec) = 
      OutputProvider.forRootDirectory(root)(codec)
  }

  /** API for configuring an output tree.
   *  Gives access to all relevant aspects of traversing and writing to
   *  a tree of outputs.
   */
  class OutputConfigBuilder (
      provider: ProviderBuilder,
      codec: Codec,
      isParallel: Boolean = false) {
    
    /** Instructs the renderers and writers to process all outputs in parallel.
     *  The recursive structure of inputs will be flattened before rendering,
     *  therefore the parallel processing includes all subtrees of this input tree.
     */
    def inParallel = new OutputConfigBuilder(provider, codec, true)
    
    /** Builds the final configuration for this output tree.
     */
    def build = OutputConfig(provider.build(codec), isParallel)
  }
  
  /** Creates OutputConfigBuilder instances for a specific root directory in the file system.
   */
  object Directory {
    def apply (name: String)(implicit codec: Codec) = new OutputConfigBuilder(new DirectoryProviderBuilder(new File(name)), codec)
    def apply (file: File)(implicit codec: Codec) = new OutputConfigBuilder(new DirectoryProviderBuilder(file), codec)
  }
  
  /** Creates OutputConfigBuilder instances using the current working directory as its root.
   */
  object DefaultDirectory {
    def apply (implicit codec: Codec) = Directory(System.getProperty("user.dir"))(codec)
  }
  
}
