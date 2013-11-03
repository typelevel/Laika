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

/** 
 *  @author Jens Halm
 */
trait OutputProvider {

  
  def name: String
  
  def newOutput (name: String): Output
  
  def newChild (name: String): OutputProvider
    
    
}


object OutputProvider {
  
  private class DirectoryOutputProvider (dir: File, codec: Codec) extends OutputProvider {
    
    val name = dir.getName
    
    def newOutput (name: String) = {
      val f = new File(dir, name)
      Output.toFile(f) // TODO - stream creation could be lazy
    }
    
    def newChild (name: String) = {
      val f = new File(dir, name)
      require(!f.exists || f.isDirectory, "File "+f.getAbsolutePath+" exists and is not a directoy")
      if (!f.exists && !f.mkdir()) throw new IllegalStateException("Unable to create directory "+f.getAbsolutePath)
      forRootDirectory(f)
    }
    
  }
  
  def forRootDirectory (root: File)(implicit codec: Codec): OutputProvider = {
    require(root.isDirectory, "File "+root.getAbsolutePath()+" is not a directoy")
    
    new DirectoryOutputProvider(root, codec)
  }
  
  case class OutputConfig (provider: OutputProvider, parallel: Boolean)

  class OutputConfigBuilder private[OutputProvider] (
      dir: File,
      codec: Codec,
      isParallel: Boolean = false) {
    
    def parallel = new OutputConfigBuilder(dir, codec, true)
    
    def build = OutputConfig(OutputProvider.forRootDirectory(dir)(codec), isParallel)
  }
  
  object Directory {
    def apply (name: String)(implicit codec: Codec) = new OutputConfigBuilder(new File(name), codec)
    def apply (file: File)(implicit codec: Codec) = new OutputConfigBuilder(file, codec)
  }
  
  object DefaultDirectory {
    def apply (implicit codec: Codec) = Directory(System.getProperty("user.dir"))(codec)
  }
  
}