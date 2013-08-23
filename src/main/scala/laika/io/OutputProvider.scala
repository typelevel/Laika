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

/** 
 *  @author Jens Halm
 */
trait OutputProvider {

  
  def name: String
  
  def newOutput (name: String): Output
  
  def newChild (name: String): OutputProvider
    
    
}


object OutputProvider {
  
  private class DirectoryOutputProvider (dir: File) extends OutputProvider {
    
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
  
  def forRootDirectory (root: File): OutputProvider = {
    require(root.isDirectory, "File "+root.getAbsolutePath()+" is not a directoy")
    
    new DirectoryOutputProvider(root)
  }
  
}