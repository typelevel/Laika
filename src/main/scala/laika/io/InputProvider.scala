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
import laika.tree.Documents.Path
import laika.tree.Documents.Root

/** 
 *  @author Jens Halm
 */
trait InputProvider {

  
  lazy val name: String = path.name
  
  def path: Path
  
  def documents: Seq[Input]

  def templates: Seq[Input]
  
  def subtrees: Seq[InputProvider]
  
  
}


object InputProvider {
  
  private class DirectoryInputProvider (dir: File, val path: Path = Root) extends InputProvider {
    
    // TODO - improve file suffix management
    lazy val documents = {
      dir.listFiles filterNot (f => f.isDirectory || f.getName.endsWith(".template")) map (Input.fromFile(_, path)) toList // TODO - stream creation could be lazy
    }
    
    lazy val templates = {
      dir.listFiles filter (f => f.isFile || f.getName.endsWith(".template")) map (Input.fromFile(_, path)) toList
    }
    
    lazy val subtrees = {
      dir.listFiles filter (_.isDirectory) map (d => new DirectoryInputProvider(d, path / d.getName)) toList
    }
    
  }
  
  def forRootDirectory (root: File): InputProvider = {
    require(root.exists, "Directory "+root.getAbsolutePath()+" does not exist")
    require(root.isDirectory, "File "+root.getAbsolutePath()+" is not a directoy")
    
    new DirectoryInputProvider(root)
  }
  
}