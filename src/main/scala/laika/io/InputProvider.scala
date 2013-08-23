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
trait InputProvider {

  
  def name: String
  
  def inputs: Seq[Input]
  
  def children: Seq[InputProvider]
  
  
}


object InputProvider {
  
  private class DirectoryInputProvider (dir: File) extends InputProvider {
    
    val name = dir.getName
    
    lazy val inputs = {
      dir.listFiles filterNot (_.isDirectory) map (Input.fromFile(_)) toList // TODO - stream creation could be lazy
    }
    
    lazy val children = {
      dir.listFiles filter (_.isDirectory) map (forRootDirectory(_)) toList
    }
    
  }
  
  def forRootDirectory (root: File): InputProvider = {
    require(root.exists, "Directory "+root.getAbsolutePath()+" does not exist")
    require(root.isDirectory, "File "+root.getAbsolutePath()+" is not a directoy")
    
    new DirectoryInputProvider(root)
  }
  
}