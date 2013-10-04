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
import laika.tree.Documents._

/** 
 *  @author Jens Halm
 */
trait InputProvider {

  
  lazy val name: String = path.name
  
  def path: Path
  
  def markupDocuments: Seq[Input]

  def dynamicDocuments: Seq[Input]

  def staticDocuments: Seq[Input]

  def templates: Seq[Input]
  
  def subtrees: Seq[InputProvider]
  
  
}


object InputProvider {
  
  private class DirectoryInputProvider (dir: File, val path: Path, docTypeMatcher: Path => DocumentType) extends InputProvider {
    
    private def docType (f: File) = docTypeMatcher(path / f.getName)

    private def toInput (pairs: Array[(DocumentType,File)]) = pairs.map(p => Input.fromFile(p._2, path)).toList

    private lazy val files = dir.listFiles filter (_.isFile) map (f => (docType(f), f)) groupBy (_._1)
    
    private def documents (docType: DocumentType) = files.get(docType).map(toInput).getOrElse(Nil)
    
    // TODO - stream creation could be lazy
    
    lazy val markupDocuments = documents(Markup)
    
    lazy val dynamicDocuments = documents(Dynamic)
    
    lazy val staticDocuments = documents(Static)
    
    lazy val templates =  documents(Template)
    
    lazy val subtrees = {
      dir.listFiles filter (f => f.isDirectory && docType(f) != Ignored) map (d => new DirectoryInputProvider(d, path / d.getName, docTypeMatcher)) toList
    }
    
  }
  
  def forRootDirectory (root: File, docTypeMatcher: Path => DocumentType): InputProvider = {
    require(root.exists, "Directory "+root.getAbsolutePath()+" does not exist")
    require(root.isDirectory, "File "+root.getAbsolutePath()+" is not a directoy")
    
    new DirectoryInputProvider(root, Root, docTypeMatcher)
  }
  
}