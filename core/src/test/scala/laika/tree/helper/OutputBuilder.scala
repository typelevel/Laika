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

package laika.tree.helper

import scala.io.Codec
import laika.io.OutputProvider
import laika.tree.Documents.Path
import laika.tree.Documents.Root
import laika.tree.Documents.Current
import laika.io.OutputProvider.ProviderBuilder
import laika.io.Output
import laika.tree.Elements.TextContainer
import laika.tree.Elements.ElementContainer
import laika.tree.Elements.Element
import scala.collection.mutable.ListBuffer
import java.io.File
import scala.annotation.tailrec
import scala.io.Source

object OutputBuilder {

  
  /* translating render results to Elements gives us nicely formatted PrettyPrint for free */
  
  case class RenderedDocument (path: Path, content: String) extends Element
  
  trait TreeContent extends Element
  
  case class Documents (content: Seq[RenderedDocument]) extends ElementContainer[RenderedDocument, Documents] with TreeContent
  case class Subtrees (content: Seq[RenderedTree]) extends ElementContainer[RenderedTree, Subtrees] with TreeContent
  
  case class RenderedTree (path: Path, content: Seq[TreeContent]) extends ElementContainer[TreeContent, RenderedTree]
  
  
  class TestOutputProvider (val path: Path) extends OutputProvider {
    
    val documents = ListBuffer[(Path,StringBuilder)]()
    
    val subtrees = ListBuffer[TestOutputProvider]()

    def toTree: RenderedTree = new RenderedTree(path, List( 
      Documents(documents map { case (path, builder) => RenderedDocument(path, builder.toString) }),
      Subtrees(subtrees map (_.toTree))
    ) filterNot { case c: ElementContainer[_,_] => c.content.isEmpty })
    
    def newOutput (name: String): Output = {
      val builder = new StringBuilder
      documents += ((path / name, builder))
      Output.toBuilder(builder)
    }
  
    def newChild (name: String): OutputProvider = {
      val provider = new TestOutputProvider(path / name)
      subtrees += provider
      provider
    }
  
  }
  
  
  class TestProviderBuilder extends ProviderBuilder {
    
    val provider = new TestOutputProvider(Root)
    
    def result: RenderedTree = provider.toTree
    
    def build (codec: Codec): OutputProvider = provider
    
  }
  
  
  def createTempDirectory (baseName: String): File = {
    val maxAttempts = 100
    val baseDir = new File(System.getProperty("java.io.tmpdir"))
    val name = System.currentTimeMillis() + "-";
    
    def abort () = throw new IllegalStateException("Failed to create directory within "
        + maxAttempts + " attempts (tried "
        + baseName + "0 to " + baseName + (maxAttempts - 1) + ')')
    
    @tailrec def createDir (num: Int): File = {
      val tempDir = new File(baseDir, name + num);
      if (tempDir.mkdir()) tempDir
      else if (num >= maxAttempts) abort()
      else createDir(num + 1)
    }
    
    createDir(1)
  }
  
  def readFile (base: String): String = readFile(new File(base))
  
  def readFile (f: File): String = {
    val source = Source.fromFile(f)
    val fileContent = source.mkString
    source.close()
    fileContent
  }
  
  
}
