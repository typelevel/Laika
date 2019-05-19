/*
 * Copyright 2012-2019 the original author or authors.
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

import java.io._

import com.typesafe.config.Config
import laika.ast.Path.Root
import laika.ast._

import scala.collection.mutable.StringBuilder
import scala.io.Codec

/** Represents the output of a renderer, abstracting over various types of IO resources. 
 *  
 *  The API provided by this trait is only meant to be used internally by a renderer
 *  implementation. For providing hooks for user customization, the renderer should
 *  wrap it in a convenient API appropriate for the corresponding output format, like
 *  the `HTMLFormatter` API for HTML renderers for example. 
 * 
 *  @author Jens Halm
 */
sealed trait Output { 

  /** The full path of this output.
   *  This path is always an absolute path
   *  from the root of the (virtual) output tree,
   *  therefore does not represent the filesystem
   *  path in case of file I/O.
   */
  def path: Path
  
  /** The local name of this output.
   */
  lazy val name: String = path.name
  
}

/** A marker trait for binary output.
  *
  *  Most renderers write character data, but formats like PDF or EPUB
  *  would require a binary stream to write to.
  */
sealed trait BinaryOutput extends Output

/** A marker trait for textual input.
  */
sealed trait TextOutput extends Output

sealed trait TreeOutput extends Output {

  val path: Path = Root
  
}

case class TextFileOutput (file: File, path: Path, codec: Codec) extends TextOutput

// TODO - 0.12 - temporary mutable solution to ease migration
case class StringOutput (builder: StringBuilder, path: Path) extends TextOutput

case class BinaryFileOutput (file: File, path: Path) extends BinaryOutput

// TODO - 0.12 - temporary mutable solution to ease migration
case class ByteOutput (out: ByteArrayOutputStream, path: Path) extends BinaryOutput

case class DirectoryOutput (directory: File, codec: Codec) extends TreeOutput

case object StringTreeOutput extends TreeOutput

sealed trait RenderContent extends Navigatable {
  def path: Path
  def title: Seq[Span]
}

case class RenderedTree (path: Path, title: Seq[Span], content: Seq[RenderContent], titleDocument: Option[RenderedDocument] = None) extends RenderContent

case class RenderedDocument (path: Path, title: Seq[Span], sections: Seq[SectionInfo], content: String) extends RenderContent

case class RenderedTreeRoot (tree: RenderedTree,
                             template: TemplateRoot,
                             config: Config,
                             coverDocument: Option[RenderedDocument] = None,
                             staticDocuments: Seq[BinaryInput] = Nil) {
  
  // TODO - 0.12 - ensure coverDocument is not in rootTree, rename template to defaultTemplate (after checking use cases)
  val title: Seq[Span] = tree.title
  val titleDocument: Option[RenderedDocument] = tree.titleDocument

  /** All documents contained in this tree, fetched recursively, depth-first.
    */
  lazy val allDocuments: Seq[RenderedDocument] = {

    def collect (tree: RenderedTree): Seq[RenderedDocument] = tree.titleDocument.toSeq ++ tree.content.flatMap {
      case doc: RenderedDocument => Seq(doc)
      case sub: RenderedTree     => collect(sub)
    }

    coverDocument.toSeq ++ collect(tree)
  }
}
