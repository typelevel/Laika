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
import laika.ast._
import laika.ast.Path.Root

import scala.collection.mutable.StringBuilder
import scala.io.Codec

/** Represents the output of a renderer, abstracting over various types of IO resources. 
 *  
 *  The API provided by this trait is only meant to be used internally by a renderer
 *  implementation. For providing hooks for user customization, the renderer should
 *  wrap it in a convenient API appropriate for the corresponding output format, like
 *  the `HTMLWriter` API for HTML renderers for example. 
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
}
sealed trait RenderNavigationContent extends RenderContent {
  def title: Seq[Span]
}

case class RenderedTree (path: Path, title: Seq[Span], content: Seq[RenderContent]) extends RenderNavigationContent {
  val titleDocument: Option[RenderedDocument] = content.collectFirst {
    case doc: RenderedDocument if doc.path.basename == "title" => doc
  }
  val navigationContent: Seq[RenderNavigationContent] = content collect {
    case rnc: RenderNavigationContent => rnc
  }
  val navigationContentAfterTitle: Seq[RenderNavigationContent] = content.collect { // TODO - 0.12 - might keep copied documents separately so that this property becomes obsolete
    case doc: RenderedDocument if doc.path.basename != "title" => doc
    case tree: RenderedTree => tree
  }
}

case class RenderedDocument (path: Path, title: Seq[Span], sections: Seq[SectionInfo], content: String) extends RenderNavigationContent

case class RenderedTemplate (path: Path, content: String) extends RenderContent

case class CopiedDocument (content: BinaryInput) extends RenderContent {
  val path: Path = content.path
}

case class RenderedTreeRoot (coverDocument: Option[RenderedDocument], rootTree: RenderedTree, template: TemplateRoot, config: Config) {
  // TODO - 0.12 - ensure coverDocument is not in rootTree
  val title: Seq[Span] = rootTree.title
  val titleDocument: Option[RenderedDocument] = rootTree.titleDocument
}
