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

package laika.io.model

import java.io._

import cats.effect.Resource
import laika.config.Config
import laika.ast.Path.Root
import laika.ast._

import scala.io.Codec

/** Represents the output of a renderer, abstracting over various types of IO resources. 
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

/** A marker trait for textual output.
  */
sealed trait TextOutput extends Output

/** A (virtual) tree of output documents.
  */
sealed trait TreeOutput extends Output {

  val path: Path = Root
  
}

case class TextFileOutput (file: File, path: Path, codec: Codec) extends TextOutput

case class StringOutput (path: Path) extends TextOutput

case class CharStreamOutput (stream: OutputStream, path: Path, autoClose: Boolean, codec: Codec) extends TextOutput

/** A resource for binary output.
  *
  * Most renderers write character data, but formats like PDF or EPUB
  * require a binary stream to write to.
  */
case class BinaryOutput[F[_]] (path: Path, output: Resource[F, OutputStream])

/** A directory as a target for a rendering operation of a document tree.
  * 
  * The specified codec will be used for writing all character output.
  */
case class DirectoryOutput (directory: File, codec: Codec) extends TreeOutput

/** Instructs the renderer to produce an in-memory representation of the
  * tree of rendered outputs.
  */
case object StringTreeOutput extends TreeOutput

/** A titled, positional element in the tree of rendered documents.
  */
sealed trait RenderContent extends Navigatable {
  def path: Path
  def title: Seq[Span]
}

/** Represents a node of the tree of rendered documents.
  * 
  * @param path the full, absolute path of this (virtual) document tree
  * @param title the title of this tree, either obtained from the title document or configuration
  * @param content the rendered documents and subtrees in a recursive structure
  * @param titleDocument the optional title document of this tree   
  */
case class RenderedTree (path: Path, 
                         title: Seq[Span], 
                         content: Seq[RenderContent], 
                         titleDocument: Option[RenderedDocument] = None) extends RenderContent

/** A single rendered document with the content as a plain string in the target format.
  * 
  * The title and section info are still represented as an AST, so they be used in any subsequent
  * step that needs to produce navigation structures.
  */
case class RenderedDocument (path: Path, title: Seq[Span], sections: Seq[SectionInfo], content: String) extends RenderContent

/** Represents the root of a tree of rendered documents. In addition to the recursive structure of documents
  * it holds additional items like static or cover documents, which may contribute to the output of a site or an e-book.
  *
  * @param tree the recursive structure of documents, usually obtained from parsing text markup 
  * @param defaultTemplate the default template configured for the output format, which may be used by a post-processor
  * @param config the root configuration of the rendered tree                       
  * @param coverDocument the cover document (usually used with e-book formats like EPUB and PDF)            
  * @param staticDocuments the paths of documents that were neither identified as text markup, config or templates, 
  *                        and will potentially be embedded or copied as is to the final output, depending on the output format
  * @param sourcePaths the paths this document tree has been built from or an empty list if this ast does not originate from the file system
  */
case class RenderedTreeRoot[F[_]] (tree: RenderedTree,
                                   defaultTemplate: TemplateRoot,
                                   config: Config,
                                   coverDocument: Option[RenderedDocument] = None,
                                   staticDocuments: Seq[BinaryInput[F]] = Nil,
                                   sourcePaths: Seq[String] = Nil) {

  /** The title of the tree, either obtained from the title document or configuration 
    */
  val title: Seq[Span] = tree.title

  /** The optional title document of the tree.
    */
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
