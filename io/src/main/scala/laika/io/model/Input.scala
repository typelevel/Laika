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

package laika.io.model

import java.io._

import laika.ast.Path.Root
import laika.ast.{DocumentTreeRoot, DocumentType, Path, TextDocumentType}
import laika.bundle.DocumentTypeMatcher

import scala.io.Codec

/** Represents the input for a parser, abstracting over various types of IO resources. 
 *  
 *  @author Jens Halm
 */
sealed trait Input extends Product with Serializable {

  /** The full virtual path of this input.
   *  This path is always an absolute path
   *  from the root of the (virtual) input tree,
   *  therefore does not represent the filesystem
   *  path in case of file I/O.
   */
  def path: Path
  
  /** The local name of this input.
   */
  lazy val name: String = path.name

}

/** A marker trait for binary input.
  * 
  * In the context of this library binary inputs are usually only
  * used by `Parallel` parsers and transformers for copying static
  * documents from input to output directory.
  */
sealed trait BinaryInput extends Input

/** A marker trait for character input.
  */
sealed trait TextInput extends Input {

  /** The type of the document, to distinguish between
    * text markup, templates, configuration and style sheets,
    * which all have a different kind of parser.
    */
  def docType: TextDocumentType
  
}

case class StringInput (source: String, docType: TextDocumentType, path: Path = Root) extends TextInput

case class TextFileInput (file: File, docType: TextDocumentType, path: Path, codec: Codec) extends TextInput

case class CharStreamInput (stream: InputStream, docType: TextDocumentType, path: Path, autoClose: Boolean, codec: Codec) extends TextInput


case class BinaryFileInput (file: File, path: Path) extends BinaryInput

case class BinaryStreamInput (stream: InputStream, autoClose: Boolean, path: Path) extends BinaryInput

/** A directory in the file system containing input documents for a tree transformation.
  * 
  * The specified `docTypeMatcher` is responsible for determining the type of input 
  * (e.g. text markup, template, etc.) based on the (virtual) document path.
  */
case class DirectoryInput (directories: Seq[File],
                           codec: Codec,
                           docTypeMatcher: Path => DocumentType = DocumentTypeMatcher.base,
                           fileFilter: File => Boolean = DirectoryInput.hiddenFileFilter) {
  lazy val sourcePaths: Seq[String] = directories map (_.getAbsolutePath)
}

object DirectoryInput {
  
  /** A filter that selects files that are hidden according to `java.io.File.isHidden`.
    */
  val hiddenFileFilter: File => Boolean = file => file.isHidden && file.getName != "."

  /** Creates a new instance using the library's defaults for the `docTypeMatcher` and
    * `fileFilter` properties.
    */
  def apply (directory: File)(implicit codec: Codec): DirectoryInput = DirectoryInput(Seq(directory), codec)
}

/** A (virtual) tree of input documents, either obtained from scanning a directory recursively or 
  * constructed programmatically (or a mix of both).
  * 
  * Even though the documents are specified as a flat sequence, they logically form a tree based
  * on their virtual path.
  */
case class InputCollection[F[_]] (textInputs: Seq[TextInput], binaryInputs: Seq[StaticDocument[F]], sourcePaths: Seq[String] = Nil) {

  /** Merges the inputs of two collections.
    */
  def ++ (other: InputCollection[F]): InputCollection[F] = InputCollection(
    textInputs ++ other.textInputs, 
    binaryInputs ++ other.binaryInputs, 
    sourcePaths ++ other.sourcePaths
  )
}

/** Factory methods for creating `InputCollections`.
  */
object InputCollection {

  /** Creates an input collection consisting solely of the specified single text input.
    */
  def apply[F[_]] (textInput: TextInput): InputCollection[F] = InputCollection(Seq(textInput), Nil, Nil)

  /** An empty input collection.
    */
  def empty[F[_]]: InputCollection[F] = InputCollection(Nil, Nil, Nil)
}

/** The result of a parsing operation for an entire document tree.
  * 
  * The `DocumentTreeRoot` is the recursive structure of parsed inputs, like markup
  * document, templates or other file types, represented by their AST.
  * 
  * The static documents are merely a sequence of unprocessed inputs that have
  * been discovered via directory scanning (or have been passed programmatically).
  * The actual processing of these inputs is left to the render step, which
  * might copy them into a target directory, or embed them into an output format
  * like EPUB.
  */
case class ParsedTree[F[_]] (root: DocumentTreeRoot, staticDocuments: Seq[StaticDocument[F]])
