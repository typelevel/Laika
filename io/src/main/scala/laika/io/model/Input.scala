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

import cats.Applicative
import cats.effect.{Async, Resource}
import laika.ast.{DocumentTreeRoot, DocumentType, Navigatable, Path, TextDocumentType}
import laika.bundle.DocumentTypeMatcher
import laika.io.runtime.InputRuntime

import scala.io.Codec

sealed trait InputReader extends Product with Serializable
case class PureReader(input: String) extends InputReader
case class StreamReader(input: Reader, sizeHint: Int) extends InputReader

case class BinaryInput[F[_]] (path: Path, input: Resource[F, InputStream], sourceFile: Option[File] = None) extends Navigatable

/** Character input for the various parsers of this library.
  * 
  * @param path    The full virtual path of this input (does not represent the filesystem path in case of file I/O)
  * @param docType Indicates the type of the document, to distinguish between text markup, templates, configuration 
  *                and style sheets, which all have a different kind of parser
  * @param input   The resource to read the character input from
  * @param sourceFile The source file from the file system, empty if this does not represent a file system resource
  */
case class TextInput[F[_]] (path: Path, docType: TextDocumentType, input: Resource[F, InputReader], sourceFile: Option[File] = None) extends Navigatable

object TextInput {
  def fromString[F[_]: Applicative] (path: Path, docType: TextDocumentType, input: String): TextInput[F] = 
    TextInput[F](path, docType, Resource.pure[F, InputReader](PureReader(input)))
  def fromFile[F[_]: Async] (path: Path, docType: TextDocumentType, file: File, codec: Codec): TextInput[F] =
    TextInput[F](path, docType, InputRuntime.textFileResource[F](file, codec).map(StreamReader(_, file.length.toInt)), Some(file))
  def fromStream[F[_]: Async] (path: Path, docType: TextDocumentType, stream: F[InputStream], codec: Codec, autoClose: Boolean): TextInput[F] =
    TextInput[F](path, docType, InputRuntime.textStreamResource(stream, codec, autoClose).map(StreamReader(_, 8096)))
}

/** A (virtual) tree of input documents, either obtained from scanning a directory recursively or 
  * constructed programmatically (or a mix of both).
  * 
  * Even though the documents are specified as a flat sequence, they logically form a tree based
  * on their virtual path.
  */
case class TreeInput[F[_]] (textInputs: Seq[TextInput[F]], binaryInputs: Seq[BinaryInput[F]], sourcePaths: Seq[String] = Nil) {

  /** Merges the inputs of two collections.
    */
  def ++ (other: TreeInput[F]): TreeInput[F] = TreeInput(
    textInputs ++ other.textInputs, 
    binaryInputs ++ other.binaryInputs, 
    sourcePaths ++ other.sourcePaths
  )
}

/** Factory methods for creating `InputCollections`.
  */
object TreeInput {

  /** An empty input collection.
    */
  def empty[F[_]]: TreeInput[F] = TreeInput(Nil, Nil, Nil)
}

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
case class ParsedTree[F[_]] (root: DocumentTreeRoot, staticDocuments: Seq[BinaryInput[F]])
