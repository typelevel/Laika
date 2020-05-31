/*
 * Copyright 2012-2020 the original author or authors.
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
import cats.data.Kleisli
import cats.effect.{Async, Resource}
import laika.ast.Path.Root
import laika.ast.{Document, DocumentTreeRoot, DocumentType, Navigatable, Path, StyleDeclaration, StyleDeclarationSet, TemplateDocument, TextDocumentType}
import laika.bundle.DocumentTypeMatcher
import laika.config.Config
import laika.io.runtime.TreeResultBuilder.{ConfigResult, DocumentResult, ParserResult, StyleResult, TemplateResult}
import laika.io.runtime.{DirectoryScanner, InputRuntime}

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
case class TreeInput[F[_]] (textInputs: Seq[TextInput[F]], 
                            binaryInputs: Seq[BinaryInput[F]], 
                            parsedResults: Seq[ParserResult], 
                            sourcePaths: Seq[String] = Nil) {

  /** Merges the inputs of two collections.
    */
  def ++ (other: TreeInput[F]): TreeInput[F] = TreeInput(
    textInputs ++ other.textInputs, 
    binaryInputs ++ other.binaryInputs,
    parsedResults ++ other.parsedResults,
    sourcePaths ++ other.sourcePaths
  )
  
  def + (textInput: TextInput[F]): TreeInput[F] = copy(textInputs = textInputs :+ textInput)
  def + (binaryInput: BinaryInput[F]): TreeInput[F] = copy(binaryInputs = binaryInputs :+ binaryInput)
  def + (parsedResult: ParserResult): TreeInput[F] = copy(parsedResults = parsedResults :+ parsedResult)
}

/** Factory methods for creating `TreeInput` instances.
  */
object TreeInput {

  def apply[F[_]: Async] (exclude: File => Boolean): InputTreeBuilder[F] = new InputTreeBuilder(exclude, Vector.empty)
  
  def apply[F[_]: Async]: InputTreeBuilder[F] = new InputTreeBuilder(DirectoryInput.hiddenFileFilter, Vector.empty)
  
  
  /** An empty input collection.
    */
  def empty[F[_]]: TreeInput[F] = TreeInput(Nil, Nil, Nil)
}


class InputTreeBuilder[F[_]] (exclude: File => Boolean, steps: Vector[(Path => DocumentType) => Kleisli[F, TreeInput[F], TreeInput[F]]])(implicit F: Async[F]) {
  
  import cats.implicits._
  
  private def addStep (step: (Path => DocumentType) => Kleisli[F, TreeInput[F], TreeInput[F]]): InputTreeBuilder[F] =
    new InputTreeBuilder(exclude, steps = steps :+ step)
  
  private def addStep (path: Path)(f: PartialFunction[DocumentType, TreeInput[F] => TreeInput[F]]): InputTreeBuilder[F] = 
    addStep { docTypeFunction => 
      Kleisli { tree =>
        f.applyOrElse[DocumentType, TreeInput[F] => TreeInput[F]](docTypeFunction(path), _ => identity)(tree).pure[F]
      }
    }
  
  private def addParserResult (result: ParserResult): InputTreeBuilder[F] = addStep { _ =>
    Kleisli(tree => (tree + result).pure[F])
  }
  
  def addDirectory (name: String)(implicit codec: Codec): InputTreeBuilder[F] = addDirectory(new File(name), Root)
  def addDirectory (name: String, mountPoint: Path)(implicit codec: Codec): InputTreeBuilder[F] = 
    addDirectory(new File(name), mountPoint)
  def addDirectory (dir: File)(implicit codec: Codec): InputTreeBuilder[F] = addDirectory(dir, Root)
  def addDirectory (dir: File, mountPoint: Path)(implicit codec: Codec): InputTreeBuilder[F] = addStep { docTypeFunction =>
    Kleisli { input => 
      DirectoryScanner.scanDirectories[F](new DirectoryInput(Seq(dir), codec, docTypeFunction, exclude)).map(input ++ _)
    }
  }

  def addFile (name: String, mountPoint: Path)(implicit codec: Codec): InputTreeBuilder[F] = 
    addFile(new File(name), mountPoint)
  def addFile (file: File, mountPoint: Path)(implicit codec: Codec): InputTreeBuilder[F] =
    addStep(mountPoint) {
      case DocumentType.Static => _ + BinaryInput(mountPoint, InputRuntime.binaryFileResource(file), Some(file))
      case docType: TextDocumentType => _ + TextInput.fromFile[F](mountPoint, docType, file, codec)
    }
  
  def addStream (stream: F[InputStream], mountPoint: Path, autoClose: Boolean = true)(implicit codec: Codec): InputTreeBuilder[F] =
    addStep(mountPoint) {
      case DocumentType.Static => 
        _ + BinaryInput(mountPoint, if (autoClose) Resource.fromAutoCloseable(stream) else Resource.liftF(stream))
      case docType: TextDocumentType => 
        _ + TextInput.fromStream(mountPoint, docType, stream, codec, autoClose)
    }
  
  def addString (str: String, mountPoint: Path): InputTreeBuilder[F] =
    addStep(mountPoint) {
      case DocumentType.Static => _ + BinaryInput(mountPoint, Resource.liftF(F.delay(new ByteArrayInputStream(str.getBytes))))
      case docType: TextDocumentType => _ + TextInput.fromString[F](mountPoint, docType, str)
    }
  
  def addDocument (doc: Document): InputTreeBuilder[F] = addParserResult(DocumentResult(doc))
  def addTemplate (doc: TemplateDocument): InputTreeBuilder[F] = addParserResult(TemplateResult(doc))
  def addConfig (config: Config, treePath: Path): InputTreeBuilder[F] = addParserResult(ConfigResult(treePath, config))
  def addStyles (styles: Set[StyleDeclaration], path: Path): InputTreeBuilder[F] = 
    addParserResult(StyleResult(StyleDeclarationSet(Set(path), styles), "fo"))
  
  def build (docTypeMatcher: Path => DocumentType): F[TreeInput[F]] = 
    steps
      .map(_(docTypeMatcher))
      .reduceLeftOption(_ andThen _)
      .fold(TreeInput.empty[F].pure[F])(_.run(TreeInput.empty[F]))

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
