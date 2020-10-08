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
import cats.effect.{Resource, Sync}
import laika.ast.Path.Root
import laika.ast.{Document, DocumentTreeRoot, DocumentType, Navigatable, Path, StyleDeclaration, StyleDeclarationSet, TemplateDocument, TextDocumentType}
import laika.bundle.{DocumentTypeMatcher, Precedence}
import laika.config.Config
import laika.io.runtime.TreeResultBuilder.{ConfigResult, DocumentResult, ParserResult, StyleResult, TemplateResult}
import laika.io.runtime.{DirectoryScanner, InputRuntime}
import laika.rewrite.nav.TargetFormats

import scala.io.Codec

sealed trait InputReader extends Product with Serializable
case class PureReader(input: String) extends InputReader
case class StreamReader(input: Reader, sizeHint: Int) extends InputReader

/** A binary input stream and its virtual path within the input tree.
  */
case class BinaryInput[F[_]: Sync] (path: Path, stream: () => InputStream, formats: TargetFormats = TargetFormats.All, autoClose: Boolean = true, sourceFile: Option[File] = None) extends Navigatable {

  /** Provides the InputStream as a cats-effect Resource.
    * 
    * The resource only closes the stream in its tear-down effect when the autoClose property on this instance is true.
    */
  def asResource: Resource[F, InputStream] = {
    val fStream = Sync[F].delay(stream())
    if (autoClose) Resource.fromAutoCloseable(fStream) else Resource.liftF(fStream)
  } 
}

object BinaryInput {
  
  /** Creates a BinaryInput instance for the specified file, assigning the given virtual path.
    */
  def apply[F[_]: Sync] (file: File, path: Path, formats: TargetFormats): BinaryInput[F] = 
    BinaryInput(path, () => new BufferedInputStream(new FileInputStream(file)), formats, autoClose = true, Some(file))
}

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
  def fromFile[F[_]: Sync] (path: Path, docType: TextDocumentType, file: File, codec: Codec): TextInput[F] =
    TextInput[F](path, docType, InputRuntime.textFileResource[F](file, codec).map(StreamReader(_, file.length.toInt)), Some(file))
  def fromStream[F[_]: Sync] (path: Path, docType: TextDocumentType, stream: F[InputStream], codec: Codec, autoClose: Boolean): TextInput[F] =
    TextInput[F](path, docType, InputRuntime.textStreamResource(stream, codec, autoClose).map(StreamReader(_, 8096)))
}

/** A (virtual) tree of input documents, either obtained from scanning a directory recursively or 
  * constructed programmatically (or a mix of both).
  * 
  * Even though the documents are specified as a flat sequence, they logically form a tree based
  * on their virtual path.
  */
case class InputTree[F[_]](textInputs: Seq[TextInput[F]] = Nil,
                           binaryInputs: Seq[BinaryInput[F]] = Nil,
                           parsedResults: Seq[ParserResult] = Nil,
                           sourcePaths: Seq[String] = Nil) {

  /** A collection of all paths in this input tree, which may contain duplicates.
    */
  lazy val allPaths: Seq[Path] = textInputs.map(_.path) ++ binaryInputs.map(_.path) ++ parsedResults.map(_.path) 
  
  /** Merges the inputs of two collections.
    */
  def ++ (other: InputTree[F]): InputTree[F] = InputTree(
    textInputs ++ other.textInputs, 
    binaryInputs ++ other.binaryInputs,
    parsedResults ++ other.parsedResults,
    sourcePaths ++ other.sourcePaths
  )
  
  def + (textInput: TextInput[F]): InputTree[F] = copy(textInputs = textInputs :+ textInput)
  def + (binaryInput: BinaryInput[F]): InputTree[F] = copy(binaryInputs = binaryInputs :+ binaryInput)
  def + (parsedResult: ParserResult): InputTree[F] = copy(parsedResults = parsedResults :+ parsedResult)
}

/** Factory methods for creating `InputTreeBuilder` instances.
  */
object InputTree {

  /** Creates a new, empty InputTreeBuilder will the specified exclusion filter.
    * The filter will only be used for scanning directories when calling `addDirectory` on the builder,
    * not for any of the other methods.
    */
  def apply[F[_]: Sync] (exclude: File => Boolean): InputTreeBuilder[F] = new InputTreeBuilder(exclude, Vector.empty)

  /** Creates a new, empty InputTreeBuilder.
    */
  def apply[F[_]: Sync]: InputTreeBuilder[F] = new InputTreeBuilder(DirectoryInput.hiddenFileFilter, Vector.empty)
  
  /** An empty input tree.
    */
  def empty[F[_]]: InputTree[F] = InputTree(Nil, Nil, Nil)
}


/** Builder API for freely constructing input trees from directories, files, classpath resources, in-memory strings
  * or pre-constructed AST trees.
  * 
  * If your input is just one or more directories, you can use the corresponding shortcuts on the parser or
  * transformer instances, e.g. `transformer.fromDirectory(...).toDirectory(...)`.
  * This builder is meant to be used for situations where more flexibility is required.
  * 
  * All the specified inputs will be combined into a single logical tree and each document gets a virtual path
  * assigned that describes its logical position within the tree.
  * As a consequence all cross-linking or referencing of images can happen within the virtual path abstraction,
  * meaning a resource from the file system can point to an input constructed in-memory via a relative, virtual path.
  * 
  * When adding strings, files or directories you need to specify a "mount point" that signals where within
  * the virtual tree the inputs should be placed.
  * For adding directories the mount point is optional, when omitted the directory becomes the virtual root of
  * the input tree.
  * 
  * The resulting input tree can be passed to parsers, transformers and theme builders.
  * 
  * Example for composing inputs from two directories, a template loaded from the classpath and a CSS
  * file generated in-memory:
  * 
  * {{{
  * val inputs = InputTree[F]
  *   .addDirectory("/path-to-my/markup-files")
  *   .addDirectory("/path-to-my/images", Root / "images")
  *   .addClasspathResource("my-templates/default.template.html", DefaultTemplatePath.forHTML)
  *   .addString(generateMyStyles(), Root / "css" / "site.css")
  * }}}
  * 
  * These inputs can then be configured for the sbt plugin:
  * 
  * {{{
  *   laikaInputs := inputs
  * }}}
  * 
  * Or passed to a `TreeTransformer` instance:
  * 
  * {{{
  * val res: F[RenderedTreeRoot[F]] = transformer.use {
  *   _.fromInputTree(inputs)
  *    .toDirectory("target")
  *    .transform
  * }
  * }}}
  */
class InputTreeBuilder[F[_]](private[model] val exclude: File => Boolean, 
                             private[model] val steps: Vector[(Path => DocumentType, File => Boolean) => Kleisli[F, InputTree[F], InputTree[F]]])(implicit F: Sync[F]) {
  
  import cats.implicits._
  
  private def addStep (step: (Path => DocumentType, File => Boolean) => Kleisli[F, InputTree[F], InputTree[F]]): InputTreeBuilder[F] =
    new InputTreeBuilder(exclude, steps = steps :+ step)
  
  private def addStep (path: Path)(f: PartialFunction[DocumentType, InputTree[F] => InputTree[F]]): InputTreeBuilder[F] = 
    addStep { (docTypeFunction, _) => 
      Kleisli { tree =>
        f.applyOrElse[DocumentType, InputTree[F] => InputTree[F]](docTypeFunction(path), _ => identity)(tree).pure[F]
      }
    }
  
  private def addParserResult (result: ParserResult): InputTreeBuilder[F] = addStep { (_,_) =>
    Kleisli(tree => (tree + result).pure[F])
  }

  /** Adds the specified directories to the input tree, merging them all into a single virtual root, recursively.
    */
  def addDirectories (dirs: Seq[File])(implicit codec: Codec): InputTreeBuilder[F] = dirs.foldLeft(this) {
    case (builder, dir) => builder.addDirectory(dir)
  }

  /** Adds the specified directories to the input tree, placing it in the virtual root.
    */
  def addDirectory (name: String)(implicit codec: Codec): InputTreeBuilder[F] = addDirectory(new File(name), Root)

  /** Adds the specified directories to the input tree, placing it at the specified mount point in the virtual tree.
    */
  def addDirectory (name: String, mountPoint: Path)(implicit codec: Codec): InputTreeBuilder[F] = 
    addDirectory(new File(name), mountPoint)

  /** Adds the specified directories to the input tree, placing it in the virtual root.
    */
  def addDirectory (dir: File)(implicit codec: Codec): InputTreeBuilder[F] = addDirectory(dir, Root)

  /** Adds the specified directories to the input tree, placing it at the specified mount point in the virtual tree.
    */
  def addDirectory (dir: File, mountPoint: Path)(implicit codec: Codec): InputTreeBuilder[F] = addStep { (docTypeFunction, fileFilter) =>
    Kleisli { input => 
      DirectoryScanner.scanDirectories[F](new DirectoryInput(Seq(dir), codec, docTypeFunction, fileFilter, mountPoint)).map(input ++ _)
    }
  }

  /** Adds the specified file to the input tree, placing it at the specified mount point in the virtual tree.
    * 
    * The content type of the stream will be determined by the suffix of the virtual path, e.g.
    * `doc.md` would be passed to the markup parser, `doc.template.html` to the template parser, and so on.
    */
  def addFile (name: String, mountPoint: Path)(implicit codec: Codec): InputTreeBuilder[F] = 
    addFile(new File(name), mountPoint)

  /** Adds the specified file to the input tree, placing it at the specified mount point in the virtual tree.
    * 
    * The content type of the stream will be determined by the suffix of the virtual path, e.g.
    * `doc.md` would be passed to the markup parser, `doc.template.html` to the template parser, and so on.
    */
  def addFile (file: File, mountPoint: Path)(implicit codec: Codec): InputTreeBuilder[F] =
    addStep(mountPoint) {
      case DocumentType.Static(formats) => _ + BinaryInput(file, mountPoint, formats)
      case docType: TextDocumentType    => _ + TextInput.fromFile[F](mountPoint, docType, file, codec)
    }

  /** Adds the specified classpath resource to the input tree, placing it at the specified mount point in the virtual tree.
    * The specified name must be compatible with Java's `ClassLoader.getResource`.
    * 
    * The content type of the stream will be determined by the suffix of the virtual path, e.g.
    * `doc.md` would be passed to the markup parser, `doc.template.html` to the template parser, and so on.
    */
  def addClasspathResource (name: String, mountPoint: Path)(implicit codec: Codec): InputTreeBuilder[F] = {
    def stream = {
      val res = getClass.getClassLoader.getResourceAsStream(name)
      if (res == null) throw new IOException(s"Classpath resource '$name' does not exist")
      else res
    }
    addStream(stream, mountPoint)
  }

  /** Adds the specified input stream to the input tree, placing it at the specified mount point in the virtual tree.
    * 
    * The content type of the stream will be determined by the suffix of the virtual path, e.g.
    * `doc.md` would be passed to the markup parser, `doc.template.html` to the template parser, and so on.
    * 
    * This is a by-name argument so that the effectful creation can be deferred internally.
    * The method does not accept an `F[InputStream]` even though this would appear most intuitive,
    * as there are cases like PDF rendering where the stream has to be passed to an impure Java callback,
    * where Laika does the necessary effect-wrapping on a higher level.
    * 
    * The autoClose argument indicates whether the stream should be closed after use.
    */
  def addStream (stream: => InputStream, mountPoint: Path, autoClose: Boolean = true)(implicit codec: Codec): InputTreeBuilder[F] =
    addStep(mountPoint) {
      case DocumentType.Static(formats) =>
        // for binary inputs the wrap-into-effect step has to be deferred due to integration issues with impure libs (e.g. Apache FOP)
        _ + BinaryInput(mountPoint, () => stream, formats, autoClose)
      case docType: TextDocumentType => 
        _ + TextInput.fromStream(mountPoint, docType, F.delay(stream), codec, autoClose)
    }

  /** Adds the specified string resource to the input tree, placing it at the specified mount point in the virtual tree.
    * 
    * The content type of the stream will be determined by the suffix of the virtual path, e.g.
    * * `doc.md` would be passed to the markup parser, `doc.template.html` to the template parser, and so on.
    */
  def addString (str: String, mountPoint: Path): InputTreeBuilder[F] =
    addStep(mountPoint) {
      case DocumentType.Static(formats) => _ + BinaryInput(mountPoint, () => new ByteArrayInputStream(str.getBytes), formats)
      case docType: TextDocumentType    => _ + TextInput.fromString[F](mountPoint, docType, str)
    }

  /** Adds the specified document AST to the input tree, by-passing the parsing step.
    * 
    * In some cases when generating input on the fly, it might be more convenient or more type-safe to construct
    * the AST directly than to generate the text markup as input for the parser.
    */
  def addDocument (doc: Document): InputTreeBuilder[F] = addParserResult(DocumentResult(doc))

  /** Adds the specified template AST to the input tree, by-passing the parsing step.
    *
    * In some cases when generating input on the fly, it might be more convenient or more type-safe to construct
    * the AST directly than to generate the template as a string as input for the template parser.
    */
  def addTemplate (doc: TemplateDocument): InputTreeBuilder[F] = addParserResult(TemplateResult(doc))

  /** Adds the specified configuration instance and assigns it to the specified tree path in a way
    * that is equivalent to having a HOCON file called `directory.conf` in that directory.
    */
  def addConfig (config: Config, treePath: Path): InputTreeBuilder[F] = addParserResult(ConfigResult(treePath, config))

  /** Adds the specified styles for PDF to the input tree.
    * These type of style declarations are only used in the context of Laika's "CSS for PDF" support 
    * which works slightly differently than web CSS as PDF generation Laika is not based on interim HTML results.
    */
  def addStyles (styles: Set[StyleDeclaration], path: Path, precedence: Precedence = Precedence.High): InputTreeBuilder[F] = 
    addParserResult(StyleResult(StyleDeclarationSet(Set(path), styles, precedence), "fo"))

  /** Adds the specified file filter to this input tree.
    * 
    * The filter will only be used for scanning directories when calling `addDirectory` on this builder,
    * not for any of the other methods.
    */
  def withFileFilter (filter: File => Boolean): InputTreeBuilder[F] = {
    val combinedFilter: File => Boolean = f => filter(f) || exclude(f)
    new InputTreeBuilder(combinedFilter, steps)
  }

  /** Merges this input tree with the specified tree, recursively.
    */
  def merge (other: InputTreeBuilder[F]): InputTreeBuilder[F] = 
    new InputTreeBuilder(f => other.exclude(f) || exclude(f), steps ++ other.steps)

  /** Builds the tree based on the inputs added to this instance.
    * 
    * The method is effectful as it might involve scanning directories to determine the tree structure.
    * 
    * This method is normally not called by application code directly, as the parser and transformer APIs
    * expect an `InputTreeBuilder` instance.
    */
  def build: F[InputTree[F]] = build(DocumentTypeMatcher.base)

  /** Builds the tree based on the inputs added to this instance and the specified custom document type matcher.
    *
    * The method is effectful as it might involve scanning directories to determine the tree structure.
    *
    * This method is normally not called by application code directly, as the parser and transformer APIs
    * expect an `InputTreeBuilder` instance.
    */
  def build (docTypeMatcher: Path => DocumentType): F[InputTree[F]] = 
    steps
      .map(_(docTypeMatcher, exclude))
      .reduceLeftOption(_ andThen _)
      .fold(InputTree.empty[F].pure[F])(_.run(InputTree.empty[F]))

}



/** A directory in the file system containing input documents for a tree transformation.
  *
  * The specified `docTypeMatcher` is responsible for determining the type of input 
  * (e.g. text markup, template, etc.) based on the (virtual) document path.
  */
case class DirectoryInput (directories: Seq[File],
                           codec: Codec,
                           docTypeMatcher: Path => DocumentType = DocumentTypeMatcher.base,
                           fileFilter: File => Boolean = DirectoryInput.hiddenFileFilter,
                           mountPoint: Path = Root) {
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

  /** Creates a file filter that filters all files in the specified directory.
    */
  def filter (dir: File): File => Boolean = file => {
    val filePath = file.getCanonicalPath
    val dirPath = dir.getCanonicalPath
    filePath.startsWith(dirPath)
  }
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
case class ParsedTree[F[_]] (root: DocumentTreeRoot, staticDocuments: Seq[BinaryInput[F]]) {

  /** Removes all static documents of this instance that match the specified filter.
    */
  def removeStaticDocuments (filter: Path => Boolean): ParsedTree[F] = copy(
    root = root.copy(staticDocuments = root.staticDocuments.filterNot(filter)),
    staticDocuments = staticDocuments.filterNot(doc => filter(doc.path))
  )
  
  /** Removes all static documents of this instance and replaces them with the specified alternatives.
    */
  def replaceStaticDocuments (newStaticDocs: Seq[BinaryInput[F]]): ParsedTree[F] = copy(
    root = root.copy(staticDocuments = newStaticDocs.map(_.path)),
    staticDocuments = newStaticDocs
  )

  /** Adds the specified static documents to this instance.
    */
  def addStaticDocuments (newStaticDocs: Seq[BinaryInput[F]]): ParsedTree[F] = copy(
    root = root.copy(staticDocuments = root.staticDocuments ++ newStaticDocs.map(_.path)),
    staticDocuments = staticDocuments ++ newStaticDocs
  )
  
}
