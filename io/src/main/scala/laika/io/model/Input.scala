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

import cats.data.Kleisli
import cats.effect.{Async, Sync}
import cats.syntax.all._
import cats.{Applicative, Functor}
import fs2.io.file.Files
import laika.ast.Path.Root
import laika.ast._
import laika.bundle.{DocumentTypeMatcher, Precedence}
import laika.config.Config
import laika.io.runtime.DirectoryScanner
import laika.io.runtime.TreeResultBuilder.{ConfigResult, DocumentResult, ParserResult, StyleResult, TemplateResult}
import laika.parse.markup.DocumentParser.DocumentInput
import laika.rewrite.nav.TargetFormats

import java.io.{File, IOException, InputStream}
import scala.io.Codec

/** A binary input stream and its virtual path within the input tree.
  */
case class BinaryInput[F[_]: Sync] (path: Path, 
                                    input: fs2.Stream[F, Byte], 
                                    formats: TargetFormats = TargetFormats.All, 
                                    sourceFile: Option[File] = None) extends Navigatable

object BinaryInput {
  
  def fromString[F[_]: Sync] (path: Path, input: String, targetFormats: TargetFormats = TargetFormats.All): BinaryInput[F] = {
    val stream = fs2.Stream.emit(input).through(fs2.text.utf8.encode)
    BinaryInput(path, stream, targetFormats)
  }

  def fromFile[F[_]: Async] (path: Path, file: File, targetFormats: TargetFormats = TargetFormats.All): BinaryInput[F] = {
    val stream = Files[F].readAll(fs2.io.file.Path.fromNioPath(file.toPath))
    BinaryInput[F](path, stream, targetFormats, Some(file))
  }

  def fromStream[F[_]: Sync] (path: Path, stream: F[InputStream], autoClose: Boolean, targetFormats: TargetFormats = TargetFormats.All): BinaryInput[F] = {
    val input = fs2.io.readInputStream(stream, 8096, autoClose)
    BinaryInput(path, input, targetFormats)
  }
}

/** Character input for the various parsers of this library.
  * 
  * @param path    The full virtual path of this input (does not represent the filesystem path in case of file I/O)
  * @param docType Indicates the type of the document, to distinguish between text markup, templates, configuration 
  *                and style sheets, which all have a different kind of parser
  * @param input   The character input
  * @param sourceFile The source file from the file system, empty if this does not represent a file system resource
  */
case class TextInput[F[_]: Functor] (path: Path, docType: TextDocumentType, input: F[String], sourceFile: Option[File] = None) extends Navigatable {
  lazy val asDocumentInput: F[DocumentInput] = input.map(DocumentInput(path, _))
}

object TextInput {

  private def readAll[F[_]: Sync](input: fs2.Stream[F, Byte], codec: Codec): F[String] = 
    input.through(fs2.text.decodeWithCharset(codec.charSet)).compile.string

  def fromString[F[_]: Applicative] (path: Path, docType: TextDocumentType, input: String): TextInput[F] = 
    TextInput[F](path, docType, Applicative[F].pure(input))

  def fromFile[F[_]: Async] (path: Path, docType: TextDocumentType, file: File, codec: Codec): TextInput[F] = {
    val input = readAll(Files[F].readAll(fs2.io.file.Path.fromNioPath(file.toPath)), codec)
    TextInput[F](path, docType, input, Some(file))
  }

  def fromStream[F[_]: Sync] (path: Path, docType: TextDocumentType, stream: F[InputStream], codec: Codec, autoClose: Boolean): TextInput[F] = {
    val input = readAll(fs2.io.readInputStream(stream, 8096, autoClose), codec)
    TextInput[F](path, docType, input)
  }
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
                           providedPaths: Seq[StaticDocument] = Nil,
                           sourcePaths: Seq[String] = Nil) {

  /** A collection of all paths in this input tree, which may contain duplicates.
    */
  lazy val allPaths: Seq[Path] = textInputs.map(_.path) ++ binaryInputs.map(_.path) ++ parsedResults.map(_.path) 
  
  /** Merges the inputs of two trees recursively.
    * 
    * This method does not perform any de-duplication in case both trees contain entries with the same virtual path, 
    * which would lead to errors when such a tree is used as input for a transformation.
    * If one tree should take precedence over the other in case of duplicates, use `overrideWith` instead. 
    */
  def ++ (other: InputTree[F]): InputTree[F] = InputTree(
    textInputs ++ other.textInputs, 
    binaryInputs ++ other.binaryInputs,
    parsedResults ++ other.parsedResults,
    providedPaths ++ other.providedPaths,
    sourcePaths ++ other.sourcePaths
  )
  
  /** Overrides inputs in this instance with the provided inputs.
    * 
    * Merges the inputs of two trees recursively, like the `++` method, but with the main difference
    * that in case both trees contain an entry with the same virtual path, 
    * the one in the overriding tree will take precedence and the one in this tree will be removed.
    */
  def overrideWith (overrides: InputTree[F]): InputTree[F] = remove(overrides.allPaths.toSet) ++ overrides
  
  def + (textInput: TextInput[F]): InputTree[F] = copy(textInputs = textInputs :+ textInput)
  def + (binaryInput: BinaryInput[F]): InputTree[F] = copy(binaryInputs = binaryInputs :+ binaryInput)
  def + (parsedResult: ParserResult): InputTree[F] = copy(parsedResults = parsedResults :+ parsedResult)
  def + (providedPath: StaticDocument): InputTree[F] = copy(providedPaths = providedPaths :+ providedPath)

  /** Returns a new input tree with all inputs matching the provided exclusions removed from this tree.
    */
  def remove (paths: Set[Path]): InputTree[F] = {
    InputTree(
      textInputs    = textInputs.filterNot(in => paths.contains(in.path)),
      binaryInputs  = binaryInputs.filterNot(in => paths.contains(in.path)),
      parsedResults = parsedResults.filterNot(in => paths.contains(in.path)),
      sourcePaths   = sourcePaths
    )
  }
  
}

/** Factory methods for creating `InputTreeBuilder` instances.
  */
object InputTree {

  /** Creates a new, empty InputTreeBuilder will the specified exclusion filter.
    * The filter will only be used for scanning directories when calling `addDirectory` on the builder,
    * not for any of the other methods.
    */
  def apply[F[_]: Async] (exclude: File => Boolean): InputTreeBuilder[F] = new InputTreeBuilder(exclude, Vector.empty, Vector.empty)

  /** Creates a new, empty InputTreeBuilder.
    */
  def apply[F[_]: Async]: InputTreeBuilder[F] = new InputTreeBuilder(DirectoryInput.hiddenFileFilter, Vector.empty, Vector.empty)
  
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
class InputTreeBuilder[F[_]](private[laika] val exclude: File => Boolean, 
                             private[model] val steps: Vector[(Path => DocumentType, File => Boolean) => Kleisli[F, InputTree[F], InputTree[F]]],
                             private[laika] val fileRoots: Vector[File])(implicit F: Async[F]) {
  
  import cats.implicits._

  private def addStep (step: (Path => DocumentType, File => Boolean) => Kleisli[F, InputTree[F], InputTree[F]]): InputTreeBuilder[F] =
    addStep(None)(step)
    
  private def addStep (newFileRoot: Option[File])
                      (step: (Path => DocumentType, File => Boolean) => Kleisli[F, InputTree[F], InputTree[F]]): InputTreeBuilder[F] =
    new InputTreeBuilder(exclude, steps = steps :+ step, newFileRoot.fold(fileRoots)(fileRoots :+ _))
  
  private def addStep (path: Path, newFileRoot: Option[File] = None)
                      (f: PartialFunction[DocumentType, InputTree[F] => InputTree[F]]): InputTreeBuilder[F] = 
    addStep(newFileRoot) { (docTypeFunction, _) =>
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
  def addDirectory (dir: File, mountPoint: Path)(implicit codec: Codec): InputTreeBuilder[F] = addStep(Some(dir)) { (docTypeFunction, fileFilter) =>
    if (fileFilter(dir)) Kleisli.ask else
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
    addStep(mountPoint, Some(file)) {
      case DocumentType.Static(formats) => _ + BinaryInput.fromFile(mountPoint, file, formats)
      case docType: TextDocumentType    => _ + TextInput.fromFile[F](mountPoint, docType, file, codec)
    }

  /** Adds the specified classpath resource to the input tree, placing it at the specified mount point in the virtual tree.
    * The specified name must be compatible with Java's `ClassLoader.getResource`.
    * 
    * The content type of the stream will be determined by the suffix of the virtual path, e.g.
    * `doc.md` would be passed to the markup parser, `doc.template.html` to the template parser, and so on.
    */
  def addClasspathResource (name: String, mountPoint: Path)(implicit codec: Codec): InputTreeBuilder[F] = {
    val streamF = F.delay(getClass.getClassLoader.getResourceAsStream(name)).flatMap[InputStream] {
      case null   => F.raiseError(new IOException(s"Classpath resource '$name' does not exist"))
      case stream => F.pure(stream)
    }
    addStream(streamF, mountPoint)
  }

  /** Adds the specified input stream to the input tree, placing it at the specified mount point in the virtual tree.
    * 
    * The content type of the stream will be determined by the suffix of the virtual path, e.g.
    * `doc.md` would be passed to the markup parser, `doc.template.html` to the template parser, and so on.
    * 
    * The `autoClose` argument indicates whether the stream should be closed after use.
    * In some integration scenarios with 3rd-party libraries, e.g. for PDF creation, `autoClose` is not
    * guaranteed as the handling of the stream is entirely managed by the 3rd party tool.
    */
  def addStream (stream: F[InputStream], mountPoint: Path, autoClose: Boolean = true)(implicit codec: Codec): InputTreeBuilder[F] =
    addStep(mountPoint) {
      case DocumentType.Static(formats) =>
        _ + BinaryInput.fromStream(mountPoint,stream, autoClose, formats)
      case docType: TextDocumentType => 
        _ + TextInput.fromStream(mountPoint, docType, stream, codec, autoClose)
    }

  /** Adds the specified string resource to the input tree, placing it at the specified mount point in the virtual tree.
    * 
    * The content type of the stream will be determined by the suffix of the virtual path, e.g.
    * * `doc.md` would be passed to the markup parser, `doc.template.html` to the template parser, and so on.
    */
  def addString (str: String, mountPoint: Path): InputTreeBuilder[F] =
    addStep(mountPoint) {
      case DocumentType.Static(formats) => _ + BinaryInput.fromString(mountPoint,str, formats)
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

  /** Adds a path to the input tree that represents a document getting processed by some external tool.
    * Such a path will be used in link validation, but no further processing for this document will be performed. 
    */
  def addProvidedPath (path: Path): InputTreeBuilder[F] = addStep(path) {
    case DocumentType.Static(formats) => _ + StaticDocument(path, formats)
    case _                            => _ + StaticDocument(path)
  }

  /** Adds the specified paths to the input tree that represent documents getting processed by some external tool.
    * Such a path will be used in link validation, but no further processing for this document will be performed. 
    */
  def addProvidedPaths (paths: Seq[Path]): InputTreeBuilder[F] = paths.foldLeft(this) {
    case (builder, path) => builder.addProvidedPath(path)
  }

  /** Adds the specified file filter to this input tree.
    * 
    * The filter will only be used for scanning directories when calling `addDirectory` on this builder,
    * not for any of the other methods.
    */
  def withFileFilter (filter: File => Boolean): InputTreeBuilder[F] = {
    val combinedFilter: File => Boolean = f => filter(f) || exclude(f)
    new InputTreeBuilder(combinedFilter, steps, fileRoots)
  }

  /** Merges this input tree with the specified tree, recursively.
    */
  def merge (other: InputTreeBuilder[F]): InputTreeBuilder[F] = 
    new InputTreeBuilder(f => other.exclude(f) || exclude(f), steps ++ other.steps, fileRoots ++ other.fileRoots)

  /** Merges this input tree with the specified tree, recursively.
    */
  def merge (other: InputTree[F]): InputTreeBuilder[F] = addStep { (_,_) => Kleisli { tree =>
    (tree ++ other).pure[F]
  }}
    

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

  /** Creates a new instance by applying the specified function to the root tree.
    */
  def modifyRoot (f: DocumentTreeRoot => DocumentTreeRoot): ParsedTree[F] = copy(root = f(root))

  /** Creates a new instance by applying the specified function to the nested tree.
    */
  def modifyTree (f: DocumentTree => DocumentTree): ParsedTree[F] = copy(root = root.modifyTree(f))
  
  /** Removes all static documents of this instance that match the specified filter.
    */
  def removeStaticDocuments (filter: Path => Boolean): ParsedTree[F] = copy(
    root = root.copy(staticDocuments = root.staticDocuments.filterNot(doc => filter(doc.path))),
    staticDocuments = staticDocuments.filterNot(doc => filter(doc.path))
  )
  
  /** Removes all static documents of this instance and replaces them with the specified alternatives.
    */
  def replaceStaticDocuments (newStaticDocs: Seq[BinaryInput[F]]): ParsedTree[F] = copy(
    root = root.copy(staticDocuments = newStaticDocs.map(doc => StaticDocument(doc.path, doc.formats))),
    staticDocuments = newStaticDocs
  )

  /** Adds the specified static documents to this instance.
    */
  def addStaticDocuments (newStaticDocs: Seq[BinaryInput[F]]): ParsedTree[F] = copy(
    root = root.copy(staticDocuments = root.staticDocuments ++ newStaticDocs.map(doc => StaticDocument(doc.path, doc.formats))),
    staticDocuments = staticDocuments ++ newStaticDocs
  )
  
}
