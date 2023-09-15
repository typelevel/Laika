/*
 * Copyright 2012-2022 the original author or authors.
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
import cats.effect.Async
import fs2.io.file.Files
import laika.api.bundle.{ DocumentTypeMatcher, Precedence }
import laika.api.config.Config
import laika.ast.Path.Root
import laika.ast.styles.{ StyleDeclaration, StyleDeclarationSet }
import laika.ast.{
  Document,
  DocumentTreeBuilder,
  DocumentType,
  Path,
  StaticDocument,
  TemplateDocument,
  TextDocumentType
}
import laika.io.descriptor.TreeInputDescriptor
import laika.io.errors.*
import laika.io.model.InputTree.{ BuilderContext, BuilderStep }
import laika.io.runtime.DirectoryScanner

import java.io.InputStream
import scala.io.Codec
import scala.reflect.ClassTag

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
class InputTreeBuilder[F[_]] private[model] (
    private[laika] val exclude: FileFilter,
    private[model] val steps: Vector[BuilderStep[F]],
    private[laika] val fileRoots: Vector[FilePath]
)(implicit F: Async[F]) {

  import cats.syntax.all._
  import DocumentTreeBuilder._

  private def addStep(step: BuilderStep[F]): InputTreeBuilder[F] =
    addStep(None)(step)

  private def addStep(newFileRoot: Option[FilePath])(step: BuilderStep[F]): InputTreeBuilder[F] =
    new InputTreeBuilder(
      exclude,
      steps = steps :+ step,
      newFileRoot.fold(fileRoots)(fileRoots :+ _)
    )

  private def addStep(path: Path, newFileRoot: Option[FilePath] = None)(
      f: PartialFunction[DocumentType, InputTree[F] => InputTree[F]]
  ): InputTreeBuilder[F] =
    addStep(newFileRoot) {
      Kleisli { ctx =>
        val m = f.applyOrElse[DocumentType, InputTree[F] => InputTree[F]](
          ctx.docTypeMatcher(path),
          _ => identity
        )
        ctx.modifyTree(m).pure[F]
      }
    }

  private def addBuilderPart(part: DocumentTreeBuilder.BuilderPart): InputTreeBuilder[F] = addStep {
    Kleisli[F, BuilderContext[F], BuilderContext[F]](_.modifyTree(_ + part).pure[F])
  }

  /** Adds the specified directories to the input tree, merging them all into a single virtual root, recursively.
    */
  def addDirectories(dirs: Seq[FilePath])(implicit codec: Codec): InputTreeBuilder[F] =
    dirs.foldLeft(this) { case (builder, dir) =>
      builder.addDirectory(dir)
    }

  /** Adds the specified directories to the input tree, placing it in the virtual root.
    */
  def addDirectory(name: String)(implicit codec: Codec): InputTreeBuilder[F] =
    addDirectory(FilePath.parse(name), Root)

  /** Adds the specified directories to the input tree, placing it at the specified mount point in the virtual tree.
    */
  def addDirectory(name: String, mountPoint: Path)(implicit codec: Codec): InputTreeBuilder[F] =
    addDirectory(FilePath.parse(name), mountPoint)

  /** Adds the specified directories to the input tree, placing it in the virtual root.
    */
  def addDirectory(dir: FilePath)(implicit codec: Codec): InputTreeBuilder[F] =
    addDirectory(dir, Root)

  /** Adds the specified directories to the input tree, placing it at the specified mount point in the virtual tree.
    */
  def addDirectory(dir: FilePath, mountPoint: Path)(implicit codec: Codec): InputTreeBuilder[F] =
    addStep(Some(dir)) {
      Kleisli { ctx =>
        Files.forAsync[F].isDirectory(dir.toFS2Path).ifM(
          DirectoryScanner
            .scanDirectories[F](
              new DirectoryInput(Seq(dir), codec, ctx.docTypeMatcher, ctx.exclude, mountPoint)
            )
            .map(res => ctx.modifyTree(_ ++ res)),
          ctx.withMissingDirectory(dir).pure[F]
        )
      }
    }

  /** Adds the specified file to the input tree, placing it at the specified mount point in the virtual tree.
    *
    * The content type of the stream will be determined by the suffix of the virtual path, e.g.
    * `doc.md` would be passed to the markup parser, `doc.template.html` to the template parser, and so on.
    */
  def addFile(name: String, mountPoint: Path)(implicit codec: Codec): InputTreeBuilder[F] =
    addFile(FilePath.parse(name), mountPoint)

  /** Adds the specified file to the input tree, placing it at the specified mount point in the virtual tree.
    *
    * The content type of the stream will be determined by the suffix of the virtual path, e.g.
    * `doc.md` would be passed to the markup parser, `doc.template.html` to the template parser, and so on.
    */
  def addFile(file: FilePath, mountPoint: Path)(implicit codec: Codec): InputTreeBuilder[F] =
    addStep(mountPoint, Some(file)) {
      case DocumentType.Static(formats) =>
        _.addBinaryInput(BinaryInput.fromFile(file, mountPoint, formats))
      case docType: TextDocumentType    =>
        _.addTextInput(TextInput.fromFile(file, mountPoint, docType))
    }

  /** Adds the specified classpath resource to the input tree, placing it at the specified mount point in the virtual tree.
    * The specified name must be compatible with Java's `Class.getResource`.
    * Relative paths will be interpreted as relative to the package name of the referenced class,
    * with all `.` replaced by `/`.
    *
    * The content type of the stream will be determined by the suffix of the virtual path, e.g.
    * `doc.md` would be passed to the markup parser, `doc.template.html` to the template parser, and so on.
    */
  def addClassResource[T: ClassTag](name: String, mountPoint: Path)(implicit
      codec: Codec
  ): InputTreeBuilder[F] =
    addStep(mountPoint) {
      case DocumentType.Static(formats) =>
        _.addBinaryInput(BinaryInput.fromClassResource[F, T](name, mountPoint, formats))
      case docType: TextDocumentType    =>
        _.addTextInput(TextInput.fromClassResource[F, T](name, mountPoint, docType))
    }

  /** Adds the specified classpath resource to the input tree, placing it at the specified mount point in the virtual tree.
    * The specified name must be compatible with Java's `ClassLoader.getResource`.
    * The optional `ClassLoader` argument can be used to ensure the resource is found in an application or plugin
    * that uses multiple class loaders.
    * If the call site is in the same module as the classpath resource, simply using `getClass.getClassLoader` should suffice.
    *
    * The content type of the stream will be determined by the suffix of the virtual path, e.g.
    * `doc.md` would be passed to the markup parser, `doc.template.html` to the template parser, and so on.
    */
  def addClassLoaderResource(
      name: String,
      mountPoint: Path,
      classLoader: ClassLoader = getClass.getClassLoader
  )(implicit codec: Codec): InputTreeBuilder[F] =
    addStep(mountPoint) {
      case DocumentType.Static(formats) =>
        _.addBinaryInput(
          BinaryInput.fromClassLoaderResource(name, mountPoint, formats, classLoader)
        )
      case docType: TextDocumentType    =>
        _.addTextInput(TextInput.fromClassLoaderResource(name, mountPoint, docType, classLoader))
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
  def addInputStream(stream: F[InputStream], mountPoint: Path, autoClose: Boolean = true)(implicit
      codec: Codec
  ): InputTreeBuilder[F] =
    addStep(mountPoint) {
      case DocumentType.Static(formats) =>
        _.addBinaryInput(BinaryInput.fromInputStream(stream, mountPoint, autoClose, formats))
      case docType: TextDocumentType    =>
        _.addTextInput(TextInput.fromInputStream(stream, mountPoint, docType, autoClose))
    }

  /** Adds the specified input stream to the input tree, placing it at the specified mount point in the virtual tree.
    *
    * The content type of the stream will be determined by the suffix of the virtual path, e.g.
    * `doc.md` would be passed to the markup parser, `doc.template.html` to the template parser, and so on.
    *
    * If the content type is text-based the stream will be decoded as UTF-8.
    * In case a different codec is required, use `addTextStream` and decode the text beforehand.
    */
  def addBinaryStream(stream: fs2.Stream[F, Byte], mountPoint: Path): InputTreeBuilder[F] =
    addStep(mountPoint) {
      case DocumentType.Static(formats) =>
        _.addBinaryInput(BinaryInput.fromStream(stream, mountPoint, formats))
      case docType: TextDocumentType    =>
        _.addTextInput(TextInput.fromBinaryStream(stream, mountPoint, docType))
    }

  /** Adds the specified input stream to the input tree, placing it at the specified mount point in the virtual tree.
    *
    * The content type of the stream will be determined by the suffix of the virtual path, e.g.
    * `doc.md` would be passed to the markup parser, `doc.template.html` to the template parser, and so on.
    *
    * If the target content type is binary the stream will be encoded as UTF-8.
    * In case a different codec is required, use `addBinaryStream` and encode the text beforehand.
    */
  def addTextStream(stream: fs2.Stream[F, String], mountPoint: Path): InputTreeBuilder[F] =
    addStep(mountPoint) {
      case DocumentType.Static(formats) =>
        _.addBinaryInput(
          BinaryInput.fromStream(stream.through(fs2.text.utf8.encode), mountPoint, formats)
        )
      case docType: TextDocumentType    =>
        _.addTextInput(TextInput.fromTextStream(stream, mountPoint, docType))
    }

  /** Adds the specified string resource to the input tree, placing it at the specified mount point in the virtual tree.
    *
    * The content type of the stream will be determined by the suffix of the virtual path, e.g.
    * * `doc.md` would be passed to the markup parser, `doc.template.html` to the template parser, and so on.
    */
  def addString(input: String, mountPoint: Path): InputTreeBuilder[F] =
    addStep(mountPoint) {
      case DocumentType.Static(formats) =>
        _.addBinaryInput(BinaryInput.fromString[F](input, mountPoint, formats))
      case docType: TextDocumentType    =>
        _.addTextInput(TextInput.fromString[F](input, mountPoint, docType))
    }

  /** Adds the specified document AST to the input tree, by-passing the parsing step.
    *
    * In some cases when generating input on the fly, it might be more convenient or more type-safe to construct
    * the AST directly than to generate the text markup as input for the parser.
    */
  def addDocument(doc: Document): InputTreeBuilder[F] = addBuilderPart(DocumentPart(doc))

  /** Adds the specified template AST to the input tree, by-passing the parsing step.
    *
    * In some cases when generating input on the fly, it might be more convenient or more type-safe to construct
    * the AST directly than to generate the template as a string as input for the template parser.
    */
  def addTemplate(doc: TemplateDocument): InputTreeBuilder[F] = addBuilderPart(TemplatePart(doc))

  /** Adds the specified configuration instance and assigns it to the specified tree path in a way
    * that is equivalent to having a HOCON file called `directory.conf` in that directory.
    */
  def addConfig(config: Config, treePath: Path): InputTreeBuilder[F] = addBuilderPart(
    ConfigPart(treePath / "directory.conf", config)
  )

  /** Adds the specified styles for PDF to the input tree.
    * These type of style declarations are only used in the context of Laika's "CSS for PDF" support
    * which works slightly differently than web CSS as PDF generation in Laika is not based on interim HTML results.
    */
  def addStyles(
      styles: Set[StyleDeclaration],
      path: Path,
      precedence: Precedence = Precedence.High
  ): InputTreeBuilder[F] =
    addBuilderPart(StylePart(StyleDeclarationSet(Set(path), styles, precedence), "fo"))

  /** Adds a path to the input tree that represents a document getting processed by some external tool.
    * Such a path will be used in link validation, but no further processing for this document will be performed.
    */
  def addProvidedPath(path: Path): InputTreeBuilder[F] = addStep(path) {
    case DocumentType.Static(formats) => _.addProvidedPath(StaticDocument(path, formats))
    case _                            => _.addProvidedPath(StaticDocument(path))
  }

  /** Adds the specified paths to the input tree that represent documents getting processed by some external tool.
    * Such a path will be used in link validation, but no further processing for this document will be performed.
    */
  def addProvidedPaths(paths: Seq[Path]): InputTreeBuilder[F] = paths.foldLeft(this) {
    case (builder, path) => builder.addProvidedPath(path)
  }

  /** Adds the specified file filter to this input tree.
    *
    * The filter will only be used for scanning directories when calling `addDirectory` on this builder,
    * not for any of the other methods.
    */
  def withFileFilter(newFilter: FileFilter): InputTreeBuilder[F] = {
    new InputTreeBuilder(exclude.orElse(newFilter), steps, fileRoots)
  }

  /** Merges this input tree with the specified tree, recursively.
    */
  def merge(other: InputTreeBuilder[F]): InputTreeBuilder[F] =
    new InputTreeBuilder(
      exclude.orElse(other.exclude),
      steps ++ other.steps,
      fileRoots ++ other.fileRoots
    )

  /** Merges this input tree with the specified tree, recursively.
    */
  def merge(other: InputTree[F]): InputTreeBuilder[F] = addStep {
    Kleisli { (ctx: BuilderContext[F]) =>
      ctx.modifyTree(_ ++ other).pure[F]
    }
  }

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
  def build(docTypeMatcher: Path => DocumentType): F[InputTree[F]] = {
    val ctx = BuilderContext(exclude, docTypeMatcher, InputTree.empty[F])
    build(ctx).flatMap { res =>
      res.missingDirectories match {
        case Nil     => res.input.withSourcePaths(fileRoots).pure[F]
        case missing => F.raiseError(ParserErrors(missing.map(MissingDirectory(_)).toSet))
      }
    }
  }

  /** Provides a description of this input tree.
    * Input directories will be scanned or reported in case they don't exist.
    * Documents added to this builder individually will be listed without additional checks.
    *
    * This functionality is mostly intended for tooling support.
    */
  def describe(docTypeMatcher: Path => DocumentType): F[TreeInputDescriptor] = {
    val ctx = BuilderContext(exclude, docTypeMatcher, InputTree.empty[F])
    build(ctx).map { res =>
      val validSourcePaths = fileRoots.diff(res.missingDirectories)
      TreeInputDescriptor.create(
        res.input.withSourcePaths(validSourcePaths),
        res.missingDirectories
      )
    }
  }

  private def build(ctx: BuilderContext[F]): F[BuilderContext[F]] =
    steps
      .reduceLeftOption(_ andThen _)
      .fold(ctx.pure[F])(_.run(ctx))

}
