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
import laika.ast.{ DocumentType, Path, StaticDocument }
import laika.io.runtime.TreeResultBuilder.ParserResult

/** A (virtual) tree of input documents, either obtained from scanning a directory recursively or
  * constructed programmatically (or a mix of both).
  *
  * Even though the documents are specified as a flat sequence, they logically form a tree based
  * on their virtual path.
  */
case class InputTree[F[_]](
    textInputs: Seq[TextInput[F]] = Nil,
    binaryInputs: Seq[BinaryInput[F]] = Nil,
    parsedResults: Seq[ParserResult] = Nil,
    providedPaths: Seq[StaticDocument] = Nil,
    sourcePaths: Seq[FilePath] = Nil
) {

  /** A collection of all paths in this input tree, which may contain duplicates.
    */
  lazy val allPaths: Seq[Path] =
    textInputs.map(_.path) ++ binaryInputs.map(_.path) ++ parsedResults.map(_.path)

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
  def overrideWith(overrides: InputTree[F]): InputTree[F] =
    remove(overrides.allPaths.toSet) ++ overrides

  def + (textInput: TextInput[F]): InputTree[F] = copy(textInputs = textInputs :+ textInput)

  def + (binaryInput: BinaryInput[F]): InputTree[F] =
    copy(binaryInputs = binaryInputs :+ binaryInput)

  def + (parsedResult: ParserResult): InputTree[F] =
    copy(parsedResults = parsedResults :+ parsedResult)

  def + (providedPath: StaticDocument): InputTree[F] =
    copy(providedPaths = providedPaths :+ providedPath)

  /** Returns a new input tree with all inputs matching the provided exclusions removed from this tree.
    */
  def remove(paths: Set[Path]): InputTree[F] = {
    InputTree(
      textInputs = textInputs.filterNot(in => paths.contains(in.path)),
      binaryInputs = binaryInputs.filterNot(in => paths.contains(in.path)),
      parsedResults = parsedResults.filterNot(in => paths.contains(in.path)),
      sourcePaths = sourcePaths
    )
  }

}

/** Factory methods for creating `InputTreeBuilder` instances.
  */
object InputTree {

  private[laika] case class BuilderContext[F[_]](
      exclude: FileFilter,
      docTypeMatcher: Path => DocumentType,
      input: InputTree[F],
      missingDirectories: Seq[FilePath] = Nil
  ) {

    def modifyTree(f: InputTree[F] => InputTree[F]): BuilderContext[F] = copy(input = f(input))

    def withMissingDirectory(dir: FilePath): BuilderContext[F] =
      copy(missingDirectories = missingDirectories :+ dir)

  }

  private[laika] type BuilderStep[F[_]] = Kleisli[F, BuilderContext[F], BuilderContext[F]]

  /** Creates a new, empty InputTreeBuilder will the specified exclusion filter.
    * The filter will only be used for scanning directories when calling `addDirectory` on the builder,
    * not for any of the other methods.
    */
  def apply[F[_]: Async](exclude: FileFilter): InputTreeBuilder[F] =
    new InputTreeBuilder(exclude, Vector.empty, Vector.empty)

  /** Creates a new, empty InputTreeBuilder.
    */
  def apply[F[_]: Async]: InputTreeBuilder[F] =
    new InputTreeBuilder(DirectoryInput.hiddenFileFilter, Vector.empty, Vector.empty)

  /** An empty input tree.
    */
  def empty[F[_]]: InputTree[F] = InputTree(Nil, Nil, Nil)
}
