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

package laika.io.runtime

import cats.effect.Async
import cats.implicits.*
import laika.api.bundle.ExtensionBundle
import laika.api.format.Format
import laika.io.api.{
  BinaryTreeRenderer,
  BinaryTreeTransformer,
  TreeParser,
  TreeRenderer,
  TreeTransformer
}
import laika.io.descriptor.ThemeDescriptor
import laika.io.model.{
  DirectoryInput,
  DirectoryOutput,
  FileFilter,
  InputTree,
  RenderedTreeRoot,
  TreeOutput
}
import laika.theme.Theme
import laika.theme.Theme.TreeProcessor

/** Internal runtime for transform operations, for text and binary output as well
  * as parallel and sequential execution.
  *
  *  @author Jens Halm
  */
private[io] object TransformerRuntime {

  private def themeWithoutInputs[F[_]](theme: Theme[F]): Theme[F] = new Theme[F] {
    def descriptor: ThemeDescriptor               = theme.descriptor
    def inputs: InputTree[F]                      = InputTree.empty
    def extensions: Seq[ExtensionBundle]          = theme.extensions
    def treeProcessor: Format => TreeProcessor[F] = theme.treeProcessor
  }

  private def fileFilterFor(output: TreeOutput): FileFilter = output match {
    case DirectoryOutput(directory, _) => DirectoryInput.filterDirectory(directory)
    case _                             => FileFilter.lift(_ => false)
  }

  /** Process the specified transform operation for an entire input tree and a character output format.
    */
  def run[F[_]: Async: Batch](op: TreeTransformer.Op[F]): F[RenderedTreeRoot[F]] = for {
    tree       <- new TreeParser.Op(
      op.parsers,
      op.theme,
      op.input.withFileFilter(fileFilterFor(op.output))
    ).parse
    mappedTree <- op.mapper.run(tree)
    res        <- new TreeRenderer.Op(
      op.renderer,
      themeWithoutInputs(op.theme),
      mappedTree.root,
      op.output,
      mappedTree.staticDocuments
    ).render
  } yield res

  /** Process the specified transform operation for an entire input tree and a binary output format.
    */
  def run[F[_]: Async: Batch](op: BinaryTreeTransformer.Op[F]): F[Unit] = for {
    tree       <- new TreeParser.Op(op.parsers, op.theme, op.input).parse
    mappedTree <- op.mapper.run(tree)
    res        <- new BinaryTreeRenderer.Op[F](
      op.renderer,
      themeWithoutInputs(op.theme),
      mappedTree.root,
      op.output,
      mappedTree.staticDocuments
    ).render
  } yield res

}
