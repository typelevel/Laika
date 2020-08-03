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

import java.io.File

import cats.Monad
import cats.data.Kleisli
import cats.effect.Sync
import cats.implicits._
import laika.bundle.ExtensionBundle
import laika.factory.Format
import laika.io.api.{BinaryTreeRenderer, BinaryTreeTransformer, TreeParser, TreeRenderer, TreeTransformer}
import laika.io.model.{DirectoryInput, DirectoryOutput, InputTree, ParsedTree, RenderedTreeRoot, TreeOutput}
import laika.io.theme.Theme

/** Internal runtime for transform operations, for text and binary output as well
  * as parallel and sequential execution. 
  *
  *  @author Jens Halm
  */
object TransformerRuntime {
  
  private def themeWithoutInputs[F[_]: Monad] (theme: Theme[F]): Theme[F] = new Theme[F] {
    def inputs: F[InputTree[F]] = Monad[F].pure(InputTree.empty)
    def extensions: Seq[ExtensionBundle] = theme.extensions
    def treeProcessor: PartialFunction[Format, Kleisli[F, ParsedTree[F], ParsedTree[F]]] = theme.treeProcessor
  }
  
  private def fileFilterFor (output: TreeOutput): File => Boolean = output match {
    case DirectoryOutput(directory, _) => DirectoryInput.filter(directory)
    case _ => _ => false
  } 

  /** Process the specified transform operation for an entire input tree and a character output format.
    */
  def run[F[_]: Sync: Runtime] (op: TreeTransformer.Op[F]): F[RenderedTreeRoot[F]] = for {
    tree       <- TreeParser.Op(op.parsers, op.theme, op.input.withFileFilter(fileFilterFor(op.output))).parse
    mappedTree <- op.mapper.run(tree)
    res        <- TreeRenderer.Op(op.renderer, themeWithoutInputs(op.theme), mappedTree.root, op.output, mappedTree.staticDocuments).render
  } yield res

  /** Process the specified transform operation for an entire input tree and a binary output format.
    */
  def run[F[_]: Sync: Runtime] (op: BinaryTreeTransformer.Op[F]): F[Unit] = for {
    tree       <- TreeParser.Op(op.parsers, op.theme, op.input).parse
    mappedTree <- op.mapper.run(tree)
    res        <- BinaryTreeRenderer.Op[F](op.renderer, themeWithoutInputs(op.theme), mappedTree.root, op.output, mappedTree.staticDocuments).render
  } yield res

}
