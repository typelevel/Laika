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

import cats.Monad
import cats.data.Kleisli
import cats.effect.Async
import cats.implicits._
import laika.bundle.ExtensionBundle
import laika.io.binary
import laika.io.model.{ParsedTree, RenderedTreeRoot, InputTree}
import laika.io.text._
import laika.io.theme.Theme

/** Internal runtime for transform operations, for text and binary output as well
  * as parallel and sequential execution. 
  *
  *  @author Jens Halm
  */
object TransformerRuntime {
  
  private def themeWithBundlesOnly[F[_]: Monad] (theme: Theme[F]): Theme[F] = new Theme[F] {
    def inputs: F[InputTree[F]] = Monad[F].pure(InputTree.empty)
    def extensions: Seq[ExtensionBundle] = theme.extensions
    def treeTransformer: Kleisli[F, ParsedTree[F], ParsedTree[F]] = Kleisli(Monad[F].pure)
  }

  /** Process the specified transform operation for a single input document and 
    * a character output format.
    */
  def run[F[_]: Async: Runtime] (op: SequentialTransformer.Op[F]): F[String] = for {
    doc <- SequentialParser.Op(op.transformer.parser, op.input).parse
    res <- SequentialRenderer.Op(op.transformer.renderer, doc.content, doc.path, op.output).render
  } yield res

  /** Process the specified transform operation for an entire input tree and 
    * a character output format.
    */
  def run[F[_]: Async: Runtime] (op: ParallelTransformer.Op[F]): F[RenderedTreeRoot[F]] = for {
    tree       <- ParallelParser.Op(op.parsers, op.theme, op.input).parse
    mappedTree <- op.mapper.run(tree)
    res        <- ParallelRenderer.Op(op.renderer, themeWithBundlesOnly(op.theme), mappedTree.root, op.output, mappedTree.staticDocuments).render
  } yield res

  /** Process the specified transform operation for a single input document and a binary output format.
    */
  def run[F[_]: Async: Runtime] (op: binary.SequentialTransformer.Op[F]): F[Unit] = for {
    doc <- SequentialParser.Op(op.transformer.markupParser, op.input).parse
    res <- binary.SequentialRenderer.Op(op.transformer.renderer, doc.content, doc.path, op.output).render
  } yield res

  /** Process the specified transform operation for an entire input tree and a binary output format.
    */
  def run[F[_]: Async: Runtime] (op: binary.ParallelTransformer.Op[F]): F[Unit] = for {
    tree       <- ParallelParser.Op(op.parsers, op.theme, op.input).parse
    mappedTree <- op.mapper.run(tree)
    res        <- binary.ParallelRenderer.Op[F](op.renderer, themeWithBundlesOnly(op.theme), mappedTree.root, op.output, mappedTree.staticDocuments).render
  } yield res

}
