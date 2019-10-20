/*
 * Copyright 2012-2019 the original author or authors.
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

package laika.io.text

import cats.Parallel
import cats.effect.{Async, Blocker, ContextShift}
import cats.implicits._
import laika.api.Renderer
import laika.api.builder.OperationConfig
import laika.ast.DocumentTreeRoot
import laika.io.model.{BinaryInput, RenderedTreeRoot, TreeOutput}
import laika.io.ops.ParallelTextOutputOps
import laika.runtime.{RendererRuntime, Runtime}

/** Renderer for a tree of output documents.
  *
  * @author Jens Halm
  */
class ParallelRenderer[F[_]: Async: Runtime] (renderer: Renderer) {

  /** Builder step that specifies the root of the document tree to render.
    */
  def from (input: DocumentTreeRoot): ParallelRenderer.OutputOps[F] =
    ParallelRenderer.OutputOps(renderer, input, Async[F].pure(Nil))

}

/** Builder API for constructing a rendering operation for a tree of output documents.
  */
object ParallelRenderer {

  /** Builder step that allows to specify the execution context
    * for blocking IO and CPU-bound tasks.
    */
  case class Builder[F[_]: Async: Runtime] (renderer: Renderer) {

    /** Final builder step that creates a parallel renderer.
      */
    def build: ParallelRenderer[F] = new ParallelRenderer[F](renderer)

  }

  /** Builder step that allows to specify the output to render to.
    */
  case class OutputOps[F[_]: Async: Runtime] (renderer: Renderer, input: DocumentTreeRoot, staticDocuments: F[Seq[BinaryInput]]) extends ParallelTextOutputOps[F] {

    val F: Async[F] = Async[F]

    type Result = Op[F]

    /** Copies the specified binary input to the output target,
      * in addition to rendering the document tree.
      */
    def copying (toCopy: F[Seq[BinaryInput]]): OutputOps[F] = {
      val combined = for { a <- staticDocuments; b <- toCopy } yield a ++ b
      copy(staticDocuments = combined)
    }

    def toOutput (output: F[TreeOutput]): Op[F] = Op[F](renderer, input, output, staticDocuments)

  }

  /** Represents a rendering operation for a tree of documents.
    *
    * It can be run by invoking the `render` method which delegates to the library's
    * default runtime implementation or by developing a custom runner that performs
    * the rendering based on this operation's properties.
    */
  case class Op[F[_]: Async: Runtime] (renderer: Renderer, input: DocumentTreeRoot, output: F[TreeOutput], staticDocuments: F[Seq[BinaryInput]]) {

    /** The configuration of the renderer.
      */
    val config: OperationConfig = renderer.config

    /** Performs the rendering operation based on the library's
      * default runtime implementation, suspended in the effect F.
      */
    def render: F[RenderedTreeRoot] = RendererRuntime.run(this)

  }

}