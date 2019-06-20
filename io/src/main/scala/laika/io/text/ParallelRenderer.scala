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

import cats.effect.{Async, ContextShift}
import cats.implicits._
import laika.api.Renderer
import laika.api.builder.OperationConfig
import laika.ast.DocumentTreeRoot
import laika.io.model.{BinaryInput, RenderedTreeRoot, TreeOutput}
import laika.io.ops.ParallelTextOutputOps
import laika.runtime.{RendererRuntime, Runtime}

/**
  * @author Jens Halm
  */
class ParallelRenderer[F[_]: Async: Runtime] (renderer: Renderer) {

  def from (input: DocumentTreeRoot): ParallelRenderer.OutputOps[F] =
    ParallelRenderer.OutputOps(renderer, input, Async[F].pure(Nil))

}

object ParallelRenderer {

  case class Builder (renderer: Renderer) {

    def build[F[_]: Async, G[_]](processingContext: ContextShift[F], blockingContext: ContextShift[F], parallelism: Int)
                                (implicit P: cats.Parallel[F, G]): ParallelRenderer[F] =
      new ParallelRenderer[F](renderer)(implicitly[Async[F]], Runtime.parallel(processingContext, blockingContext, parallelism))

    def build[F[_]: Async, G[_]](processingContext: ContextShift[F], blockingContext: ContextShift[F])
                                (implicit P: cats.Parallel[F, G]): ParallelRenderer[F] =
      build(processingContext, blockingContext, java.lang.Runtime.getRuntime.availableProcessors)

  }

  case class OutputOps[F[_]: Async: Runtime] (renderer: Renderer, input: DocumentTreeRoot, staticDocuments: F[Seq[BinaryInput]]) extends ParallelTextOutputOps[F] {

    val F: Async[F] = Async[F]

    type Result = Op[F]

    def copying (toCopy: F[Seq[BinaryInput]]): OutputOps[F] = {
      val combined = for { a <- staticDocuments; b <- toCopy } yield a ++ b
      copy(staticDocuments = combined)
    }

    def toOutput (output: F[TreeOutput]): Op[F] = Op[F](renderer, input, output, staticDocuments)

  }

  case class Op[F[_]: Async: Runtime] (renderer: Renderer, input: DocumentTreeRoot, output: F[TreeOutput], staticDocuments: F[Seq[BinaryInput]]) {

    val config: OperationConfig = renderer.config

    def render: F[RenderedTreeRoot] = RendererRuntime.run(this)

  }

}