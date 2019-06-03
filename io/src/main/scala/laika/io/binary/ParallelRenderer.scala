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

package laika.io.binary

import cats.effect.Async
import laika.api.builder.{OperationConfig, TwoPhaseRenderer}
import laika.ast.DocumentTreeRoot
import laika.factory.BinaryPostProcessor
import laika.io.BinaryOutput
import laika.io.binary.ParallelRenderer.BinaryRenderer
import laika.runtime.RendererRuntime

/**
  * @author Jens Halm
  */
class ParallelRenderer[F[_]: Async] (renderer: BinaryRenderer) {

  def from (input: DocumentTreeRoot): ParallelRenderer.OutputOps[F] =
    ParallelRenderer.OutputOps(renderer, input)

}

object ParallelRenderer {

  type BinaryRenderer = TwoPhaseRenderer[BinaryPostProcessor]

  case class Builder (renderer: BinaryRenderer) {

    def build[F[_]: Async]: ParallelRenderer[F] = new ParallelRenderer[F](renderer)

  }

  case class OutputOps[F[_]: Async] (renderer: BinaryRenderer, input: DocumentTreeRoot) extends BinaryOutputOps[F] {

    val F: Async[F] = Async[F]

    type Result = Op[F]

    def toOutput (output: F[BinaryOutput]): Op[F] = Op[F](renderer, input, output)

  }

  case class Op[F[_]: Async] (renderer: BinaryRenderer, input: DocumentTreeRoot, output: F[BinaryOutput]) {

    val config: OperationConfig = renderer.interimRenderer.config

    def render: F[Unit] = RendererRuntime.run(this)

  }

}