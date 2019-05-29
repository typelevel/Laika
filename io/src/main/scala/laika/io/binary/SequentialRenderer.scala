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
import laika.api.builder.TwoPhaseRenderer
import laika.ast.{Document, Element, Path}
import laika.factory.BinaryPostProcessor
import laika.io.BinaryOutput
import laika.io.binary.SequentialRenderer.BinaryRenderer

/**
  * @author Jens Halm
  */
class SequentialRenderer[F[_]: Async] (renderer: BinaryRenderer) {

  def from (input: Document): SequentialRenderer.OutputOps[F] = from(input.content, input.path)

  def from (element: Element): SequentialRenderer.OutputOps[F] = from(element, Path.Root)

  def from (element: Element, path: Path): SequentialRenderer.OutputOps[F] =
    SequentialRenderer.OutputOps(renderer, element, path)

}

object SequentialRenderer {
  
  type BinaryRenderer = TwoPhaseRenderer[BinaryPostProcessor]

  case class Builder (renderer: BinaryRenderer) {

    def build[F[_]: Async]: SequentialRenderer[F] = new SequentialRenderer[F](renderer)

  }

  case class OutputOps[F[_]: Async] (renderer: BinaryRenderer, input: Element, path: Path) extends BinaryOutputOps[F] {

    val F: Async[F] = Async[F]

    type Result = Op[F]

    def toOutput (output: F[BinaryOutput]): Op[F] = Op[F](renderer, input, path, output)

  }

  case class Op[F[_]: Async] (renderer: BinaryRenderer, input: Element, path: Path, output: F[BinaryOutput]) {

    def render: F[Unit] = ???

  }

}
