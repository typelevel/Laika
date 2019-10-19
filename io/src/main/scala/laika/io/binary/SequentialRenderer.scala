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

import cats.effect.{Async, Blocker, ContextShift}
import laika.api.builder.TwoPhaseRenderer
import laika.ast.{Document, Element, Path}
import laika.factory.BinaryPostProcessor
import laika.io.model.BinaryOutput
import laika.io.binary.SequentialRenderer.BinaryRenderer
import laika.io.ops.BinaryOutputOps
import laika.runtime.{RendererRuntime, Runtime}

/** Renderer for a single binary output document.
  *
  * @author Jens Halm
  */
class SequentialRenderer[F[_]: Async: Runtime] (renderer: BinaryRenderer) {

  /** Builder step that specifies the document to render.
    */
  def from (input: Document): SequentialRenderer.OutputOps[F] = from(input.content, input.path)

  /** Builder step that specifies a single AST element to render.
    * It will be interpreted as a `Document` containing the element as a root node.
    */
  def from (element: Element): SequentialRenderer.OutputOps[F] = from(element, Path.Root)

  /** Builder step that specifies a single AST element to render.
    * It will be interpreted as a `Document` containing the element as a root node.
    */
  def from (element: Element, path: Path): SequentialRenderer.OutputOps[F] =
    SequentialRenderer.OutputOps(renderer, element, path)

}

/** Builder API for constructing a rendering operation for a single binary output document.
  */
object SequentialRenderer {
  
  type BinaryRenderer = TwoPhaseRenderer[BinaryPostProcessor]

  /** Builder step that allows to specify the execution context
    * for blocking IO and CPU-bound tasks.
    */
  case class Builder (renderer: BinaryRenderer) {

    /** Builder step that allows to specify the execution context
      * for blocking IO and CPU-bound tasks.
      *
      * @param blocker the execution context for blocking IO
      */
    def build[F[_]: Async: ContextShift] (blocker: Blocker): SequentialRenderer[F] = 
      new SequentialRenderer[F](renderer)(implicitly[Async[F]], Runtime.sequential(blocker))

  }

  /** Builder step that allows to specify the output to render to.
    */
  case class OutputOps[F[_]: Async: Runtime] (renderer: BinaryRenderer, input: Element, path: Path) extends BinaryOutputOps[F] {

    val F: Async[F] = Async[F]

    type Result = Op[F]

    def toOutput (output: F[BinaryOutput]): Op[F] = Op[F](renderer, input, path, output)

  }

  /** Represents a rendering operation for a single binary output document.
    *
    * It can be run by invoking the `render` method which delegates to the library's
    * default runtime implementation or by developing a custom runner that performs
    * the rendering based on this operation's properties.
    */
  case class Op[F[_]: Async: Runtime] (renderer: BinaryRenderer, input: Element, path: Path, output: F[BinaryOutput]) {

    /** Performs the rendering operation based on the library's
      * default runtime implementation, suspended in the effect F.
      */
    def render: F[Unit] = RendererRuntime.run(this)

  }

}
