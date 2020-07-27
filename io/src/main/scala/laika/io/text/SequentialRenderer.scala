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

package laika.io.text

import cats.effect.Sync
import laika.api.Renderer
import laika.ast.{Document, Element, Path}
import laika.io.descriptor.{ParserDescriptor, RendererDescriptor}
import laika.io.model.TextOutput
import laika.io.ops.SequentialTextOutputOps
import laika.io.runtime.{RendererRuntime, Runtime}

/** Renderer for a single output document.
  *
  * @author Jens Halm
  */
class SequentialRenderer[F[_]: Sync: Runtime] (renderer: Renderer) {

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

/** Builder API for constructing a render operation for a single output document.
  */
object SequentialRenderer {

  /** Builder step that allows to specify the execution context
    * for blocking IO and CPU-bound tasks.
    */
  case class Builder[F[_]: Sync: Runtime] (renderer: Renderer) {

    /** Final builder step that creates a sequential renderer.
      */
    def build: SequentialRenderer[F] = new SequentialRenderer[F](renderer)

  }

  /** Builder step that allows to specify the output to render to.
    */
  case class OutputOps[F[_]: Sync: Runtime] (renderer: Renderer, input: Element, path: Path) extends SequentialTextOutputOps[F] {

    val F: Sync[F] = Sync[F]

    type Result = Op[F]

    def toOutput (output: TextOutput[F]): Op[F] = Op[F](renderer, input, path, output)

  }

  /** Represents a rendering operation for a single output document.
    *
    * It can be run by invoking the `render` method which delegates to the library's
    * default runtime implementation or by developing a custom runner that performs
    * the rendering based on this operation's properties.
    */
  case class Op[F[_]: Sync: Runtime] (renderer: Renderer, input: Element, path: Path, output: TextOutput[F]) {

    /** Performs the rendering operation based on the library's
      * default runtime implementation, suspended in the effect F.
      */
    def render: F[String] = RendererRuntime.run(this)

    /** Provides a description of this operation, the renderers
      * and extension bundles used, as well as the output target.
      * This functionality is mostly intended for tooling support.
      */
    def describe: F[RendererDescriptor] = RendererDescriptor.create(this)
    
  }

}
