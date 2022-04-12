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

package laika.io.api

import cats.effect.{Resource, Sync}
import laika.api.Renderer
import laika.api.builder.OperationConfig
import laika.ast.DocumentTreeRoot
import laika.io.api.BinaryTreeRenderer.Builder
import laika.io.descriptor.RendererDescriptor
import laika.io.model.{BinaryInput,BinaryInput2, ParsedTree, RenderedTreeRoot, TreeOutput}
import laika.io.ops.TextOutputOps
import laika.io.runtime.{Batch, RendererRuntime}
import laika.theme.{Theme, ThemeProvider}
import cats.effect.kernel.Async

/** Renderer for a tree of output documents.
  *
  * @author Jens Halm
  */
class TreeRenderer[F[_]: Async: Batch] (renderer: Renderer, theme: Theme[F]) {

  /** Builder step that specifies the root of the document tree to render.
    */
  def from (input: DocumentTreeRoot): TreeRenderer.OutputOps[F] =
    TreeRenderer.OutputOps(renderer, theme, input, Nil)

  /** Builder step that specifies the root of the document tree to render
    * and the static documents to copy to the target (if it is file-system based).
    */
  def from (input: ParsedTree[F]): TreeRenderer.OutputOps[F] =
    TreeRenderer.OutputOps(renderer, theme, input.root, input.staticDocuments)

}

/** Builder API for constructing a rendering operation for a tree of output documents.
  */
object TreeRenderer {

  /** Builder step that allows to specify the execution context for blocking IO and CPU-bound tasks.
    */
  class Builder[F[_]: Async: Batch] (renderer: Renderer, theme: Resource[F, Theme[F]]) {

    /** Applies the specified theme to this renderer, overriding any previously specified themes.
      */
    def withTheme (theme: ThemeProvider): Builder[F] = new Builder(renderer, theme.build)

    /** Applies the specified theme to this renderer, overriding any previously specified themes.
      */
    def withTheme (theme: Theme[F]): Builder[F] = new Builder(renderer, Resource.pure[F, Theme[F]](theme))
    
    /** Final builder step that creates a parallel renderer.
      */
    def build: Resource[F, TreeRenderer[F]] = theme.map(new TreeRenderer[F](renderer, _))

  }

  /** Builder step that allows to specify the output to render to.
    */
  case class OutputOps[F[_]: Async: Batch] (renderer: Renderer,
                                           theme: Theme[F],
                                           input: DocumentTreeRoot,
                                           staticDocuments: Seq[BinaryInput[F]]) extends TextOutputOps[F] {

    val F: Async[F] = Async[F]

    type Result = Op[F]

    /** Copies the specified binary input to the output target, in addition to rendering the document tree.
      */
    def copying (toCopy: Seq[BinaryInput[F]]): OutputOps[F] = copy(staticDocuments = staticDocuments ++ toCopy)

    def toOutput (output: TreeOutput): Op[F] = Op[F](renderer, theme, input, output, staticDocuments)

  }

  /** Represents a rendering operation for a tree of documents.
    *
    * It can be run by invoking the `render` method which delegates to the library's default runtime implementation
    * or by developing a custom runner that performs the rendering based on this operation's properties.
    */
  case class Op[F[_]: Async: Batch] (renderer: Renderer,
                                    theme: Theme[F],
                                    input: DocumentTreeRoot,
                                    output: TreeOutput,
                                    staticDocuments: Seq[BinaryInput[F]] = Nil) {

    /** The configuration of the renderer.
      */
    val config: OperationConfig = renderer.config.withBundles(theme.extensions)

    /** Performs the rendering operation based on the library's default runtime implementation,
      * suspended in the effect F.
      */
    def render: F[RenderedTreeRoot[F]] = RendererRuntime.run(this)

    /** Provides a description of this operation, the renderers and extension bundles used,
      * as well as the output target.
      * This functionality is mostly intended for tooling support.
      */
    def describe: F[RendererDescriptor] = RendererDescriptor.create(this)
    
  }

  /** Represents a rendering operation for a tree of documents.
    *
    * It can be run by invoking the `render` method which delegates to the library's default runtime implementation
    * or by developing a custom runner that performs the rendering based on this operation's properties.
    */
  case class Op2[F[_]: Async: Batch] (renderer: Renderer,
                                    theme: Theme[F],
                                    input: DocumentTreeRoot,
                                    output: TreeOutput,
                                    staticDocuments: Seq[BinaryInput2[F]] = Nil) {

    /** The configuration of the renderer.
      */
    val config: OperationConfig = renderer.config.withBundles(theme.extensions)

    /** Performs the rendering operation based on the library's default runtime implementation,
      * suspended in the effect F.
      */
    def render: F[RenderedTreeRoot[F]] = RendererRuntime.run(this)

    /** Provides a description of this operation, the renderers and extension bundles used,
      * as well as the output target.
      * This functionality is mostly intended for tooling support.
      */
    def describe: F[RendererDescriptor] = RendererDescriptor.create(this)
    
  }

}
