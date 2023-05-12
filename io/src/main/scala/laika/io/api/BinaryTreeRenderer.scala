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

import cats.effect.{ Async, Resource }
import laika.api.Renderer
import laika.api.builder.{ OperationConfig, RendererBuilder }
import laika.ast.DocumentTreeRoot
import laika.factory.{ BinaryPostProcessor, BinaryPostProcessorBuilder, TwoPhaseRenderFormat }
import laika.io.api.BinaryTreeRenderer.BinaryRenderer
import laika.io.descriptor.RendererDescriptor
import laika.io.model.{ BinaryInput, BinaryOutput, ParsedTree }
import laika.io.ops.BinaryOutputOps
import laika.io.runtime.{ Batch, RendererRuntime }
import laika.theme.{ Theme, ThemeProvider }

/** Renderer that merges a tree of input documents to a single binary output document.
  *
  * @author Jens Halm
  */
class BinaryTreeRenderer[F[_]: Async: Batch](renderer: BinaryRenderer[F], theme: Theme[F]) {

  /** Builder step that specifies the root of the document tree to render.
    */
  def from(input: DocumentTreeRoot): BinaryTreeRenderer.OutputOps[F] =
    BinaryTreeRenderer.OutputOps(renderer, theme, input, Nil)

  /** Builder step that specifies the root of the document tree to render
    * and the static documents to copy to the target (if it is file-system based).
    */
  def from(input: ParsedTree[F]): BinaryTreeRenderer.OutputOps[F] =
    BinaryTreeRenderer.OutputOps(renderer, theme, input.root, input.staticDocuments)

}

/** Builder API for constructing a rendering operation for a tree of binary output documents.
  */
object BinaryTreeRenderer {

  /** A renderer that operates with two phases, producing an interim result.
    *
    * Examples for such renderers are EPUB (with XHTML as the interim format)
    * and PDF (with XSL-FO as the interim format).
    *
    * This instance does not come with its own runtime. Instead its need to be passed
    * to a builder API in laika-io that knows how to execute such an operation.
    *
    * @param interimRenderer the renderer for the 1st phase, producing the interim result
    * @param prepareTree a hook with which the interim result can be modified before it gets
    *                    passed to the post processor
    * @param postProcessor the processor taking the interim result and producing the final
    *                      result, the implementing type may vary from format to format
    * @param description short string describing the output format for tooling and logging
    */
  case class BinaryRenderer[F[_]: Async](
      interimRenderer: Renderer,
      prepareTree: DocumentTreeRoot => Either[Throwable, DocumentTreeRoot],
      postProcessor: BinaryPostProcessor[F],
      description: String
  )

  type BinaryRenderFormat = TwoPhaseRenderFormat[_, BinaryPostProcessorBuilder]

  private[laika] def buildRenderer[F[_]: Async](
      format: BinaryRenderFormat,
      config: OperationConfig,
      theme: Theme[F]
  ): Resource[F, BinaryRenderer[F]] = {
    val combinedConfig = config.withBundles(theme.extensions)
    format.postProcessor.build(combinedConfig.baseConfig, theme).map { pp =>
      val targetRenderer =
        new RendererBuilder(format.interimFormat, combinedConfig).build.skipRewritePhase
      BinaryRenderer(targetRenderer, format.prepareTree, pp, format.description)
    }
  }

  /** Builder step that allows to specify the execution context for blocking IO and CPU-bound tasks.
    */
  class Builder[F[_]: Async: Batch](
      format: BinaryRenderFormat,
      config: OperationConfig,
      theme: Resource[F, Theme[F]]
  ) {

    /** Applies the specified theme to this renderer, overriding any previously specified themes.
      */
    def withTheme(theme: ThemeProvider): Builder[F] = new Builder(format, config, theme.build)

    /** Applies the specified theme to this renderer, overriding any previously specified themes.
      */
    def withTheme(theme: Theme[F]): Builder[F] =
      new Builder(format, config, Resource.pure[F, Theme[F]](theme))

    /** Final builder step that creates a parallel renderer for binary output.
      */
    def build: Resource[F, BinaryTreeRenderer[F]] = for {
      initializedTheme    <- theme
      initializedRenderer <- buildRenderer(format, config, initializedTheme)
    } yield new BinaryTreeRenderer[F](initializedRenderer, initializedTheme)

  }

  /** Builder step that allows to specify the output to render to.
    */
  case class OutputOps[F[_]: Async: Batch](
      renderer: BinaryRenderer[F],
      theme: Theme[F],
      input: DocumentTreeRoot,
      staticDocuments: Seq[BinaryInput[F]]
  ) extends BinaryOutputOps[F] {

    val F: Async[F] = Async[F]

    type Result = Op[F]

    /** Copies the specified binary input to the output target,
      * in addition to rendering the document tree.
      */
    def copying(toCopy: Seq[BinaryInput[F]]): OutputOps[F] =
      copy(staticDocuments = staticDocuments ++ toCopy)

    def toOutput(output: BinaryOutput[F]): Op[F] =
      Op[F](renderer, theme, input, output, staticDocuments)

  }

  /** Represents a rendering operation for a tree of documents merged into a single binary output document.
    *
    * It can be run by invoking the `render` method which delegates to the library's
    * default runtime implementation or by developing a custom runner that performs
    * the rendering based on this operation's properties.
    */
  case class Op[F[_]: Async: Batch](
      renderer: BinaryRenderer[F],
      theme: Theme[F],
      input: DocumentTreeRoot,
      output: BinaryOutput[F],
      staticDocuments: Seq[BinaryInput[F]] = Nil
  ) {

    /** The configuration of the renderer for the interim format.
      */
    val config: OperationConfig = renderer.interimRenderer.config

    /** Performs the rendering operation based on the library's
      * default runtime implementation, suspended in the effect F.
      */
    def render: F[Unit] = RendererRuntime.run(this)

    /** Provides a description of this operation, the renderers
      * and extension bundles used, as well as the output target.
      * This functionality is mostly intended for tooling support.
      */
    def describe: F[RendererDescriptor] = RendererDescriptor.create(this)
  }

}
