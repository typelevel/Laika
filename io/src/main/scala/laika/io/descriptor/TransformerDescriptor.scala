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

package laika.io.descriptor

import cats.data.NonEmptyList
import cats.effect.Async
import cats.syntax.all.*
import laika.ast.DocumentTree
import laika.io.api.{
  BinaryTreeRenderer,
  BinaryTreeTransformer,
  TreeParser,
  TreeRenderer,
  TreeTransformer
}
import laika.io.internal.runtime.Batch

/** Provides a description of a transform operation, including the parsers, renderers and extension bundles used,
  * as well as the sources and output target.
  * This functionality is mostly intended for tooling support.
  *
  * @author Jens Halm
  */
class TransformerDescriptor private (
    val parsers: NonEmptyList[String],
    val renderer: String,
    val bundles: Seq[ExtensionBundleDescriptor],
    val inputs: TreeInputDescriptor,
    val theme: ThemeDescriptor,
    val output: String,
    val strict: Boolean,
    val acceptRawContent: Boolean,
    val compactRendering: Boolean
) {

  def formatted: String = {
    s"""Parser(s):
       |  ${parsers.toList.mkString("\n  ")}
       |Renderer:
       |  $renderer
       |Extension Bundles:
       |  ${bundles.map(_.formatted).mkString("\n  ")}
       |Theme:
       |  ${theme.formatted}
       |Settings:
       |  Strict Mode: $strict
       |  Accept Raw Content: $acceptRawContent
       |  Compact Rendering: $compactRendering
       |Sources:
       |  ${inputs.formatted}
       |Target:
       |  $output""".stripMargin
  }

  private[laika] def withRendererDescription(desc: String): TransformerDescriptor =
    new TransformerDescriptor(
      parsers,
      desc,
      bundles,
      inputs,
      theme,
      output,
      strict,
      acceptRawContent,
      compactRendering
    )

}

object TransformerDescriptor {

  def apply(parser: ParserDescriptor, renderer: RendererDescriptor): TransformerDescriptor =
    new TransformerDescriptor(
      parser.parsers,
      renderer.renderer,
      parser.bundles,
      parser.inputs,
      renderer.theme,
      renderer.output,
      parser.strict,
      parser.acceptRawContent,
      renderer.compactRendering
    )

  private[io] def create[F[_]: Async: Batch](op: TreeTransformer.Op[F]): F[TransformerDescriptor] =
    for {
      parserDesc <- ParserDescriptor.create(new TreeParser.Op(op.parsers, op.theme, op.input))
      renderDesc <- RendererDescriptor.create(
        new TreeRenderer.Op(
          op.renderer,
          op.theme,
          DocumentTree.builder.buildRoot,
          op.output,
          Nil
        )
      )
    } yield apply(parserDesc, renderDesc)

  private[io] def create[F[_]: Async: Batch](
      op: BinaryTreeTransformer.Op[F]
  ): F[TransformerDescriptor] = for {
    parserDesc <- ParserDescriptor.create(new TreeParser.Op(op.parsers, op.theme, op.input))
    renderDesc <- RendererDescriptor.create(
      new BinaryTreeRenderer.Op(
        op.renderer,
        op.theme,
        DocumentTree.builder.buildRoot,
        op.output,
        Nil
      )
    )
  } yield apply(parserDesc, renderDesc)

}
