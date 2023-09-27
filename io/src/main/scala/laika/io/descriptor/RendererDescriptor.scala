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

import cats.Applicative
import laika.io.api.{ BinaryTreeRenderer, TreeRenderer }
import laika.io.internal.model.{ DirectoryOutput, TreeOutput }
import laika.io.model.BinaryOutput

/** Provides a description of a render operation, including the renderers
  * and extension bundles used, as well as the output target.
  * This functionality is mostly intended for tooling support.
  *
  * @author Jens Halm
  */
class RendererDescriptor(
    val renderer: String,
    val bundles: Seq[ExtensionBundleDescriptor],
    val theme: ThemeDescriptor,
    val output: String,
    val compactRendering: Boolean
) {

  def formatted: String = {
    s"""Renderer:
       |  $renderer
       |Extension Bundles:
       |  ${bundles.mkString("\n  ")}
       |Theme:
       |  ${theme.formatted}
       |Settings:
       |  Compact Rendering: $compactRendering
       |Target:
       |  $output""".stripMargin
  }

}

private[io] object RendererDescriptor {

  private def describeOutput[F[_]](out: BinaryOutput[F]): String = out.targetFile.fold(
    "In-memory bytes or stream"
  )(f => s"File '${f.toString}'")

  private def describeOutput(out: TreeOutput): String = out match {
    case DirectoryOutput(dir, _) => s"Directory '${dir.toString}'"
    case _                       => "In-memory strings or streams"
  }

  def create[F[_]: Applicative](op: TreeRenderer.Op[F]): F[RendererDescriptor] =
    Applicative[F].pure(
      new RendererDescriptor(
        op.renderer.format.description,
        op.renderer.config.filteredBundles.map(new ExtensionBundleDescriptor(_)),
        op.theme.descriptor,
        describeOutput(op.output),
        op.renderer.config.compactRendering
      )
    )

  def create[F[_]: Applicative](op: BinaryTreeRenderer.Op[F]): F[RendererDescriptor] =
    Applicative[F].pure(
      new RendererDescriptor(
        op.renderer.description,
        op.renderer.interimRenderer.config.filteredBundles.map(new ExtensionBundleDescriptor(_)),
        op.theme.descriptor,
        describeOutput(op.output),
        op.renderer.interimRenderer.config.compactRendering
      )
    )

}
