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

import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.{Async, Sync}
import laika.ast.Path.Root
import laika.ast.{DocumentTree, DocumentTreeRoot}
import laika.io.api.{BinaryTreeRenderer, BinaryTreeTransformer, TreeParser, TreeRenderer, TreeTransformer}
import laika.io.runtime.Batch

/** Provides a description of a transform operation, including the parsers, renderers and extension bundles used,
  * as well as the sources and output target.
  * This functionality is mostly intended for tooling support.
  * 
  * @author Jens Halm
  */
case class TransformerDescriptor (parsers: NonEmptyList[String], 
                                  renderer: String,
                                  bundles: Seq[ExtensionBundleDescriptor],
                                  inputs: TreeInputDescriptor,
                                  output: String,
                                  strict: Boolean,
                                  acceptRawContent: Boolean,
                                  renderFormatted: Boolean) {

  def formatted: String = {
    s"""Parser(s):
       |  ${parsers.toList.mkString("\n  ")}
       |Renderer:
       |  $renderer
       |Extension Bundles:
       |  ${bundles.map(_.formatted).mkString("\n  ")}
       |Settings:
       |  Strict Mode: $strict
       |  Accept Raw Content: $acceptRawContent
       |  Render Formatted: $renderFormatted
       |Sources:
       |  ${inputs.formatted}
       |Target:
       |  $output""".stripMargin
  }
  
}

object TransformerDescriptor {
  
  def apply (parser: ParserDescriptor, renderer: RendererDescriptor): TransformerDescriptor =
    apply(
      parser.parsers, 
      renderer.renderer, 
      parser.bundles, 
      parser.inputs, 
      renderer.output,
      parser.strict, 
      parser.acceptRawContent, 
      renderer.renderFormatted
    )
  
  def create[F[_]: Sync: Batch] (op: TreeTransformer.Op[F]): F[TransformerDescriptor] = for {
    parserDesc <- ParserDescriptor.create(TreeParser.Op(op.parsers, op.theme, op.input))
    renderDesc <- RendererDescriptor.create(TreeRenderer.Op(op.renderer, op.theme, DocumentTreeRoot(DocumentTree(Root, Nil)), op.output, Nil))
  } yield apply(parserDesc, renderDesc)

  def create[F[_]: Async: Batch] (op: BinaryTreeTransformer.Op[F]): F[TransformerDescriptor] = for {
    parserDesc <- ParserDescriptor.create(TreeParser.Op(op.parsers, op.theme, op.input))
    renderDesc <- RendererDescriptor.create(BinaryTreeRenderer.Op(op.renderer, op.theme, DocumentTreeRoot(DocumentTree(Root, Nil)), op.output, Nil))
  } yield apply(parserDesc, renderDesc)

}
