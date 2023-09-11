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

package laika.format

import cats.effect.{ Async, Resource }
import laika.api.builder.OperationConfig
import laika.ast.DocumentTreeRoot
import laika.config.Config
import laika.factory.{
  BinaryPostProcessor,
  BinaryPostProcessorBuilder,
  RenderFormat,
  TwoPhaseRenderFormat
}
import laika.io.model.{ BinaryOutput, RenderedDocument, RenderedTree, RenderedTreeRoot }
import laika.render.Formatter
import laika.theme.Theme

object TestRenderResultProcessor
    extends TwoPhaseRenderFormat[Formatter, BinaryPostProcessorBuilder] {

  val interimFormat: RenderFormat[Formatter] = AST

  def prepareTree(tree: DocumentTreeRoot): Either[Throwable, DocumentTreeRoot] = Right(tree)

  def postProcessor: BinaryPostProcessorBuilder = new BinaryPostProcessorBuilder {

    def build[F[_]: Async](config: Config, theme: Theme[F]): Resource[F, BinaryPostProcessor[F]] =
      Resource.pure(new BinaryPostProcessor[F] {

        override def process(
            result: RenderedTreeRoot[F],
            output: BinaryOutput[F],
            config: OperationConfig
        ): F[Unit] = {

          def append(sb: StringBuilder, result: RenderedTree): Unit = {
            result.content.foreach {
              case d: RenderedDocument => sb.append(d.content + "\n")
              case t: RenderedTree     => append(sb, t)
              // case _ => ()
            }
          }

          val sb           = new StringBuilder
          append(sb, result.tree)
          val resultString = sb.toString

          output.resource.use { out =>
            Async[F].delay(out.write(resultString.getBytes("UTF-8")))
          }
        }

      })

  }

}
