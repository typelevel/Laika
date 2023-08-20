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

import cats.effect.std.Dispatcher
import cats.effect.{ Async, Resource }
import cats.syntax.all.*
import laika.api.builder.OperationConfig
import laika.ast.{ DocumentTreeRoot, TemplateRoot }
import laika.config.{ Config, Key }
import laika.factory.{
  BinaryPostProcessor,
  BinaryPostProcessorBuilder,
  RenderFormat,
  TwoPhaseRenderFormat
}
import laika.io.model.{ BinaryOutput, RenderedTreeRoot }
import laika.theme.Theme
import laika.render.FOFormatter
import laika.render.FOFormatter.Preamble
import laika.render.pdf.{ FOConcatenation, FopFactoryBuilder, PDFRenderer }
import laika.theme.config.BookConfig

/** A post processor for PDF output, based on an interim XSL-FO renderer.
  *  May be directly passed to the `Render` or `Transform` APIs:
  *
  * {{{
  * val transformer = Transformer
  *   .from(Markdown)
  *   .to(PDF)
  *   .using(GitHubFlavor)
  *   .parallel[IO]
  *   .build
  *
  * val res: IO[Unit] = transformer
  *   .fromDirectory("src")
  *   .toFile("demo.pdf")
  *   .transform
  * }}}
  *
  *  In the example above the input from an entire directory gets
  *  merged into a single output file.
  *
  *  @author Jens Halm
  */
object PDF extends TwoPhaseRenderFormat[FOFormatter, BinaryPostProcessorBuilder] {

  override val description: String = "PDF"

  val interimFormat: RenderFormat[FOFormatter] = XSLFO

  /** The key to read `BookConfig` instance from for this PDF renderer. */
  val configKey: Key = Key("laika", "pdf")

  /** Adds a preamble to each document for navigation and replaces the template with a fallback.
    * The modified tree will be used for rendering the interim XSL-FO result.
    * The original template will only be applied to the concatenated result of the XSL-FO renderer
    * in a later step.
    */
  def prepareTree(root: DocumentTreeRoot): Either[Throwable, DocumentTreeRoot] =
    Right(
      root
        .modifyTree(_.withDefaultTemplate(TemplateRoot.fallback, "fo"))
        .modifyDocumentsRecursively { doc =>
          val preamble = Preamble(doc.title.fold(doc.name)(_.extractText))
          doc.prependContent(preamble)
        }
    )

  /** Processes the interim XSL-FO result, transforms it to PDF and writes it to the specified final output.
    */
  def postProcessor: BinaryPostProcessorBuilder = new BinaryPostProcessorBuilder {

    def build[F[_]: Async](config: Config, theme: Theme[F]): Resource[F, BinaryPostProcessor[F]] =
      Dispatcher.parallel[F].evalMap { dispatcher =>
        val pdfConfig = BookConfig.decodeWithDefaults(config, configKey).getOrElse(BookConfig.empty)
        FopFactoryBuilder.build(pdfConfig, theme.inputs.binaryInputs, dispatcher).map {
          fopFactory =>
            new BinaryPostProcessor[F] {
              private val renderer = new PDFRenderer(fopFactory, dispatcher)
              override def process(
                  result: RenderedTreeRoot[F],
                  output: BinaryOutput[F],
                  opConfig: OperationConfig
              ): F[Unit] =
                for {
                  fo <- Async[F].fromEither(FOConcatenation(result, pdfConfig, opConfig))
                  _  <- renderer.render(fo, output, pdfConfig.metadata, result.staticDocuments)
                } yield ()
            }
        }
      }

  }

}
