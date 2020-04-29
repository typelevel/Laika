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

import java.io.File

import cats.effect.Async
import cats.implicits._
import laika.ast.{DocumentMetadata, DocumentTreeRoot, SpanSequence, TemplateRoot}
import laika.config.ConfigException
import laika.factory.{BinaryPostProcessor, RenderFormat, TwoPhaseRenderFormat}
import laika.io.model.{BinaryOutput, RenderedTreeRoot}
import laika.io.runtime.Runtime
import laika.render.FOFormatter
import laika.render.pdf.{FOConcatenation, PDFConfigBuilder, PDFNavigation, PDFRenderer}
import org.apache.fop.apps.{FopFactory, FopFactoryBuilder}

/** A post processor for PDF output, based on an interim XSL-FO renderer. 
 *  May be directly passed to the `Render` or `Transform` APIs:
 *
  * {{{
  * implicit val cs: ContextShift[IO] = 
  *   IO.contextShift(ExecutionContext.global)
  *
  * val blocker = Blocker.liftExecutionContext(
  *   ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  * )
  *
  * val transformer = Transformer
  *   .from(Markdown)
  *   .to(PDF)
  *   .using(GitHubFlavor)
  *   .io(blocker)
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
class PDF private(val interimFormat: RenderFormat[FOFormatter], config: Option[PDF.Config], fopFactory: Option[FopFactory]) 
  extends TwoPhaseRenderFormat[FOFormatter, BinaryPostProcessor] {

  override val description: String = "PDF"

  @deprecated("use withConfigValue on builder API for Transformer or Renderer", "0.15.0")
  def withConfig (config: PDF.Config): PDF = new PDF(interimFormat, Some(config), fopFactory)

  /** Allows to specify a custom FopFactory in case additional configuration
    * is required for custom fonts, stemmers or other FOP features.
    *
    * A `FopFactory` is a fairly heavy-weight object, so make sure that you reuse
    * either the `FopFactory` instance itself or the resulting `PDF` renderer.
    * In case you do not specify a custom factory, Laika ensures that the default
    * factory is reused between renderers.
    */
  def withFopFactory (fopFactory: FopFactory): PDF = new PDF(interimFormat, config, Some(fopFactory))
  
  private lazy val renderer = new PDFRenderer(config, fopFactory)


  /** Adds PDF bookmarks and/or a table of content to the specified document tree, depending on configuration.
    * The modified tree will be used for rendering the interim XSL-FO result.
    */
  def prepareTree (root: DocumentTreeRoot): Either[Throwable, DocumentTreeRoot] = {
    val pdfConfig = config.map(Right(_)).getOrElse(PDFConfigBuilder.fromTreeConfig(root.config))
    val rootWithTemplate = root.copy(tree = root.tree.withDefaultTemplate(TemplateRoot.fallback, "fo"))
    pdfConfig.map(PDFNavigation.prepareTree(rootWithTemplate, _)).left.map(ConfigException)
  }

  /** Processes the interim XSL-FO result, transforms it to PDF and writes
    * it to the specified final output.
    */
  val postProcessor: BinaryPostProcessor = new BinaryPostProcessor {
    override def process[F[_]: Async: Runtime] (result: RenderedTreeRoot[F], output: BinaryOutput[F]): F[Unit] = {
      
      val title = result.title.map(_.extractText)
      val pdfConfig = config.map(Right(_)).getOrElse(PDFConfigBuilder.fromTreeConfig(result.config))
      
      for {
        config   <- Async[F].fromEither(pdfConfig.left.map(ConfigException))
        fo       <- Async[F].fromEither(FOConcatenation(result, config).left.map(ConfigException))
        metadata <- Async[F].fromEither(DocumentMetadata.fromConfig(result.config).left.map(ConfigException))
        _        <- renderer.render(fo, output, metadata, title, result.sourcePaths)
      } yield ()
    }
  }

}

/** The default instance of the PDF renderer.
  */
object PDF extends PDF(XSLFO, None, None) {

  /** The reusable default instance of the FOP factory
    * that the PDF renderer will use if no custom
    * factory is specified.
    */
  lazy val defaultFopFactory: FopFactory = new FopFactoryBuilder(new File(".").toURI).build

  /** Configuration options for the generated PDF output.
    *
    *  @param bookmarkDepth the number of levels bookmarks should be generated for, use 0 to switch off bookmark generation
    *  @param tocDepth the number of levels to generate a table of contents for, use 0 to switch off toc generation
    *  @param tocTitle the title for the table of contents
    */
  case class Config(bookmarkDepth: Int = Int.MaxValue, tocDepth: Int = Int.MaxValue, tocTitle: Option[String] = None)

  /** Companion for the creation of `PDFConfig` instances.
    */
  object Config {

    /** The default configuration, with all optional features enabled.
      */
    val default: Config = apply()
  }

}

