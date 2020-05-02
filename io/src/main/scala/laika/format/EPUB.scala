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

import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.util.{Date, Locale, UUID}

import cats.effect.Async
import laika.ast.Path.Root
import laika.ast._
import laika.config.Config.ConfigResult
import laika.config.{Config, ConfigBuilder, ConfigDecoder, ConfigEncoder, ConfigException, DefaultKey, Key, LaikaKeys}
import laika.factory.{BinaryPostProcessor, RenderContext, RenderFormat, TwoPhaseRenderFormat}
import laika.io.model.{BinaryOutput, RenderedTreeRoot}
import laika.io.runtime.Runtime
import laika.render.epub.{ContainerWriter, HtmlRenderExtensions, HtmlTemplate, StyleSupport}
import laika.render.{HTMLFormatter, XHTMLFormatter, XHTMLRenderer}

/** A post processor for EPUB output, based on an interim HTML renderer.
 *  May be directly passed to the `Renderer` or `Transformer` APIs:
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
  *   .to(EPUB)
  *   .using(GitHubFlavor)
  *   .io(blocker)
  *   .parallel[IO]
  *   .build
  *
  * val res: IO[Unit] = transformer
  *   .fromDirectory("src")
  *   .toFile("demo.epub")
  *   .transform
  * }}}
 *  
 *  In the example above the input from an entire directory gets
 *  merged into a single output file.
 * 
 *  @author Jens Halm
 */
case object EPUB extends TwoPhaseRenderFormat[HTMLFormatter, BinaryPostProcessor] {

  /** A render format for XHTML output as used by EPUB output.
    *
    * This format is usually not used directly with Laika's `Render` or `Transform` APIs.
    * It is primarily used internally by the parent `EPUB` instance.
    *
    *  @author Jens Halm
    */
  object XHTML extends RenderFormat[HTMLFormatter] {

    val fileSuffix: String = "epub.xhtml"

    val defaultRenderer: (HTMLFormatter, Element) => String = XHTMLRenderer

    val formatterFactory: RenderContext[HTMLFormatter] => HTMLFormatter = XHTMLFormatter

    override lazy val defaultTheme: Theme = Theme(
      customRenderer = HtmlRenderExtensions.all,
      defaultTemplate = Some(HtmlTemplate.default)
    )

  }

  val interimFormat: RenderFormat[HTMLFormatter] = XHTML

  /** Configuration options for the generated EPUB output.
    *
    * The duplication of the existing `BookConfig` instance from laika-core happens to have a different
    * implicit key association with the EPUB-specific instance.
    *  
    * @param metadata the metadata associated with the document
    * @param navigationDepth the number of levels to generate a table of contents for
    * @param coverImage the path to the cover image within the virtual document tree   
    */
  case class BookConfig(metadata: DocumentMetadata = DocumentMetadata(),
                        navigationDepth: Option[Int] = None,
                        coverImage: Option[Path] = None) {
    lazy val identifier: String = metadata.identifier.getOrElse(s"urn:uuid:${UUID.randomUUID.toString}")
    lazy val date: Date = metadata.date.getOrElse(new Date)
    lazy val formattedDate: String = DateTimeFormatter.ISO_INSTANT.format(date.toInstant.truncatedTo(ChronoUnit.SECONDS))
    lazy val language: String = metadata.language.getOrElse(Locale.getDefault.getDisplayName)
  }
  
  object BookConfig {
    
    implicit val decoder: ConfigDecoder[BookConfig] = laika.rewrite.nav.BookConfig.decoder.map(c => BookConfig(
      c.metadata, c.navigationDepth, c.coverImage
    ))
    implicit val encoder: ConfigEncoder[BookConfig] = laika.rewrite.nav.BookConfig.encoder.contramap(c => 
      laika.rewrite.nav.BookConfig(c.metadata, c.navigationDepth, c.coverImage)
    )
    implicit val defaultKey: DefaultKey[BookConfig] = DefaultKey(Key("laika","epub"))
    
    def decodeWithDefaults (config: Config): ConfigResult[BookConfig] = for {
      epubConfig   <- config.getOpt[BookConfig].map(_.getOrElse(BookConfig()))
      commonConfig <- config.getOpt[laika.rewrite.nav.BookConfig].map(_.getOrElse(laika.rewrite.nav.BookConfig()))
    } yield {
      BookConfig(
        epubConfig.metadata.withDefaults(commonConfig.metadata), 
        epubConfig.navigationDepth.orElse(commonConfig.navigationDepth),
        epubConfig.coverImage.orElse(commonConfig.coverImage)
      )
    }
    
  }
  
  private lazy val writer = new ContainerWriter

  /** Adds a cover image (if specified in the configuration)
    * and a fallback CSS resource (if the input tree did not contain any CSS),
    * before the tree gets passed to the XHTML renderer.
    */
  def prepareTree (tree: DocumentTreeRoot): Either[Throwable, DocumentTreeRoot] = {
    BookConfig.decodeWithDefaults(tree.config).map { treeConfig =>
      
      val treeWithStyles = StyleSupport.ensureContainsStyles(tree)
      treeConfig.coverImage.fold(tree) { image =>
        treeWithStyles.copy(tree = treeWithStyles.tree.copy(
          content = Document(Root / "cover", RootElement(SpanSequence(Image("cover", InternalTarget(image, image.relative)))), 
          config = ConfigBuilder.empty.withValue(LaikaKeys.title, "Cover").build) +: tree.tree.content
        ))
      }
    }.left.map(ConfigException)
  }

  /** Produces an EPUB container from the specified result tree.
   *
   *  It includes the following files in the container:
   *
   *  - All text markup in the provided document tree, transformed to HTML by the specified render function.
   *  - All static content in the provided document tree, copied to the same relative path within the EPUB container.
   *  - Metadata and navigation files as required by the EPUB specification, auto-generated from the document tree
   *    and the configuration of this instance.
   */
  val postProcessor: BinaryPostProcessor = new BinaryPostProcessor {
    def process[F[_] : Async: Runtime] (result: RenderedTreeRoot[F], output: BinaryOutput[F]): F[Unit] = writer.write(result, output)
  }
  
}
