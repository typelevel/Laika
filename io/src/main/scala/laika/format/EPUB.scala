package laika.format

import java.time.Instant
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.util.{Locale, UUID}

import cats.effect.Async
import laika.ast.Path.Root
import laika.ast._
import laika.config.ConfigBuilder
import laika.factory.{BinaryPostProcessor, RenderContext, RenderFormat, TwoPhaseRenderFormat}
import laika.io.model.{BinaryOutput, RenderedTreeRoot}
import laika.io.runtime.Runtime
import laika.render.epub.{ConfigFactory, ContainerWriter, HtmlRenderExtensions, HtmlTemplate, StyleSupport}
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
object EPUB extends TwoPhaseRenderFormat[HTMLFormatter, BinaryPostProcessor] {

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
    *  @param metadata the metadata associated with the document
    *  @param tocDepth the number of levels to generate a table of contents for
    *  @param tocTitle the title for the table of contents
    *  @param coverImage the path to the cover image within the virtual document tree                
    */
  case class Config(metadata: DocumentMetadata = DocumentMetadata(), tocDepth: Int = Int.MaxValue, tocTitle: Option[String] = None, coverImage: Option[String] = None) {
    lazy val identifier: String = metadata.identifier.getOrElse(s"urn:uuid:${UUID.randomUUID.toString}")
    lazy val date: Instant = metadata.date.getOrElse(Instant.now)
    lazy val formattedDate: String = DateTimeFormatter.ISO_INSTANT.format(date.truncatedTo(ChronoUnit.SECONDS))
    lazy val language: Locale = metadata.language.getOrElse(Locale.getDefault)
  }

  /** Companion for the creation of `Config` instances.
    */
  object Config {

    /** The default configuration.
      */
    val default: Config = apply()
  }

  private lazy val writer = new ContainerWriter

  /** Adds a cover image (if specified in the configuration)
    * and a fallback CSS resource (if the input tree did not contain any CSS),
    * before the tree gets passed to the XHTML renderer.
    */
  def prepareTree (tree: DocumentTreeRoot): Either[Throwable, DocumentTreeRoot] = {
    val treeConfig = ConfigFactory.forTreeConfig(tree.config)
    val treeWithStyles = StyleSupport.ensureContainsStyles(tree)
    Right(treeConfig.coverImage.fold(tree) { image =>
      treeWithStyles.copy(tree = treeWithStyles.tree.copy(
        content = Document(Root / "cover", 
          RootElement(Seq(SpanSequence(Seq(Image("cover", URI(image)))))), 
        config = ConfigBuilder.empty.withValue("title", "Cover").build) +: tree.tree.content
      ))
    })
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
