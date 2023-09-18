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
import laika.api.config.{ Config, ConfigDecoder, ConfigEncoder, DefaultKey, Key }
import laika.api.format.{
  BinaryPostProcessor,
  RenderContext,
  RenderFormat,
  TagFormatter,
  TwoPhaseRenderFormat
}
import laika.ast.Path.Root
import laika.ast.*
import laika.config.*
import laika.api.config.ConfigError.ValidationError
import laika.epub.internal.{ ContainerWriter, XHTMLRenderer }
import laika.internal.render.HTMLFormatter
import laika.io.internal.errors.ConfigException
import laika.io.model.{ BinaryOutput, RenderedTreeRoot }
import laika.theme.config.BookConfig
import laika.theme.Theme

/** A post processor for EPUB output, based on an interim HTML renderer.
  *  May be directly passed to the `Renderer` or `Transformer` APIs:
  *
  * {{{
  * val transformer = Transformer
  *   .from(Markdown)
  *   .to(EPUB)
  *   .using(GitHubFlavor)
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
case object EPUB extends TwoPhaseRenderFormat[TagFormatter, BinaryPostProcessor.Builder] {

  override val description: String = "EPUB"

  /** A render format for XHTML output as used by EPUB output.
    *
    * This format is usually not used directly with Laika's `Render` or `Transform` APIs.
    * It is primarily used internally by the parent `EPUB` instance.
    *
    *  @author Jens Halm
    */
  object XHTML extends RenderFormat[TagFormatter] {

    override val description: String = "XHTML"

    val fileSuffix: String = "xhtml"

    val defaultRenderer: (TagFormatter, Element) => String = XHTMLRenderer

    val formatterFactory: RenderContext[TagFormatter] => TagFormatter =
      context => new HTMLFormatter(closeEmptyTags = true, context)

  }

  val interimFormat: RenderFormat[TagFormatter] = XHTML

  /** The key to read `BookConfig` instance from for this EPUB renderer. */
  val configKey: Key = Key("laika", "epub")

  /** Configuration Enumeration that indicates whether an EPUB template contains scripting. */
  sealed trait ScriptedTemplate extends Product

  object ScriptedTemplate {

    /** Indicates that the template is considered to include scripting in all scenarios. */
    case object Always extends ScriptedTemplate

    /** Indicates that the template is never considered to include scripting in any scenario. */
    case object Never extends ScriptedTemplate

    /** Indicates that the template's scripting support should be determined from the environment,
      * e.g. the presence of JavaScript files for EPUB in the input tree.
      */
    case object Auto extends ScriptedTemplate

    implicit val decoder: ConfigDecoder[ScriptedTemplate] = ConfigDecoder.string.flatMap {
      case "always" => Right(Always)
      case "never"  => Right(Never)
      case "auto"   => Right(Auto)
      case other    => Left(ValidationError(s"Invalid value: $other"))
    }

    implicit val encoder: ConfigEncoder[ScriptedTemplate] =
      ConfigEncoder.string.contramap(_.productPrefix.toLowerCase())

    implicit val defaultKey: DefaultKey[ScriptedTemplate] =
      DefaultKey(LaikaKeys.root.child(Key("epub", "scripted")))

  }

  private lazy val writer = new ContainerWriter

  /** Adds a cover image (if specified in the configuration)
    * and a fallback CSS resource (if the input tree did not contain any CSS),
    * before the tree gets passed to the XHTML renderer.
    */
  def prepareTree(root: DocumentTreeRoot): Either[Throwable, DocumentTreeRoot] = {
    BookConfig.decodeWithDefaults(root.config, configKey).map { treeConfig =>
      treeConfig.coverImage.fold(root) { image =>
        root.modifyTree(
          _.prependContent(
            Document(
              Root / "cover",
              RootElement(SpanSequence(Image(InternalTarget(image), alt = Some("cover"))))
            )
              .modifyConfig(_.withValue(LaikaKeys.title, "Cover"))
          )
        )
      }
    }.left.map(ConfigException.apply)
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
  def postProcessor: BinaryPostProcessor.Builder = new BinaryPostProcessor.Builder {

    def build[F[_]: Async](config: Config, theme: Theme[F]): Resource[F, BinaryPostProcessor[F]] =
      Resource.pure[F, BinaryPostProcessor[F]](new BinaryPostProcessor[F] {

        def process(
            result: RenderedTreeRoot[F],
            output: BinaryOutput[F],
            config: OperationConfig
        ): F[Unit] =
          writer.write(result, output)

      })

  }

}
