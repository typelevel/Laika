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

package laika.render.epub

import cats.effect.{ Async, Sync }
import cats.implicits.*
import laika.ast.Path
import laika.ast.Path.Root
import laika.config.ConfigException
import laika.format.EPUB
import laika.io.model.*
import laika.render.TagFormatter
import laika.theme.config.BookConfig

/** Creates the EPUB container based on a document tree and the HTML result
  * of a preceding render operation.
  *
  * @author Jens Halm
  */
private[laika] class ContainerWriter {

  private val opfRenderer = new OPFRenderer
  private val navRenderer = new HtmlNavRenderer
  private val ncxRenderer = new NCXRenderer

  /** Collects all documents that need to be written to the EPUB container.
    *
    * This includes:
    *
    * - All rendered HTML in the provided result tree.
    * - All static content in the provided document tree, copied to the same relative path within the EPUB container.
    * - Metadata and navigation files as required by the EPUB specification, auto-generated from the document tree
    *   and the configuration of this instance.
    *
    *  @param result the result of the render operation as a tree
    *  @return a list of all documents that need to be written to the EPUB container.
    */
  def collectInputs[F[_]](
      result: RenderedTreeRoot[F],
      config: BookConfig
  ): Seq[BinaryInput[F]] = {

    val contentRoot = Root / "EPUB" / "content"

    def shiftContentPath(path: Path): Path =
      if (path.suffix.contains("html")) contentRoot / path.withSuffix("xhtml").relative
      else contentRoot / path.relative

    def toBinaryInput(content: String, path: Path): BinaryInput[F] =
      BinaryInput.fromString(content, path)

    val finalResult = result.withStaticDocuments(result.staticDocuments)

    val staticDocs: Seq[BinaryInput[F]] = finalResult.staticDocuments
      .filter(in =>
        in.path.suffix.exists(suf =>
          MimeTypes.supportedTypes.contains(suf) ||
          MimeTypes.supportedTypes.contains(suf.split("\\.").last)
        )
      )
      .map { doc =>
        doc.withPath(shiftContentPath(doc.path))
      }

    val title = TagFormatter.escape(
      config.metadata.title.getOrElse(finalResult.title.fold("UNTITLED")(_.extractText))
    )

    val mimeType  = toBinaryInput(StaticContent.mimeType, Root / "mimetype")
    val container = toBinaryInput(StaticContent.container, Root / "META-INF" / "container.xml")
    val iBooksOpt = toBinaryInput(
      StaticContent.iBooksOptions,
      Root / "META-INF" / "com.apple.ibooks.display-options.xml"
    )
    val opf       =
      toBinaryInput(opfRenderer.render(finalResult, title, config), Root / "EPUB" / "content.opf")
    val nav       = toBinaryInput(
      navRenderer.render(finalResult, title, config.navigationDepth),
      Root / "EPUB" / "nav.xhtml"
    )
    val ncx       = toBinaryInput(
      ncxRenderer.render(finalResult, title, config.identifier, config.navigationDepth),
      Root / "EPUB" / "toc.ncx"
    )

    val renderedDocs =
      finalResult.allDocuments.map(doc => toBinaryInput(doc.content, shiftContentPath(doc.path)))

    Seq(mimeType, container, iBooksOpt, opf, nav, ncx) ++ renderedDocs ++ staticDocs
  }

  /** Produces an EPUB container from the specified document tree.
    *
    * This includes:
    *
    * - All rendered HTML in the provided result tree.
    * - All static content in the provided document tree, copied to the same relative path within the EPUB container.
    * - Metadata and navigation files as required by the EPUB specification, auto-generated from the document tree
    *   and the configuration of this instance.
    *
    * @param result the result of the render operation as a tree
    * @param output the output to write the final result to
    */
  def write[F[_]: Async](result: RenderedTreeRoot[F], output: BinaryOutput[F]): F[Unit] = {

    for {
      config <- Sync[F].fromEither(
        BookConfig.decodeWithDefaults(result.config, EPUB.configKey).left.map(ConfigException.apply)
      )
      inputs = collectInputs(result, config)
      _ <- ZipWriter.zipEPUB(inputs, output)
    } yield ()

  }

}
