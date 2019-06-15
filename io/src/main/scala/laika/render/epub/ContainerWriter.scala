/*
 * Copyright 2013-2018 the original author or authors.
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

import java.io.{BufferedInputStream, ByteArrayInputStream, FileInputStream}
import java.nio.charset.Charset

import cats.effect.Async
import cats.implicits._
import laika.ast.Path
import laika.ast.Path.Root
import laika.format.EPUB
import laika.io._

/** Creates the EPUB container based on a document tree and the HTML result
  * of a preceding render operation.
  *
  * @author Jens Halm
  */
class ContainerWriter {


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
  def collectInputs[F[_]: Async] (result: RenderedTreeRoot, config: EPUB.Config): F[Vector[BinaryInput]] = {

    val contentRoot = Root / "EPUB" / "content"

    def shiftContentPath (path: Path): Path =
      if (path.suffix == "html") Path(contentRoot, path.withSuffix("xhtml").components)
      else Path(contentRoot, path.components)

    def toBinaryInput (content: String, path: Path): F[BinaryInput] = Async[F].delay {
      new ByteArrayInputStream(content.getBytes(Charset.forName("UTF-8")))
    }.map(BinaryStreamInput(_, autoClose = true, path))

    val staticDocs: Seq[F[BinaryInput]] = result.staticDocuments.filter(in => MimeTypes.supportedTypes.contains(in.path.suffix)).map {
      case fileInput: BinaryFileInput => Async[F].pure[BinaryInput](fileInput.copy(path = shiftContentPath(fileInput.path)))
      case streamInput: BinaryStreamInput => Async[F].pure[BinaryInput](streamInput.copy(path = shiftContentPath(streamInput.path)))
    }

    val mimeType  = toBinaryInput(StaticContent.mimeType, Root / "mimetype")
    val container = toBinaryInput(StaticContent.container, Root / "META-INF" / "container.xml")
    val iBooksOpt = toBinaryInput(StaticContent.iBooksOptions, Root / "META-INF" / "com.apple.ibooks.display-options.xml")
    val opf       = toBinaryInput(opfRenderer.render(result, config), Root / "EPUB" / "content.opf")
    val nav       = toBinaryInput(navRenderer.render(result, config.tocDepth), Root / "EPUB" / "nav.xhtml")
    val ncx       = toBinaryInput(ncxRenderer.render(result, config.identifier, config.tocDepth), Root / "EPUB" / "toc.ncx")
    
    val renderedDocs = result.allDocuments.map(doc => toBinaryInput(doc.content, shiftContentPath(doc.path)))

    val allInputs: Seq[F[BinaryInput]] = Seq(mimeType, container, iBooksOpt, opf, nav, ncx) ++ renderedDocs ++ staticDocs
    
    allInputs.toVector.sequence
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
  def write[F[_]: Async] (result: RenderedTreeRoot, output: BinaryOutput): F[Unit] = {

    val inputs = collectInputs(result, ConfigFactory.forTreeConfig(result.config))

    ZipWriter.zipEPUB(inputs, output)

  }


}
