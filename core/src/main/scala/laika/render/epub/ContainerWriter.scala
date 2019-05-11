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

import java.io.{BufferedInputStream, ByteArrayInputStream, FileInputStream, InputStream}
import java.nio.charset.Charset

import laika.ast.Path
import laika.ast.Path.Root
import laika.format.EPUB2
import laika.io._

// TODO - 0.12 - replace with new model
case class StreamInput (stream: InputStream, path: Path)

/** Creates the EPUB container based on a document tree and the HTML result
  * of a preceding render operation.
  *
  * @author Jens Halm
  */
class ContainerWriter {


  private val opfRenderer = new OPFRenderer
  private val navRenderer = new HtmlNavRenderer
  private val ncxRenderer = new NCXRenderer

  // TODO - 0.12 - replace with new model
  def fromStream (stream: InputStream, path: Path = Path.Root): StreamInput = StreamInput(stream, path)
  

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
  def collectInputs (result: RenderResult2, config: EPUB2.Config): Seq[StreamInput] = {

    val contentRoot = Root / "EPUB" / "content"

    def shiftContentPath (path: Path): Path =
      if (path.suffix == "html") Path(contentRoot, path.components.dropRight(1) :+ path.basename + ".xhtml")
      else Path(contentRoot, path.components)

    def toBinaryInput (content: String, path: Path): StreamInput = {
      val bytes = content.getBytes(Charset.forName("UTF-8"))
      val in = new ByteArrayInputStream(bytes)
      fromStream(in, path)
    }

    def collectDocuments (currentTree: RenderedTree): Seq[StreamInput] = {
      currentTree.content.flatMap {
        case RenderedDocument(path, _, _, content) => Seq(toBinaryInput(content, shiftContentPath(path)))
          
        case childTree: RenderedTree => collectDocuments(childTree)
          
        case CopiedDocument(input) if MimeTypes.supportedTypes.contains(input.path.suffix) => input match {
          case fileInput: BinaryFileInput =>
            Seq(fromStream(new BufferedInputStream(new FileInputStream(fileInput.file)), shiftContentPath(input.path)))
          case byteInput: ByteInput =>
            Seq(fromStream(new ByteArrayInputStream(byteInput.bytes), shiftContentPath(input.path)))
        }
        case _ => Seq()
      }
    }

    val mimeType  = toBinaryInput(StaticContent.mimeType, Root / "mimetype")
    val container = toBinaryInput(StaticContent.container, Root / "META-INF" / "container.xml")
    val iBooksOpt = toBinaryInput(StaticContent.iBooksOptions, Root / "META-INF" / "com.apple.ibooks.display-options.xml")
    val opf       = toBinaryInput(opfRenderer.render(result, config), Root / "EPUB" / "content.opf")
    val nav       = toBinaryInput(navRenderer.render(result, config.tocDepth), Root / "EPUB" / "nav.xhtml")
    val ncx       = toBinaryInput(ncxRenderer.render(result, config.identifier, config.tocDepth), Root / "EPUB" / "toc.ncx")

    Seq(mimeType, container, iBooksOpt, opf, nav, ncx) ++ collectDocuments(result.rootTree)
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
  def write (result: RenderResult2, output: BinaryOutput): Unit = {

    val inputs = collectInputs(result, ConfigFactory.forTreeConfig(result.config))

    IO.zipEPUB(inputs, output)

  }


}
