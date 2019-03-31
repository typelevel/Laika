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

import java.io.ByteArrayInputStream
import java.nio.charset.Charset

import laika.ast.Path.Root
import laika.ast.{DocumentTree, Path, StaticDocument}
import laika.format.EPUB
import laika.io.Input.Binary
import laika.io.Output.BinaryOutput
import laika.io.OutputTree.ResultTree
import laika.io.{IO, Input}

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
    *  @param tree the tree to obtain navigation info from
    *  @param html the dynamically rendered HTML output
    *  @return a list of all documents that need to be written to the EPUB container.
    */
  def collectInputs (tree: DocumentTree, config: EPUB.Config, html: ResultTree): Seq[Input with Binary] = {

    val contentRoot = Root / "EPUB" / "content"

    def shiftContentPath (path: Path): Path =
      if (path.suffix == "html") Path(contentRoot, path.components.dropRight(1) :+ path.basename + ".xhtml")
      else Path(contentRoot, path.components)

    def toBinaryInput (content: String, path: Path): Input with Binary = {
      val bytes = content.getBytes(Charset.forName("UTF-8"))
      val in = new ByteArrayInputStream(bytes)
      Input.fromStream(in, path)
    }

    def toInput (htmlTree: ResultTree): Seq[Input with Binary] = {
      htmlTree.results.map {
        doc => toBinaryInput(doc.result, shiftContentPath(doc.path))
      } ++ htmlTree.subtrees.flatMap(toInput)
    }

    def collectStaticFiles (currentTree: DocumentTree): Seq[Input with Binary] = {
      currentTree.additionalContent.filter(c => MimeTypes.supportedTypes.contains(c.path.suffix)).flatMap {
        case StaticDocument(input: Input with Binary) =>
          Seq(Input.fromStream(input.asBinaryInput.asStream, shiftContentPath(input.path)))
        case StaticDocument(input) =>
          Seq(toBinaryInput(input.asParserInput.input, shiftContentPath(input.path)))
        case _ => Seq()
      } ++ currentTree.content.flatMap {
        case childTree: DocumentTree => collectStaticFiles(childTree)
        case _ => Seq()
      }
    }

    val mimeType  = toBinaryInput(StaticContent.mimeType, Root / "mimetype")
    val container = toBinaryInput(StaticContent.container, Root / "META-INF" / "container.xml")
    val iBooksOpt = toBinaryInput(StaticContent.iBooksOptions, Root / "META-INF" / "com.apple.ibooks.display-options.xml")
    val opf       = toBinaryInput(opfRenderer.render(tree, config), Root / "EPUB" / "content.opf")
    val nav       = toBinaryInput(navRenderer.render(tree, config.tocDepth), Root / "EPUB" / "nav.xhtml")
    val ncx       = toBinaryInput(ncxRenderer.render(tree, config.identifier, config.tocDepth), Root / "EPUB" / "toc.ncx")

    Seq(mimeType, container, iBooksOpt, opf, nav, ncx) ++ toInput(html) ++ collectStaticFiles(tree)
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
    *  @param tree the tree to obtain navigation info from
    *  @param config the configuration for the EPUB container
    *  @param html the dynamically rendered HTML output
    *  @param output the output to write the EPUB container to
    */
  def write (tree: DocumentTree, config: EPUB.Config, html: ResultTree, output: BinaryOutput): Unit = {

    val inputs = collectInputs(tree, config, html)

    IO.zip(inputs, output)

  }


}
