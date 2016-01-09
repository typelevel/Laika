/*
 * Copyright 2014 the original author or authors.
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

package laika.render

import laika.factory.RenderResultProcessor
import laika.io.Output
import laika.io.Output.BinaryOutput
import laika.io.OutputProvider.OutputConfig
import laika.tree.Documents.DocumentTree
import laika.tree.Elements.Element
import laika.tree.Elements.MessageLevel
import java.io.File
import java.io.OutputStream
import java.io.StringReader
import javax.xml.transform.TransformerFactory
import javax.xml.transform.URIResolver
import javax.xml.transform.sax.SAXResult
import javax.xml.transform.stream.StreamSource
import org.apache.fop.apps.FopFactory
import org.apache.xmlgraphics.util.MimeConstants

/** A post processor for PDF output, based on an interim XSL-FO renderer. 
 *  May be directly passed to the `Render` or `Transform` APIs:
 * 
 *  {{{
 *  val document: Document = ...
 *  Render as PDF from document toFile "hello.pdf"
 *  
 *  Transform from Markdown to PDF fromDirectory "book-src" toFile "book.pdf"
 *  }}}
 *  
 *  In the second example above the input from an entire directory gets
 *  merged into a single output directory.
 * 
 *  @author Jens Halm
 */
class PDF private (val factory: XSLFO, config: PDFConfig) extends RenderResultProcessor[FOWriter] {

  
  private lazy val fopFactory = FopFactory.newInstance
  
  
  /** Specifies the minimum required level for a system message
   *  to get included into the output by this renderer.
   */
  def withMessageLevel (level: MessageLevel): PDF = new PDF(factory.withMessageLevel(level), config)
  
  /** Allows to specify configuration options like insertion
   *  of bookmarks or table of content.
   */
  def withConfig (config: PDFConfig): PDF = new PDF(factory, config)
  
  private lazy val foForPDF = new FOforPDF(config)
  

  /** Renders the XSL-FO that serves as a basis for producing the final PDF output.
   *  The result should include the output from rendering the documents in the 
   *  specified tree as well as any additional insertions like bookmarks or
   *  table of content. For this the specified `DocumentTree` instance may get
   *  modified before passing it to the given render function.
   *  
   *  The default implementation simply delegates to an instance of `FOforPDF`
   *  which uses a `PDFConfig` object to drive configuration. In rare cases
   *  where the flexibility provided by `PDFConfig` is not sufficient, this
   *  method may get overridden.
   * 
   *  @param tree the document tree serving as input for the renderer
   *  @param render the actual render function for producing the XSL-FO output
   *  @return the rendered XSL-FO as a String 
   */
  protected def renderFO (tree: DocumentTree, render: (DocumentTree, OutputConfig) => Unit): String =
    foForPDF.renderFO(tree, render)
  
  /** Processes the tree by first using the specified render function
   *  to produce the interim XSL-FO result, transform the result to PDF and write
   *  it to the specified final output.
   * 
   *  @param tree the tree to render to the interim result
   *  @param render the render function for producing the interim XSL-FO result
   *  @param output the output to write the final result to
   */  
  def process (tree: DocumentTree, render: (DocumentTree, OutputConfig) => Unit, output: BinaryOutput): Unit = {
    
    def createSAXResult (out: OutputStream) = {
      val foUserAgent = fopFactory.newFOUserAgent
      foUserAgent.setURIResolver(new URIResolver {
        def resolve (uri: String, base: String) = (tree.sourcePaths.collectFirst {
          case source if (new File(source+uri)).isFile => new StreamSource(new File(source+uri))
        }).getOrElse(null)
      })
      val fop = fopFactory.newFop(MimeConstants.MIME_PDF, foUserAgent, out)
      new SAXResult(fop.getDefaultHandler())
    }
    
    def createTransformer = {
      val factory = TransformerFactory.newInstance
      factory.newTransformer // identity transformer
    }
    
    val fo = renderFO(tree, render)
    val out = output.asStream
    
    try {
      val source = new StreamSource(new StringReader(fo))
      val result = createSAXResult(out)

      createTransformer.transform(source, result)
    
    } finally {
      out.close()
    }
    
  }
  
  
}

/** The default instance of the PDF renderer.
 */
object PDF extends PDF(XSLFO.unformatted, PDFConfig.default)

/** Configuration options for the generated PDF output.
 * 
 *  @param treeTitles indicates whether a title will be inserted for each tree and subtree, 
 *  relying on titles specified in the tree configuration file
 *  @param docTitles indicates whether a title will be inserted for each document, 
 *  relying on `Title` elements in the document or titles specified in the document configuration header
 *  @param bookmarks indicates whether PDF bookmarks should be inserted into the PDF document
 *  @param toc indicates whether a table of content should be inserted at the beginning of the PDF document
 */
case class PDFConfig(treeTitles: Boolean = true, docTitles: Boolean = true, bookmarks: Boolean = true, toc: Boolean = true)

/** Companion for creation `PDFConfig` instances.
 */
object PDFConfig {
  
  /** The default configuration, with all optional features enabled.
   */
  val default: PDFConfig = apply()
}

