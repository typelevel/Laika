/*
 * Copyright 2014-2016 the original author or authors.
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
import laika.io.Input
import laika.io.Output
import laika.io.Output.BinaryOutput
import laika.io.OutputProvider.OutputConfig
import laika.tree.Documents.DocumentTree
import laika.tree.Elements.Element
import laika.tree.Elements.MessageLevel
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStream
import java.io.StringReader
import java.net.URI
import javax.xml.transform.TransformerFactory
import javax.xml.transform.URIResolver
import javax.xml.transform.sax.SAXResult
import javax.xml.transform.stream.StreamSource
import org.apache.fop.apps.FopFactory
import org.apache.fop.apps.FopFactoryBuilder
import org.apache.fop.apps.FOUserAgentFactory
import org.apache.xmlgraphics.io.Resource
import org.apache.xmlgraphics.io.ResourceResolver
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
class PDF private (val factory: XSLFO, config: Option[PDFConfig]) extends RenderResultProcessor[FOWriter] {

  
  private lazy val fopFactory =
    new FopFactoryBuilder(new File(".").toURI()).build()
  
  
  /** Specifies the minimum required level for a system message
   *  to get included into the output by this renderer.
   */
  def withMessageLevel (level: MessageLevel): PDF = new PDF(factory.withMessageLevel(level), config)
  
  /** Allows to specify configuration options like insertion
   *  of bookmarks or table of content.
   */
  def withConfig (config: PDFConfig): PDF = new PDF(factory, Some(config))
  
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
    
    val fo = renderFO(tree, render)
    
    renderPDF(Input.fromString(fo), output, tree.sourcePaths)
    
  }
  
  /** Render the given XSL-FO input as a PDF to the specified
   *  binary output. The optional `sourcePaths` argument
   *  may be used to allow resolving relative paths for
   *  loading external files like images.
   * 
   *  @param foInput the input in XSL-FO format
   *  @param output the output to write the final result to
   *  @param sourcePaths the paths that may contain files like images
   *  which will be used to resolve relative paths
   */
  def renderPDF (foInput: Input, output: BinaryOutput, sourcePaths: Seq[String] = Nil): Unit = {
    
    def createSAXResult (out: OutputStream) = {
      val resolver = new ResourceResolver {
        
        def getResource (uri: URI): Resource =
          new Resource(resolve(uri).toURL().openStream())
        
        def getOutputStream (uri: URI): OutputStream =
          new FileOutputStream(new File(resolve(uri)))
        
        def resolve (uri: URI): URI = (sourcePaths.collectFirst {
          case source if (new File(source+uri.getPath)).isFile => new File(source+uri).toURI
        }).getOrElse(if (uri.isAbsolute) uri else new File(uri.getPath).toURI)
      }
      val foUserAgent = FOUserAgentFactory.createFOUserAgent(fopFactory, resolver)
      val fop = fopFactory.newFop(MimeConstants.MIME_PDF, foUserAgent, out)
      new SAXResult(fop.getDefaultHandler())
    }
    
    def createTransformer = {
      val factory = TransformerFactory.newInstance
      factory.newTransformer // identity transformer
    }
    
    val out = output.asStream
    
    try {
      val source = new StreamSource(foInput.asReader)
      val result = createSAXResult(out)

      createTransformer.transform(source, result)
    
    } finally {
      out.close()
    }
    
  }
  
  
}

/** The default instance of the PDF renderer.
 */
object PDF extends PDF(XSLFO.unformatted, None)

/** Configuration options for the generated PDF output.
 * 
 *  @param insertTitles indicates whether a title will be inserted for each tree, subtree,
 *  and document, relying on titles specified in the configuration file for the tree
 *  or document, but only if there is no `Title` element in the document already
 *  @param bookmarkDepth the number of levels bookmarks should be generated for, use 0 to switch off bookmark generation
 *  @param tocDepth the number of levels to generate a table of contents for, use 0 to switch off toc generation
 *  @param tocTitle the title for the table of contents
 */
case class PDFConfig(insertTitles: Boolean = true, bookmarkDepth: Int = Int.MaxValue, tocDepth: Int = Int.MaxValue, tocTitle: Option[String] = None)

/** Companion for the creation of `PDFConfig` instances.
 */
object PDFConfig {
  
  /** The default configuration, with all optional features enabled.
   */
  val default: PDFConfig = apply()
}
