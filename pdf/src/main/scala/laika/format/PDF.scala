/*
 * Copyright 2012-2019 the original author or authors.
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

import java.io.{File, FileOutputStream, OutputStream, StringReader}
import java.net.URI
import java.util.Date

import javax.xml.transform.TransformerFactory
import javax.xml.transform.sax.SAXResult
import javax.xml.transform.stream.StreamSource
import laika.ast.{DocumentMetadata, DocumentTree, SpanSequence}
import laika.execute.OutputExecutor
import laika.factory.{RenderFormat, RenderResultProcessor}
import laika.io.{BinaryOutput, RenderedTreeRoot}
import laika.render.{FOFormatter, FOforPDF}
import org.apache.fop.apps.{FOUserAgent, FOUserAgentFactory, FopFactory, FopFactoryBuilder}
import org.apache.xmlgraphics.io.{Resource, ResourceResolver}
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
 *  merged into a single output file.
 * 
 *  @author Jens Halm
 */
class PDF private(val format: RenderFormat[FOFormatter], config: Option[PDF.Config], fopFactory: Option[FopFactory]) extends RenderResultProcessor[FOFormatter] {


  /** Allows to specify configuration options like insertion
   *  of bookmarks or table of content.
   */
  def withConfig (config: PDF.Config): PDF = new PDF(format, Some(config), fopFactory)

  /** Allows to specify a custom FopFactory in case additional configuration
    * is required for custom fonts, stemmers or other FOP features.
    *
    * A `FopFactory` is a fairly heavy-weight object, so make sure that you reuse
    * either the `FopFactory` instance itself or the resulting `PDF` renderer.
    * In case you do not specify a custom factory, Laika ensures that the default
    * factory is reused between renderers.
    */
  def withFopFactory (fopFactory: FopFactory): PDF = new PDF(format, config, Some(fopFactory))
  
  private lazy val foForPDF = new FOforPDF(config)


  /** Adds PDF bookmarks and/or a table of content to the specified document tree, depending on configuration.
    * The modified tree will be used for rendering the interim XSL-FO result.
    */
  def prepareTree (tree: DocumentTree): DocumentTree = foForPDF.prepareTree(tree)

  /** Processes the interim XSL-FO result, transforms it to PDF and writes
    * it to the specified final output.
    *
    * @param result the result of the render operation as a tree
    * @param output the output to write the final result to
    */
  def process (result: RenderedTreeRoot, output: BinaryOutput): Unit = {

    val fo: String = foForPDF.renderFO(result, result.template)

    val metadata = DocumentMetadata.fromConfig(result.config)
    val title = if (result.title.isEmpty) None else Some(SpanSequence(result.title).extractText)

    renderPDF(fo, output, metadata, title, Nil) // TODO - 0.12 - add result.sourcePaths

  }

  /** Render the given XSL-FO input as a PDF to the specified
   *  binary output. The optional `sourcePaths` argument
   *  may be used to allow resolving relative paths for
   *  loading external files like images.
   * 
   *  @param foInput the input in XSL-FO format
   *  @param output the output to write the final result to
   *  @param metadata the metadata associated with the PDF
   *  @param title the title of the document
   *  @param sourcePaths the paths that may contain files like images
   *  which will be used to resolve relative paths
   */
  def renderPDF (foInput: String, output: BinaryOutput, metadata: DocumentMetadata, title: Option[String] = None, sourcePaths: Seq[String] = Nil): Unit = {

    def applyMetadata (agent: FOUserAgent): Unit = {
      metadata.date.foreach(d => agent.setCreationDate(Date.from(d)))
      metadata.authors.headOption.foreach(a => agent.setAuthor(a))
      title.foreach(t => agent.setTitle(t))
    }

    def createSAXResult (out: OutputStream) = {

      val resolver = new ResourceResolver {
        
        def getResource (uri: URI): Resource =
          new Resource(resolve(uri).toURL.openStream())
        
        def getOutputStream (uri: URI): OutputStream =
          new FileOutputStream(new File(resolve(uri)))
        
        def resolve (uri: URI): URI = sourcePaths.collectFirst {
          case source if new File(source + uri.getPath).isFile => new File(source + uri).toURI
        }.getOrElse(if (uri.isAbsolute) uri else new File(uri.getPath).toURI)
      }

      val factory = fopFactory.getOrElse(PDF.defaultFopFactory)
      val foUserAgent = FOUserAgentFactory.createFOUserAgent(factory, resolver)
      applyMetadata(foUserAgent)
      val fop = factory.newFop(MimeConstants.MIME_PDF, foUserAgent, out)
      new SAXResult(fop.getDefaultHandler)
    }
    
    def createTransformer = {
      val factory = TransformerFactory.newInstance
      factory.newTransformer // identity transformer
    }
    
    val out = OutputExecutor.asStream(output)
    
    try {
      val source = new StreamSource(new StringReader(foInput))
      val result = createSAXResult(out)

      createTransformer.transform(source, result)
    
    } finally {
      out.close()
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

