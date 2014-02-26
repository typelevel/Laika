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

import java.io.OutputStream
import java.io.StringReader
import javax.xml.transform.TransformerFactory
import javax.xml.transform.stream.StreamSource
import javax.xml.transform.sax.SAXResult
import laika.factory.RenderResultProcessor
import laika.tree.Documents.DocumentTree
import laika.io.Output
import laika.io.Output.BinaryOutput
import laika.io.OutputProvider.OutputConfig
import org.apache.fop.apps.FopFactory
import org.apache.xmlgraphics.util.MimeConstants
import javax.xml.transform.URIResolver
import java.io.File

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
 *  @author Jens Halm
 */
class PDF extends RenderResultProcessor[FOWriter] {

  
  val factory = XSLFO.unformatted
  
  
  private val fopFactory = FopFactory.newInstance
  
  
  protected def renderFO (tree: DocumentTree, render: (DocumentTree, OutputConfig) => Unit): String =
    FOforPDF.renderFO(tree, render)
  
    
  def process (tree: DocumentTree, render: (DocumentTree, OutputConfig) => Unit, output: BinaryOutput) = {
    
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
