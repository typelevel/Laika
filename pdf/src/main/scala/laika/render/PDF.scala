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
import laika.io.Output.Binary
import laika.io.OutputProvider.OutputConfig
import laika.io.OutputProvider.ResultTree
import laika.io.OutputProvider.StringOutputProvider
import laika.tree.Documents.Document
import laika.tree.Documents.DocumentTree

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
 *  @author Jens Halm
 */
class PDF extends RenderResultProcessor[FOWriter] {

  
  val factory = XSLFO.unformatted
  
  
  private val fopFactory = FopFactory.newInstance
  
  
  def process (tree: DocumentTree, render: OutputConfig => Unit, output: Output with Binary) = {
    
    def append (sb: StringBuilder, result: ResultTree, src: DocumentTree): Unit = {
      src.navigatables.foreach {
        case d: Document => result.result(d.name).foreach(sb.append)
        case t: DocumentTree => result.subtree(t.name).foreach(append(sb, _, t))
      }
    }
    
    def renderTree = {
      val foOutput = new StringOutputProvider(tree.path)
      render(OutputConfig(foOutput, false))
      val sb = new StringBuilder
      append(sb, foOutput.result, tree)
      sb.toString
    }
    
    def createSAXResult (out: OutputStream) = {
      val foUserAgent = fopFactory.newFOUserAgent
      foUserAgent.setBaseURL("") // TODO - need to introduce DocumentTree.sourcePath (Option[String])
      val fop = fopFactory.newFop(MimeConstants.MIME_PDF, foUserAgent, out)
      new SAXResult(fop.getDefaultHandler())
    }
    
    def createTransformer = {
      val factory = TransformerFactory.newInstance
      factory.newTransformer // identity transformer
    }
    
    val fo = renderTree
    val out = output.asBinaryOutput.asStream
    
    try {
      val source = new StreamSource(new StringReader(fo))
      val result = createSAXResult(out)

      createTransformer.transform(source, result)
    
    } finally {
      out.close()
    }
    
  }
  
  
}
