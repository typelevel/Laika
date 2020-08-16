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

package laika.render.pdf

import java.io.{InputStream, OutputStream, StringReader}
import java.net.URI

import cats.effect.Sync
import cats.implicits._
import javax.xml.transform.sax.SAXResult
import javax.xml.transform.stream.StreamSource
import javax.xml.transform.{Transformer, TransformerFactory}
import laika.ast.{DocumentMetadata, Path}
import laika.io.model.{BinaryInput, BinaryOutput}
import laika.io.runtime.Runtime
import org.apache.fop.apps.io.ResourceResolverFactory
import org.apache.fop.apps.{FOUserAgent, FOUserAgentFactory, FopFactory}
import org.apache.xmlgraphics.io.{Resource, ResourceResolver}
import org.apache.xmlgraphics.util.MimeConstants

/** Responsible for the final step in producing the binary PDF format from a single XSL-FO input stream 
  * that represents the entire document and its navigation elements.
  * 
  * @author Jens Halm
  */
class PDFRenderer (fopFactory: FopFactory) {

  /** Render the given XSL-FO input as a PDF to the specified binary output. 
    *
    *  @param foInput the input in XSL-FO format
    *  @param output the output to write the final result to
    *  @param metadata the metadata associated with the PDF
    *  @param staticDocuments additional files like fonts or images that the renderer should resolve for FOP
    *  which will be used to resolve relative paths
    */
  def render[F[_] : Sync: Runtime] (foInput: String, output: BinaryOutput[F], metadata: DocumentMetadata, staticDocuments: Seq[BinaryInput[F]] = Nil): F[Unit] = {

    def applyMetadata (agent: FOUserAgent): F[Unit] = Sync[F].delay {
      metadata.date.foreach(d => agent.setCreationDate(d))
      metadata.authors.headOption.foreach(a => agent.setAuthor(a))
      metadata.title.foreach(t => agent.setTitle(t))
    }

    def createSAXResult (out: OutputStream): F[SAXResult] = 
      for {
        foUserAgent <- Sync[F].delay(FOUserAgentFactory.createFOUserAgent(fopFactory, new FopResourceResolver(staticDocuments)))
        _           <- applyMetadata(foUserAgent)
        fop         <- Sync[F].delay(fopFactory.newFop(MimeConstants.MIME_PDF, foUserAgent, out))
      } yield new SAXResult(fop.getDefaultHandler)

    def createTransformer: F[Transformer] = Sync[F].delay {
      val factory = TransformerFactory.newInstance
      factory.newTransformer // identity transformer
    }
    
    Runtime[F].runBlocking {
      output.resource.use { out =>
        for {
          source      <- Sync[F].delay(new StreamSource(new StringReader(foInput)))
          result      <- createSAXResult(out)
          transformer <- createTransformer
          _           <- Sync[F].delay(transformer.transform(source, result))
        } yield ()
      }
    }

  }
  
}
