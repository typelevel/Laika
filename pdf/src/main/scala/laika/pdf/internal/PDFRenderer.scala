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

package laika.pdf.internal

import cats.effect.std.Dispatcher
import cats.effect.{ Async, Sync }
import cats.implicits.*
import laika.io.model.{ BinaryInput, BinaryOutput }
import laika.theme.config.DocumentMetadata
import org.apache.fop.apps.{ FOUserAgent, FOUserAgentFactory, FopFactory }
import org.apache.xmlgraphics.util.MimeConstants

import java.io.{ OutputStream, StringReader }
import javax.xml.transform.sax.SAXResult
import javax.xml.transform.stream.StreamSource
import javax.xml.transform.{ Transformer, TransformerFactory }

/** Responsible for the final step in producing the binary PDF format from a single XSL-FO input stream
  * that represents the entire document and its navigation elements.
  *
  * @author Jens Halm
  */
private[laika] class PDFRenderer[F[_]: Async](fopFactory: FopFactory, dispatcher: Dispatcher[F]) {

  /** Render the given XSL-FO input as a PDF to the specified binary output.
    *
    *  @param foInput the input in XSL-FO format
    *  @param output the output to write the final result to
    *  @param metadata the metadata associated with the PDF
    *  @param staticDocuments additional files like fonts or images that the renderer should resolve for FOP
    *  which will be used to resolve relative paths
    */
  def render(
      foInput: String,
      output: BinaryOutput[F],
      metadata: DocumentMetadata,
      staticDocuments: Seq[BinaryInput[F]] = Nil
  ): F[Unit] = {

    def applyMetadata(agent: FOUserAgent): F[Unit] = Sync[F].delay {
      metadata.dateModified.orElse(metadata.datePublished).foreach(d =>
        agent.setCreationDate(new java.util.Date(d.toInstant.toEpochMilli))
      )
      metadata.authors.headOption.foreach(a => agent.setAuthor(a))
      metadata.title.foreach(t => agent.setTitle(t))
    }

    def createSAXResult(out: OutputStream): F[SAXResult] =
      for {
        foUserAgent <- Async[F].delay(
          FOUserAgentFactory.createFOUserAgent(
            fopFactory,
            new FopResourceResolver(staticDocuments, dispatcher)
          )
        )
        _           <- applyMetadata(foUserAgent)
        fop         <- Async[F].delay(fopFactory.newFop(MimeConstants.MIME_PDF, foUserAgent, out))
      } yield new SAXResult(fop.getDefaultHandler)

    def createTransformer: F[Transformer] = Sync[F].delay {
      val factory = TransformerFactory.newInstance
      factory.newTransformer // identity transformer
    }

    output.resource.use { out =>
      for {
        source      <- Async[F].delay(new StreamSource(new StringReader(foInput)))
        result      <- createSAXResult(out)
        transformer <- createTransformer
        _           <- Async[F].blocking(transformer.transform(source, result))
      } yield ()
    }

  }

}
