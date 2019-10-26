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

package laika.render.pdf

import java.io.{File, FileOutputStream, OutputStream, StringReader}
import java.net.URI
import java.util.Date

import cats.effect.Async
import cats.implicits._
import javax.xml.transform.sax.SAXResult
import javax.xml.transform.stream.StreamSource
import javax.xml.transform.{Transformer, TransformerFactory}
import laika.ast.DocumentMetadata
import laika.format.PDF
import laika.io.model.BinaryOutput
import laika.io.runtime.Runtime
import org.apache.fop.apps.{FOUserAgent, FOUserAgentFactory, FopFactory}
import org.apache.xmlgraphics.io.{Resource, ResourceResolver}
import org.apache.xmlgraphics.util.MimeConstants

/** Responsible for the final step in producing the binary PDF format from
  * a single XSL-FO input stream that represents the entire document and
  * its navigation elements.
  * 
  * @author Jens Halm
  */
class PDFRenderer (config: Option[PDF.Config], fopFactory: Option[FopFactory]) {

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
  def render[F[_] : Async: Runtime] (foInput: String, output: BinaryOutput[F], metadata: DocumentMetadata, title: Option[String] = None, sourcePaths: Seq[String] = Nil): F[Unit] = {

    def applyMetadata (agent: FOUserAgent): F[Unit] = Async[F].delay {
      metadata.date.foreach(d => agent.setCreationDate(Date.from(d)))
      metadata.authors.headOption.foreach(a => agent.setAuthor(a))
      title.foreach(t => agent.setTitle(t))
    }

    def createSAXResult (out: OutputStream): F[SAXResult] = {

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
      for {
        foUserAgent <- Async[F].delay(FOUserAgentFactory.createFOUserAgent(factory, resolver))
        _           <- applyMetadata(foUserAgent)
        fop         <- Async[F].delay(factory.newFop(MimeConstants.MIME_PDF, foUserAgent, out))
      } yield new SAXResult(fop.getDefaultHandler)

    }

    def createTransformer: F[Transformer] = Async[F].delay {
      val factory = TransformerFactory.newInstance
      factory.newTransformer // identity transformer
    }

    Runtime[F].runBlocking {
      output.output.use { out =>
        for {
          source      <- Async[F].delay(new StreamSource(new StringReader(foInput)))
          result      <- createSAXResult(out)
          transformer <- createTransformer
          _           <- Async[F].delay(transformer.transform(source, result))
        } yield ()
      }
    }

  }
  
}
