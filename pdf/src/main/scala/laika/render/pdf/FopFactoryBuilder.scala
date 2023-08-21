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

import java.io.{ ByteArrayInputStream, File }
import cats.effect.Async
import cats.effect.std.Dispatcher
import laika.io.model.BinaryInput
import laika.theme.config.BookConfig
import org.apache.fop.apps.{ FopConfParser, FopFactory }

/** Creates a FopFactory instance based on user configuration, registering all fonts to be embedded into the PDF.
  *
  * @author Jens Halm
  */
private[laika] object FopFactoryBuilder {

  def generateXMLConfig(config: BookConfig): String = {
    // since there is no API to define fonts for Apache FOP we have to generate configuration XML here
    val fontDefs = config.fonts.flatMap { font =>
      font.resource.embedResource.map { res =>
        s"""        <font kerning="yes" embed-url="${res.path}" embedding-mode="subset">
           |          <font-triplet name="${font.family}" style="${font.style.value}" weight="${
            font.weight.value
          }"/>
           |        </font>""".stripMargin
      }
    }.mkString("\n").stripPrefix("        ")

    s"""<fop version="1.0">
       |  <renderers>
       |    <renderer mime="application/pdf">
       |      <fonts>
       |        $fontDefs
       |      </fonts>
       |    </renderer>
       |  </renderers>
       |</fop>""".stripMargin
  }

  def build[F[_]: Async](
      config: BookConfig,
      staticDocs: Seq[BinaryInput[F]],
      dispatcher: Dispatcher[F]
  ): F[FopFactory] = {

    val confInput = new ByteArrayInputStream(generateXMLConfig(config).getBytes)
    val resolver  = new FopResourceResolver(staticDocs, dispatcher)

    Async[F].delay {
      val parser = new FopConfParser(confInput, new File(".").toURI, resolver)
      parser.getFopFactoryBuilder.build()
    }
  }

}
