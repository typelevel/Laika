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

import java.io.{ByteArrayInputStream, File}

import laika.format.PDF
import laika.theme.{EmbeddedFontFile, EmbeddedFontResource}
import org.apache.fop.apps.FopFactory

/** Creates a FopFactory instance based on user configuration, registering all fonts to be embedded into
  * the PDF.
  * 
  * @author Jens Halm
  */
object FopFactoryBuilder {

  def build (config: PDF.BookConfig): FopFactory = {
    // since there is no API to define fonts for Apache FOP we have to generate configuration XML here
    val fontDefs = config.fonts.flatMap { font =>
      font.resource.embedResource.map { res =>
        val url = res match {
          case EmbeddedFontFile(file) => file.getAbsolutePath
          case EmbeddedFontResource(name) => s"classpath:$name"
        }
        s"""<font kerning="yes" embed-url="$url" embedding-mode="subset">
            |  <font-triplet name="${font.family}" style="${font.style.value}" weight="${font.weight.value}"/>
            |</font>""".stripMargin
      }.mkString("\n        ")
    }
    val xmlConf =
      s"""<fop version="1.0">
        |  <renderers>
        |    <renderer mime="application/pdf">
        |      <fonts>
        |        $fontDefs
        |      </fonts>
        |    </renderer>
        |  </renderers>
        |</fop>
      """.stripMargin
    FopFactory.newInstance(new File(".").toURI, new ByteArrayInputStream(xmlConf.getBytes))
  }
  
}
