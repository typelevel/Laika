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

package laika.render

import laika.format.PDF
import laika.helium.Helium
import laika.helium.internal.generate.ConfigGenerator
import laika.pdf.internal.FopFactoryBuilder
import laika.theme.config.{ BookConfig, Font, FontDefinition, FontStyle, FontWeight }
import munit.FunSuite

/** @author Jens Halm
  */
class FopFactoryConfigSpec extends FunSuite {

  def renderXML(helium: Helium): String = {
    val config    = ConfigGenerator.populateConfig(helium)
    val pdfConfig = BookConfig.decodeWithDefaults(config, PDF.configKey).getOrElse(BookConfig.empty)
    FopFactoryBuilder.generateXMLConfig(pdfConfig)
  }

  test("defaults") {
    val expected =
      """<fop version="1.0">
        |  <renderers>
        |    <renderer mime="application/pdf">
        |      <fonts>
        |        <font kerning="yes" embed-url="/laika/fonts/Lato-Regular.ttf" embedding-mode="subset">
        |          <font-triplet name="Lato" style="normal" weight="normal"/>
        |        </font>
        |        <font kerning="yes" embed-url="/laika/fonts/Lato-Italic.ttf" embedding-mode="subset">
        |          <font-triplet name="Lato" style="italic" weight="normal"/>
        |        </font>
        |        <font kerning="yes" embed-url="/laika/fonts/Lato-Bold.ttf" embedding-mode="subset">
        |          <font-triplet name="Lato" style="normal" weight="bold"/>
        |        </font>
        |        <font kerning="yes" embed-url="/laika/fonts/Lato-BoldItalic.ttf" embedding-mode="subset">
        |          <font-triplet name="Lato" style="italic" weight="bold"/>
        |        </font>
        |        <font kerning="yes" embed-url="/laika/fonts/FiraMono-Medium.otf" embedding-mode="subset">
        |          <font-triplet name="Fira Mono" style="normal" weight="normal"/>
        |        </font>
        |        <font kerning="yes" embed-url="/laika/fonts/icofont.ttf" embedding-mode="subset">
        |          <font-triplet name="IcoFont" style="normal" weight="normal"/>
        |        </font>
        |      </fonts>
        |    </renderer>
        |  </renderers>
        |</fop>""".stripMargin
    assertEquals(renderXML(Helium.defaults), expected)
  }

  private val customXML =
    """<fop version="1.0">
      |  <renderers>
      |    <renderer mime="application/pdf">
      |      <fonts>
      |        <font kerning="yes" embed-url="/laika/fonts/font-1.tff" embedding-mode="subset">
      |          <font-triplet name="Font-1" style="normal" weight="normal"/>
      |        </font>
      |        <font kerning="yes" embed-url="/laika/fonts/font-2.tff" embedding-mode="subset">
      |          <font-triplet name="Font-2" style="italic" weight="bold"/>
      |        </font>
      |      </fonts>
      |    </renderer>
      |  </renderers>
      |</fop>""".stripMargin

  test("custom fonts via 'pdf' selector - removing default theme fonts") {
    val helium = Helium.defaults.pdf.clearFontResources.pdf.addFontResources(
      FontDefinition(
        Font.withEmbeddedFile("/projects/fonts/font-1.tff"),
        "Font-1",
        FontWeight.Normal,
        FontStyle.Normal
      ),
      FontDefinition(
        Font.withEmbeddedFile("/projects/fonts/font-2.tff"),
        "Font-2",
        FontWeight.Bold,
        FontStyle.Italic
      )
    )
    assertEquals(renderXML(helium), customXML)
  }

  test("custom fonts via 'all' selector - removing default theme fonts") {
    val helium = Helium.defaults.all.clearFontResources.all.addFontResources(
      FontDefinition(
        Font.withEmbeddedFile("/projects/fonts/font-1.tff"),
        "Font-1",
        FontWeight.Normal,
        FontStyle.Normal
      ),
      FontDefinition(
        Font.withEmbeddedFile("/projects/fonts/font-2.tff"),
        "Font-2",
        FontWeight.Bold,
        FontStyle.Italic
      )
    )
    assertEquals(renderXML(helium), customXML)
  }

}
