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

package laika.helium.generate

import laika.ast.RelativePath
import laika.ast.Path.Root
import laika.helium.config.{CommonSettings, EPUBSettings, SiteSettings}
import laika.theme.config.FontDefinition

private[helium] object CSSVarGenerator {

  private def generateFontFace (fontDef: FontDefinition, path: RelativePath): String = 
    s"""@font-face {
        |  font-family: "${fontDef.family}";
        |  font-weight: ${fontDef.weight.value.toLowerCase};
        |  font-style: ${fontDef.style.value.toLowerCase};
        |  src: url("$path");
        |}""".stripMargin
  
  def generate (settings: SiteSettings): String = {
    import settings.layout._
    val layoutStyles = Seq(
      "content-width" -> contentWidth.displayValue,
      "nav-width" -> navigationWidth.displayValue
    )
    generate(settings, layoutStyles)
  }
  
  def generate (settings: EPUBSettings): String = {
    val embeddedFonts = settings.bookConfig.fonts.flatMap { font =>
      font.resource.embedResource.map { res =>
        generateFontFace(font, res.path.relativeTo(Root / "helium" / "laika-helium.epub.css"))
      }
    }.mkString("", "\n\n", "\n\n")
    embeddedFonts + generate(settings, Nil)
  }

  def generate (common: CommonSettings, additionalStyles: Seq[(String, String)]): String = {
    import common._
    (Seq(
      "primary-color" -> colors.primary.displayValue,
      "primary-light" -> colors.primaryLight.displayValue,
      "primary-medium" -> colors.primaryMedium.displayValue,
      "primary-dark" -> colors.primaryDark.displayValue,
      "secondary-color" -> colors.secondary.displayValue,
      "text-color" -> colors.text.displayValue,
      "messages-info" -> colors.messages.info.displayValue,
      "messages-info-light" -> colors.messages.infoLight.displayValue,
      "messages-warning" -> colors.messages.warning.displayValue,
      "messages-warning-light" -> colors.messages.warningLight.displayValue,
      "messages-error" -> colors.messages.error.displayValue,
      "messages-error-light" -> colors.messages.errorLight.displayValue,
      "syntax-base1" -> colors.syntaxHighlighting.base.c1.displayValue,
      "syntax-base2" -> colors.syntaxHighlighting.base.c2.displayValue,
      "syntax-base3" -> colors.syntaxHighlighting.base.c3.displayValue,
      "syntax-base4" -> colors.syntaxHighlighting.base.c4.displayValue,
      "syntax-base5" -> colors.syntaxHighlighting.base.c5.displayValue,
      "syntax-wheel1" -> colors.syntaxHighlighting.wheel.c1.displayValue,
      "syntax-wheel2" -> colors.syntaxHighlighting.wheel.c2.displayValue,
      "syntax-wheel3" -> colors.syntaxHighlighting.wheel.c3.displayValue,
      "syntax-wheel4" -> colors.syntaxHighlighting.wheel.c4.displayValue,
      "syntax-wheel5" -> colors.syntaxHighlighting.wheel.c5.displayValue,
      "body-font" -> ("\"" + themeFonts.body + "\""),
      "header-font" -> ("\"" + themeFonts.headlines + "\""),
      "code-font" -> ("\"" + themeFonts.code + "\""),
      "body-font-size" -> fontSizes.body.displayValue,
      "code-font-size" -> fontSizes.code.displayValue,
      "small-font-size" -> fontSizes.small.displayValue,
      "title-font-size" -> fontSizes.title.displayValue,
      "header2-font-size" -> fontSizes.header2.displayValue,
      "header3-font-size" -> fontSizes.header3.displayValue,
      "header4-font-size" -> fontSizes.header4.displayValue,
      "block-spacing" -> common.layout.defaultBlockSpacing.displayValue,
      "line-height" -> common.layout.defaultLineHeight.toString
    ) ++ additionalStyles)
      .map { case (name, value) => 
        s"  --$name: $value;"
      }
      .mkString(
        ":root {\n  ",
        "\n",
        "\n}\n\n"
    )
  }
  
}
