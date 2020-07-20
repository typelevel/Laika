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

import laika.helium.Helium

/**
  * @author Jens Halm
  */
object CSSVarGenerator {

  def generate (helium: Helium): String = {
    Seq(
      "primary-color" -> helium.colors.primary.displayValue,
      "primary-light" -> helium.colors.primaryLight.displayValue,
      "secondary-color" -> helium.colors.secondary.displayValue,
      "messages-info" -> helium.colors.messages.info.displayValue,
      "messages-info-light" -> helium.colors.messages.infoLight.displayValue,
      "messages-warning" -> helium.colors.messages.warning.displayValue,
      "messages-warning-light" -> helium.colors.messages.warningLight.displayValue,
      "messages-error" -> helium.colors.messages.error.displayValue,
      "messages-error-light" -> helium.colors.messages.errorLight.displayValue,
      "syntax-base1" -> helium.colors.syntaxHighlighting.base.c1.displayValue,
      "syntax-base2" -> helium.colors.syntaxHighlighting.base.c2.displayValue,
      "syntax-base3" -> helium.colors.syntaxHighlighting.base.c3.displayValue,
      "syntax-base4" -> helium.colors.syntaxHighlighting.base.c4.displayValue,
      "syntax-base5" -> helium.colors.syntaxHighlighting.base.c5.displayValue,
      "syntax-wheel1" -> helium.colors.syntaxHighlighting.wheel.c1.displayValue,
      "syntax-wheel2" -> helium.colors.syntaxHighlighting.wheel.c2.displayValue,
      "syntax-wheel3" -> helium.colors.syntaxHighlighting.wheel.c3.displayValue,
      "syntax-wheel4" -> helium.colors.syntaxHighlighting.wheel.c4.displayValue,
      "syntax-wheel5" -> helium.colors.syntaxHighlighting.wheel.c5.displayValue,
      "body-font" -> ("\"" + helium.themeFonts.body + "\""),
      "header-font" -> ("\"" + helium.themeFonts.headlines + "\""),
      "code-font" -> ("\"" + helium.themeFonts.code + "\""),
      "body-font-size" -> helium.fontSizes.body.displayValue,
      "code-font-size" -> helium.fontSizes.code.displayValue,
      "small-font-size" -> helium.fontSizes.small.displayValue,
      "title-font-size" -> helium.fontSizes.title.displayValue,
      "header2-font-size" -> helium.fontSizes.header2.displayValue,
      "header3-font-size" -> helium.fontSizes.header3.displayValue,
      "header4-font-size" -> helium.fontSizes.header4.displayValue,
      "content-width" -> helium.webLayout.contentWidth.displayValue,
      "nav-width" -> helium.webLayout.navigationWidth.displayValue,
      "block-spacing" -> helium.webLayout.defaultBlockSpacing.displayValue,
      "line-height" -> helium.webLayout.defaultLineHeight.toString
    )
      .map { case (name, value) => 
        s"  --$name: $value;"
      }
      .mkString(
        ":root {\n  ",
        "\n",
        "\n}"
    )
  }
  
}
