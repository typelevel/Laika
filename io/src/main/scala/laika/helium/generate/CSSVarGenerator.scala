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
import laika.helium.config.{ColorSet, CommonSettings, DarkModeSupport, EPUBSettings, SiteSettings}
import laika.theme.config.FontDefinition

private[helium] object CSSVarGenerator {

  private val darkModeMediaQuery = "@media (prefers-color-scheme: dark) {"
  
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
      "content-width"  -> contentWidth.displayValue,
      "nav-width"      -> navigationWidth.displayValue,
      "top-bar-height" -> topBarHeight.displayValue
    )
    generate(settings, layoutStyles, settings.layout.topNavigationBar.highContrast)
  }
  
  def generate (settings: EPUBSettings): String = {
    val embeddedFonts = settings.bookConfig.fonts.flatMap { font =>
      font.resource.embedResource.map { res =>
        generateFontFace(font, res.path.relativeTo(Root / "helium" / "laika-helium.epub.css"))
      }
    }.mkString("", "\n\n", "\n\n")
    embeddedFonts + generate(settings, Nil, topBarHighContrast = false)
  }
  
  private def toVars (pairs: Seq[(String, String)]): Seq[(String, String)] = pairs.map { 
    case (name, value) => (s"--$name", value)
  }

  private def renderStyles (styles: Seq[(String, String)], darkMode: Boolean = false): String = {
    val renderedStyles = styles.map { case (name, value) =>
      s"$name: $value;"
    }
    
    if (darkMode) renderedStyles.mkString(s"$darkModeMediaQuery\n  :root {\n    ", "\n    ", "\n  }\n}\n\n")
    else renderedStyles.mkString(":root {\n  ", "\n  ", "\n}\n\n")
  }
  
  def colorSet (colors: ColorSet, topBarHighContrast: Boolean): Seq[(String, String)] = {
    import colors._
    def ref (name: String): String = s"var(--$name)"
    Seq(
      "primary-color" -> theme.primary.displayValue,
      "primary-light" -> theme.primaryLight.displayValue,
      "primary-medium" -> theme.primaryMedium.displayValue,
      "primary-dark" -> theme.primaryDark.displayValue,
      "secondary-color" -> theme.secondary.displayValue,
      "text-color" -> theme.text.displayValue,
      "bg-color" -> theme.background.displayValue,
      "top-color" ->  (if (topBarHighContrast) ref("primary-light") else ref("primary-color")),
      "top-bg" ->     (if (topBarHighContrast) ref("primary-color") else ref("primary-light")),
      "top-hover" ->  (if (topBarHighContrast) ref("bg-color") else ref("secondary-color")),
      "top-border" -> (if (topBarHighContrast) ref("bg-color") else ref("primary-medium")),
      "messages-info" -> messages.info.displayValue,
      "messages-info-light" -> messages.infoLight.displayValue,
      "messages-warning" -> messages.warning.displayValue,
      "messages-warning-light" -> messages.warningLight.displayValue,
      "messages-error" -> messages.error.displayValue,
      "messages-error-light" -> messages.errorLight.displayValue,
      "syntax-base1" -> syntaxHighlighting.base.c1.displayValue,
      "syntax-base2" -> syntaxHighlighting.base.c2.displayValue,
      "syntax-base3" -> syntaxHighlighting.base.c3.displayValue,
      "syntax-base4" -> syntaxHighlighting.base.c4.displayValue,
      "syntax-base5" -> syntaxHighlighting.base.c5.displayValue,
      "syntax-wheel1" -> syntaxHighlighting.wheel.c1.displayValue,
      "syntax-wheel2" -> syntaxHighlighting.wheel.c2.displayValue,
      "syntax-wheel3" -> syntaxHighlighting.wheel.c3.displayValue,
      "syntax-wheel4" -> syntaxHighlighting.wheel.c4.displayValue,
      "syntax-wheel5" -> syntaxHighlighting.wheel.c5.displayValue
    )
  }

  def generate (common: DarkModeSupport, additionalVars: Seq[(String, String)], topBarHighContrast: Boolean): String = {
    import common._
    val vars = 
      colorSet(common.colors, topBarHighContrast) ++ 
      Seq(
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
      ) ++ 
      additionalVars
      
    val (colorScheme, darkModeStyles) = common.darkMode match {
      case Some(darkModeColors) => (Seq(("color-scheme", "light dark")), 
                                    renderStyles(toVars(colorSet(darkModeColors, topBarHighContrast)), darkMode = true))
      case None                 => (Nil, "")
    }
    
    renderStyles(toVars(vars) ++ colorScheme) + darkModeStyles
  }
  
}
