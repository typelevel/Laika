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

package laika.render.fo

import laika.ast.Path.Root
import laika.ast.{/, StyleDeclarationSet, TemplateContextReference, TemplateRoot}
import laika.helium.Helium
import laika.helium.generate.FOStyles
import laika.parse.css.CSSParsers
import laika.rewrite.ReferenceResolver.CursorKeys
import laika.theme.config.{Font, FontDefinition, FontStyle, FontWeight}

object TestTheme {
  
  lazy val heliumTestProps = Helium.defaults.all.fontFamilies("serif", "sans-serif", "monospaced")
  lazy val foStyles: StyleDeclarationSet = CSSParsers.styleDeclarationSet
    .parse(new FOStyles(heliumTestProps).input)
    .map(StyleDeclarationSet(Set(FOStyles.defaultPath), _))
    .getOrElse(StyleDeclarationSet.empty) 
  lazy val foTemplate = TemplateRoot(
    TemplateContextReference(CursorKeys.fragment("bookmarks"), required = false),
    TemplateContextReference(CursorKeys.documentContent, required = true)
  )
  lazy val htmlTemplate = TemplateRoot.fallback
  val staticPaths = Seq(
    Root / "laika" / "fonts" / "Lato-Regular.ttf",
    Root / "laika" / "fonts" / "Lato-Italic.ttf", 
    Root / "laika" / "fonts" / "Lato-Bold.ttf", 
    Root / "laika" / "fonts" / "Lato-BoldItalic.ttf", 
    Root / "laika" / "fonts" / "FiraCode-Medium.otf", 
    Root / "laika" / "fonts" / "icofont.ttf",
    Root / "helium" / "laika-helium.js",
    Root / "helium" / "landing.page.css",
    Root / "helium" / "icofont.min.css", 
    Root / "helium" / "fonts"/ "icofont.woff", 
    Root / "helium" / "fonts"/ "icofont.woff2",
    Root / "helium" / "laika-helium.css",
    Root / "helium" / "laika-helium.epub.css"
  )
  val fonts = Seq(
    FontDefinition(Font.embedFile("/path/to/font-a.tff"), "Font-A", FontWeight.Normal, FontStyle.Normal),
    FontDefinition(Font.embedResource("/path/to/font-b.tff"), "Font-B", FontWeight.Bold, FontStyle.Normal),
    FontDefinition(Font.webCSS("http://fonts.com/font-c.css"), "Font-C", FontWeight.Normal, FontStyle.Italic)
  )
  
}
