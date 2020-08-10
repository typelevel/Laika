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
import laika.ast.TemplateRoot
import laika.helium.Helium
import laika.helium.generate.{FOStyles, FOTemplate}
import laika.io.theme.{Font, FontDefinition, FontStyle, FontWeight, ThemeFonts}

object TestTheme {
  
  lazy val heliumTestProps = Helium.defaults.copy(themeFonts = ThemeFonts("serif", "sans-serif", "monospaced"))
  lazy val foStyles = new FOStyles(heliumTestProps).styles 
  lazy val foTemplate = new FOTemplate(heliumTestProps).root
  lazy val htmlTemplate = TemplateRoot.fallback
  val staticPaths = Seq(
    Root / "css" / "container.css", 
    Root / "css" / "content.css", 
    Root / "css" / "nav.css", 
    Root / "css" / "code.css", 
    Root / "css" / "toc.css", 
    Root / "css" / "vars.css"
  )
  val fonts = Seq(
    FontDefinition(Font.embedFile("/path/to/font-a.tff"), "Font-A", FontWeight.Normal, FontStyle.Normal),
    FontDefinition(Font.embedResource("/path/to/font-b.tff"), "Font-B", FontWeight.Bold, FontStyle.Normal),
    FontDefinition(Font.webCSS("http://fonts.com/font-c.css"), "Font-C", FontWeight.Normal, FontStyle.Italic)
  )
  
}
