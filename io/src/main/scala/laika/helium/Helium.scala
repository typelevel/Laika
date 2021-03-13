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

package laika.helium

import laika.helium.builder.HeliumThemeBuilder
import laika.helium.config._
import laika.theme._

/** Configuration API for the Helium theme settings.
  * 
  * Helium can be fully configured with its Scala API and does not require any configuration files.
  *
  * The entry point for all configuration steps is always `Helium.defaults` to ensure there are sensible defaults
  * in place for all the options you omit in your configuration.
  *
  * For each configuration step you need to choose one of the four selectors, 
  * either `all` to specify options for all three output formats or `site`, `epub` or `pdf` to select a single format.
  * Not all options are available for all formats, but the IDE's context help and the documentation below can guide you.
  *
  * In the minimal example below we only specify some metadata for all formats as well as the navigation depth
  * for EPUB and PDF:
  *
  * ```scala
  * val theme = Helium.defaults
  * .all.metadata(
  * title = Some("Project Name"),
  * language = Some("de"),
  * )
  * .epub.navigationDepth(4)
  * .pdf.navigationDepth(4)
  * .build
  * ```
  *
  * Laika also provides convenient constructors for some of the data types used frequently in its theme API.
  * You can import `laika.theme.Color._` for specifying colors with `hex("ffaaff")` or `rgb(255, 0, 0)` and 
  * `laika.ast.LengthUnit._` for specifying sizes with `px(12)` or `pt(9)` or other available units.
  *
  * The final call to `build` produces a `ThemeProvider` that can be passed to Laika's transformers
  * or the `laikaTheme` sbt setting:
  *
  * Example for sbt plugin:
  * {{{
  *   laikaTheme := theme
  * }}}
  * 
  * Example for the library API:
  * {{{
  * val transformer = Transformer
  *   .from(Markdown)
  *   .to(EPUB)
  *   .parallel[IO]
  *   .witTheme(theme)
  *   .build
  * }}}
  * 
  * @author Jens Halm
  */
class Helium private[laika] (private[laika] val siteSettings: SiteSettings,
                             private[laika] val epubSettings: EPUBSettings,
                             private[laika] val pdfSettings: PDFSettings) { self =>

  /** Selects the configuration options available for site generation.
    */
  object site extends SiteOps {
    protected def helium: Helium = self
  }

  /** Selects the configuration options available for EPUB generation.
    */
  object epub extends EPUBOps {
    protected def helium: Helium = self
  }

  /** Selects the configuration options available for PDF generation.
    */
  object pdf extends PDFOps {
    protected def helium: Helium = self
  }

  /** Selects the configuration options available for all three output formats.
    * 
    * This means that it only contains the options that exist in all three formats,
    * for anything specific to one or two formats you need to use their respective selectors.
    */
  object all extends AllFormatsOps {
    protected def helium: Helium = self
  }

  /** Builds a theme provider that can be passed to the sbt plugin's `laikaTheme` setting
    * or the `withTheme` method of parsers and transformers when using the library API.
    */
  def build: ThemeProvider = new HeliumThemeBuilder(this)
  
}

/** Entry point for the configuration API of the Helium theme.
  * 
  * See the documentation for the `Helium` class for a full introduction.
  */
object Helium {

  /** The defaults of the Helium theme which are always used as an entry point for any customizations.
    * 
    * If you do not override any defaults, the generated site and e-books will look like those for the
    * Laika manual, including all color and font choices.
    */
  val defaults: Helium = HeliumDefaults.instance
    
}
