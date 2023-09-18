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

package laika.helium.internal.builder

import cats.effect.{Async, Resource}
import laika.api.bundle.*
import laika.ast.{Document, OutputContext}
import laika.ast.Path.Root
import laika.format.{EPUB, HTML, XSLFO}
import laika.helium.Helium
import laika.helium.internal.generate.ConfigGenerator
import laika.theme.{Theme, ThemeBuilder, ThemeProvider}

/** @author Jens Halm
  */
private[helium] class HeliumThemeBuilder(helium: Helium) extends ThemeProvider {

  private val heliumWithIncludes = {
    import HeliumInputBuilder.*
    def isLandingPage(doc: Document): Boolean =
      doc.path.parent == Root && doc.path.basename == "README" // TODO - 1.0 - better set a flag in the LandingPageGenerator to distinguish from user inputs with this path
    helium
      .site.internalJS(Root / "helium") // picks up all JS in the directory
      .site.internalCSS(paths.icoFontCSS)
      .site.internalCSS(paths.siteCSS)
      .site.internalCSS(paths.landingCSS, condition = isLandingPage)
      .epub.internalCSS(paths.epubCSS)
  }

  object directives extends DirectiveRegistry {
    override def origin: BundleOrigin                   = BundleOrigin.Theme
    override val description: String                    = "Directives for theme 'Helium'"
    val spanDirectives: Seq[SpanDirectives.Directive]   = Nil
    val blockDirectives: Seq[BlockDirectives.Directive] = Nil

    val templateDirectives: Seq[TemplateDirectives.Directive] =
      HeliumDirectives.all(heliumWithIncludes)

    val linkDirectives: Seq[LinkDirectives.Directive] = Nil
  }

  def build[F[_]: Async]: Resource[F, Theme[F]] = {

    import helium._

    val treeProcessor = new HeliumTreeProcessor[F](helium)

    ThemeBuilder("Helium")
      .addInputs(HeliumInputBuilder.build(helium))
      .addBaseConfig(ConfigGenerator.populateConfig(helium))
      .addRewriteRules(HeliumRewriteRules.build(helium))
      .addRenderOverrides(
        HTML.Overrides(HeliumRenderOverrides.forHTML(siteSettings.layout.anchorPlacement))
      )
      .addRenderOverrides(EPUB.XHTML.Overrides(HeliumRenderOverrides.forEPUB))
      .addRenderOverrides(XSLFO.Overrides(HeliumRenderOverrides.forPDF))
      .addExtensions(directives)
      .processTree(treeProcessor.forHTML, OutputContext(HTML))
      .processTree(treeProcessor.forAllFormats)
      .build

  }

}
