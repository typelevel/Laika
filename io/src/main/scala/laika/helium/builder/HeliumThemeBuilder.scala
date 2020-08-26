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

package laika.helium.builder

import cats.effect.{Resource, Sync}
import laika.ast._
import laika.bundle.{BundleOrigin, ExtensionBundle}
import laika.config.Config
import laika.format.{HTML, XSLFO}
import laika.helium.Helium
import laika.helium.generate._
import laika.theme.Theme

/**
  * @author Jens Halm
  */
private[helium] object HeliumThemeBuilder {

  def build[F[_]: Sync](helium: Helium): Resource[F, Theme[F]] = {

    import helium._

    val bundle: ExtensionBundle = new ExtensionBundle {
      override val origin: BundleOrigin = BundleOrigin.Theme
      val description = "Helium Theme Rewrite Rules and Render Overrides"
      override val rewriteRules: Seq[DocumentCursor => RewriteRules] = HeliumRewriteRules.build(pdfSettings)
      override val renderOverrides = Seq(
        HTML.Overrides(HeliumRenderOverrides.forHTML(siteSettings.webLayout.anchorPlacement)),
        XSLFO.Overrides(HeliumRenderOverrides.forPDF)
      )
      override val baseConfig: Config = ConfigGenerator.populateConfig(helium)
    }

    val treeProcessor = new HeliumTreeProcessor[F](helium)

    Theme(HeliumInputBuilder.build(helium), bundle).processTree {
      case HTML => treeProcessor.forHTML
      case format => treeProcessor.forFormat(format)
    }.build

  }

}
