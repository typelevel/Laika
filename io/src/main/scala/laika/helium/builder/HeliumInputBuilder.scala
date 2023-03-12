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

import cats.effect.Async
import cats.implicits._
import laika.ast.Path.Root
import laika.config.{ ConfigBuilder, LaikaKeys }
import laika.helium.Helium
import laika.helium.generate.{ CSSVarGenerator, FOStyles, MergedCSSGenerator }
import laika.io.model.{ InputTree, InputTreeBuilder }
import laika.rewrite.DefaultTemplatePath
import laika.theme.config.{ EmbeddedFontFile, EmbeddedFontResource }

/** @author Jens Halm
  */
private[helium] object HeliumInputBuilder {

  def build[F[_]: Async](helium: Helium): F[InputTreeBuilder[F]] = {

    import helium._

    val fontResources =
      (siteSettings.fontResources ++ epubSettings.bookConfig.fonts ++ pdfSettings.bookConfig.fonts)
        .flatMap(_.resource.embedResource).distinct

    val fontInputs = fontResources.foldLeft(InputTree[F]) { case (tree, embedResource) =>
      embedResource match {
        case res: EmbeddedFontFile     => tree.addFile(res.file, res.path)
        case res: EmbeddedFontResource => tree.addClassLoaderResource(res.name, res.path)
      }
    }

    val heliumPath    = Root / "helium"
    val templatesPath = heliumPath / "templates"
    val unversioned   = ConfigBuilder.empty.withValue(LaikaKeys.versioned, false).build

    val themeInputs = fontInputs
      .addClassLoaderResource(
        "laika/helium/templates/default.template.epub.xhtml",
        DefaultTemplatePath.forEPUB
      )
      .addClassLoaderResource(
        "laika/helium/templates/default.template.html",
        DefaultTemplatePath.forHTML
      )
      .addClassLoaderResource(
        "laika/helium/templates/landing.template.html",
        Root / "landing.template.html"
      )
      .addClassLoaderResource(
        "laika/helium/templates/default.template.fo",
        DefaultTemplatePath.forFO
      )
      .addClassLoaderResource(
        "laika/helium/templates/includes/head.template.html",
        templatesPath / "head.template.html"
      )
      .addClassLoaderResource(
        "laika/helium/templates/includes/topNav.template.html",
        templatesPath / "topNav.template.html"
      )
      .addClassLoaderResource(
        "laika/helium/templates/includes/mainNav.template.html",
        templatesPath / "mainNav.template.html"
      )
      .addClassLoaderResource(
        "laika/helium/templates/includes/pageNav.template.html",
        templatesPath / "pageNav.template.html"
      )
      .addClassLoaderResource(
        "laika/helium/templates/includes/footer.template.html",
        templatesPath / "footer.template.html"
      )
      .addClassLoaderResource("laika/helium/js/theme.js", heliumPath / "laika-helium.js")
      .addClassLoaderResource("laika/helium/js/preview.js", heliumPath / "laika-preview.js")
      .addClassLoaderResource("laika/helium/css/landing.css", heliumPath / "landing.page.css")
      .addClassLoaderResource(
        "laika/helium/fonts/icofont/icofont.min.css",
        heliumPath / "icofont.min.css"
      )
      .addClassLoaderResource(
        "laika/helium/fonts/icofont/fonts/icofont.woff",
        heliumPath / "fonts" / "icofont.woff"
      )
      .addClassLoaderResource(
        "laika/helium/fonts/icofont/fonts/icofont.woff2",
        heliumPath / "fonts" / "icofont.woff2"
      )
      .addString(new FOStyles(helium).input, FOStyles.defaultPath)
      .addConfig(
        ConfigBuilder.empty.withValue(
          LaikaKeys.targetFormats,
          Seq("epub", "epub.xhtml", "pdf")
        ).build,
        Root / "laika" / "fonts" / "generated.conf"
      )
      .addConfig(unversioned, Root / "laika" / "generated.conf")
      .addConfig(unversioned, Root / "downloads" / "generated.conf")

    val versionedInputs =
      if (helium.siteSettings.versions.isEmpty) themeInputs
      else
        themeInputs.addClassLoaderResource(
          "laika/helium/js/versions.js",
          heliumPath / "laika-versions.js"
        )

    val siteVars = MergedCSSGenerator.mergeSiteCSS(CSSVarGenerator.generate(helium.siteSettings))
    val epubVars = MergedCSSGenerator.mergeEPUBCSS(CSSVarGenerator.generate(helium.epubSettings))

    (siteVars, epubVars).mapN { (siteCSS, epubCSS) =>
      versionedInputs
        .addString(siteCSS, heliumPath / "laika-helium.css")
        .addString(epubCSS, heliumPath / "laika-helium.epub.css")
    }
  }

}
