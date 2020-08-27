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

import cats.effect.Sync
import cats.implicits._
import laika.ast.Path.Root
import laika.ast.TemplateDocument
import laika.helium.Helium
import laika.helium.generate.{CSSVarGenerator, EPUBTemplate, FOStyles, MergedCSSGenerator}
import laika.io.model.{InputTree, InputTreeBuilder}
import laika.io.runtime.Runtime
import laika.rewrite.DefaultTemplatePath
import laika.theme.config.{EmbeddedFontFile, EmbeddedFontResource}

/**
  * @author Jens Halm
  */
private[helium] object HeliumInputBuilder {

  def build[F[_]: Sync: Runtime] (helium: Helium): F[InputTreeBuilder[F]] = {
    
    import helium._
    
    val fontResources = (siteSettings.fontResources ++ epubSettings.bookConfig.fonts ++ pdfSettings.bookConfig.fonts)
      .flatMap(_.resource.embedResource).distinct
    
    val fontInputs = fontResources.foldLeft(InputTree[F]) { case (tree, embedResource) =>
      embedResource match {
        case res: EmbeddedFontFile     => tree.addFile(res.file, res.path)
        case res: EmbeddedFontResource => tree.addClasspathResource(res.name, res.path)
      }
    }
  
    val themeInputs = fontInputs
      .addTemplate(TemplateDocument(DefaultTemplatePath.forEPUB, EPUBTemplate.default))
      .addClasspathResource("laika/helium/templates/default.template.html", DefaultTemplatePath.forHTML)
      .addClasspathResource("laika/helium/templates/landing.template.html", Root / "landing.template.html")
      .addClasspathResource("laika/helium/templates/default.template.fo", DefaultTemplatePath.forFO)
      .addClasspathResource("laika/helium/js/theme.js", Root / "helium" / "laika-helium.js")
      .addString(new FOStyles(helium).input, FOStyles.defaultPath)
  
    MergedCSSGenerator.merge(CSSVarGenerator.generate(helium)).map {
      themeInputs.addString(_, Root / "helium" / "laika-helium.css")
    }
  }
  
}
