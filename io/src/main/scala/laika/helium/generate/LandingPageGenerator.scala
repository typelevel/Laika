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

import cats.data.Kleisli
import cats.effect.Sync
import laika.ast.Path.Root
import laika.ast.{Document, RootElement}
import laika.config.LaikaKeys
import laika.helium.config.LandingPage
import laika.rewrite.nav.TitleDocumentConfig
import laika.theme.Theme.TreeProcessor

private[helium] object LandingPageGenerator {

  def generate[F[_]: Sync] (landingPage: LandingPage): TreeProcessor[F] = Kleisli { tree =>
    
    val (landingPageContent, landingPageConfig) = tree.root.tree.content.collectFirst {
      case d: Document if d.path.withoutSuffix.name == "landing-page" => (d.content, d.config)
    }.getOrElse((RootElement.empty, tree.root.config))
    
    val titleDocument = tree.root.titleDocument.fold(
      Document(Root / TitleDocumentConfig.inputName(tree.root.config), landingPageContent, config = landingPageConfig)
    ) { titleDoc =>
      titleDoc.copy(content = RootElement(titleDoc.content.content ++ landingPageContent.content))
    }
    
    val titleDocWithTemplate =
      if (titleDocument.config.hasKey(LaikaKeys.template)) titleDocument
      else titleDocument.copy(config = titleDocument.config.withValue(LaikaKeys.template, "landing.template.html").build)
    
    // TODO - add API for replaceDocument, removeDocument, appendDocument, prependDocument
    Sync[F].pure(tree.copy(root = tree.root.copy(tree = tree.root.tree.copy(
      titleDocument = Some(titleDocWithTemplate),
      content = tree.root.tree.content.filterNot(_.path.withoutSuffix.name == "landing-page")
    ))))
  }

}
