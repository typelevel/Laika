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
import cats.syntax.all.*
import cats.effect.Sync
import laika.ast.Path.Root
import laika.ast.{ Document, Element, RootElement }
import laika.config.{ ConfigException, LaikaKeys }
import laika.helium.config.LandingPage
import laika.rewrite.nav.TitleDocumentConfig
import laika.theme.Theme.TreeProcessor

private[helium] object LandingPageGenerator {

  def generate[F[_]: Sync](landingPage: LandingPage): TreeProcessor[F] = Kleisli { tree =>
    val (landingPageContent, fragments, landingPageConfig) = tree.root.tree.content.collectFirst {
      case d: Document if d.path.withoutSuffix.name == "landing-page" =>
        (d.content, d.fragments, d.config)
    }.getOrElse((RootElement.empty, Map.empty[String, Element], tree.root.config))

    val titleDocument = tree.root.titleDocument.fold(
      TitleDocumentConfig.inputName(tree.root.config).map { inputName =>
        Document(
          path = Root / inputName,
          content = landingPageContent,
          fragments = fragments,
          config = landingPageConfig.withValue(LaikaKeys.versioned, false).build
        )
      }
    ) { titleDoc =>
      Right(
        titleDoc.copy(
          content = RootElement(titleDoc.content.content ++ landingPageContent.content),
          fragments = titleDoc.fragments ++ fragments,
          config = landingPageConfig.withFallback(titleDoc.config).withValue(
            LaikaKeys.versioned,
            false
          ).build
        )
      )
    }

    val result = titleDocument.map { doc =>
      val configWithTemplate   =
        if (doc.config.hasKey(LaikaKeys.template)) doc.config
        else doc.config.withValue(LaikaKeys.template, "landing.template.html").build
      val titleDocWithTemplate = doc.copy(config =
        configWithTemplate
          .withValue(
            LaikaKeys.site.css.child("searchPaths"),
            (Root / "helium" / "landing.page.css") +: landingPage.styles
          )
          .build
      )

      tree.modifyTree { tree =>
        tree
          .withTitleDocument(titleDocWithTemplate)
          .withContent(tree.content.filterNot(_.path.withoutSuffix.name == "landing-page"))
        // TODO - M3 - this should ideally be .removeIfPresent(path) or .removeDocument
      }
    }

    Sync[F].fromEither(result.leftMap(ConfigException.apply))
  }

}
