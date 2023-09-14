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
import laika.config.{ ConfigBuilder, LaikaKeys }
import laika.io.errors.ConfigException
import laika.rewrite.nav.TitleDocumentConfig
import laika.theme.Theme.TreeProcessor

private[helium] object LandingPageGenerator {

  def generate[F[_]: Sync]: TreeProcessor[F] = Kleisli { tree =>
    val (landingPageContent, fragments, landingPageConfig) = tree.root.tree.content.collectFirst {
      case d: Document if d.path.withoutSuffix.name == "landing-page" =>
        (d.content, d.fragments, d.config)
    }.getOrElse((RootElement.empty, Map.empty[String, Element], tree.root.config))

    val titleDocument = tree.root.titleDocument.fold(
      TitleDocumentConfig.inputName(tree.root.config).map { inputName =>
        Document(
          path = Root / inputName,
          content = landingPageContent
        )
          .withFragments(fragments)
          .withConfig(landingPageConfig.withValue(LaikaKeys.versioned, false).build)
      }
    ) { titleDoc =>
      val config =
        landingPageConfig.withFallback(titleDoc.config).withValue(
          LaikaKeys.versioned,
          false
        ).build
      Right(
        titleDoc
          .appendContent(landingPageContent.content)
          .addFragments(fragments)
          .withConfig(config)
      )
    }

    val result = titleDocument.map { doc =>
      def configWithTemplate(builder: ConfigBuilder): ConfigBuilder =
        if (doc.config.hasKey(LaikaKeys.template)) builder
        else builder.withValue(LaikaKeys.template, "landing.template.html")

      val titleDocWithTemplate = doc.modifyConfig(configWithTemplate)

      tree.modifyTree { tree =>
        tree
          .withTitleDocument(titleDocWithTemplate)
          .removeContent(_.withoutSuffix.name == "landing-page")
      }
    }

    Sync[F].fromEither(result.leftMap(ConfigException.apply))
  }

}
