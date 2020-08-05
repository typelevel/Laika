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
import laika.config.{Config, ConfigBuilder, ConfigEncoder, LaikaKeys}
import laika.helium.{ExternalLink, InternalLink, LandingPage, LandingPageLink, ReleaseInfo, Teaser}
import laika.io.model.ParsedTree
import laika.rewrite.nav.TitleDocumentConfig

/**
  * @author Jens Halm
  */
private[laika] object LandingPageGenerator {

  implicit val releaseEncoder: ConfigEncoder[ReleaseInfo] = ConfigEncoder[ReleaseInfo] { releaseInfo =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("title", releaseInfo.title)
      .withValue("version", releaseInfo.version)
      .build
  }

  implicit val linkEncoder: ConfigEncoder[LandingPageLink] = ConfigEncoder[LandingPageLink] { link =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("text", link.text)
      .withValue("version", link match { case e: ExternalLink => e.target; case i: InternalLink => i.target.toString })
      .build
  }

  implicit val teaserEncoder: ConfigEncoder[Teaser] = ConfigEncoder[Teaser] { teaser =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("title", teaser.title)
      .withValue("description", teaser.description)
      .build
  }

  implicit val landingPageEncoder: ConfigEncoder[LandingPage] = ConfigEncoder[LandingPage] { landingPage =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("logo", landingPage.logo)
      .withValue("title", landingPage.title)
      .withValue("subtitle", landingPage.subtitle)
      .withValue("latestReleases", landingPage.latestReleases)
      .withValue("license", landingPage.license)
      .withValue("documentationLinks", landingPage.documentationLinks)
      .withValue("projectLinks", landingPage.projectLinks)
      .withValue("teasers", landingPage.teasers) // TODO - change to teaserRows
      .build
  }

  def populateConfig (landingPage: LandingPage): Config = 
    ConfigBuilder.empty
      .withValue("helium.landingPage", landingPage)
      .build

  def generate[F[_]: Sync] (landingPage: LandingPage): Kleisli[F, ParsedTree[F], ParsedTree[F]] = Kleisli { tree =>
    val landingPageContent = tree.root.tree.content.collectFirst {
      case d: Document if d.path.withoutSuffix.name == "landing-page" => d.content
    }.getOrElse(RootElement.empty)
    val titleDocument = tree.root.titleDocument.fold(
      Document(Root / TitleDocumentConfig.inputName(tree.root.config), landingPageContent)
    ) { titleDoc =>
      titleDoc.copy(content = RootElement(titleDoc.content.content ++ landingPageContent.content))
    }
    val titleDocWithTemplate =
      if (titleDocument.config.hasKey(LaikaKeys.template)) titleDocument
      else titleDocument.copy(config = titleDocument.config.withValue(LaikaKeys.template, "landing-template.html").build)
    Sync[F].pure(tree.copy(root = tree.root.copy(tree = tree.root.tree.copy(titleDocument = Some(titleDocWithTemplate)))))
  }

}
