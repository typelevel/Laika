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

import laika.ast.SpanSequence
import laika.config.{Config, ConfigBuilder, ConfigEncoder}
import laika.helium.{Helium, LandingPage, ReleaseInfo, Teaser, TopNavigationBar}

/**
  * @author Jens Halm
  */
object ConfigGenerator {

  implicit val releaseEncoder: ConfigEncoder[ReleaseInfo] = ConfigEncoder[ReleaseInfo] { releaseInfo =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("title", releaseInfo.title)
      .withValue("version", releaseInfo.version)
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

  implicit val topNavBarEncoder: ConfigEncoder[TopNavigationBar] = ConfigEncoder[TopNavigationBar] { navBar =>
    ConfigEncoder.ObjectBuilder.empty
      .withValue("home", navBar.logo)
      .withValue("links", SpanSequence(navBar.links))
      .build
  }

  def populateConfig (helium: Helium): Config =
    ConfigBuilder.empty
      .withValue("helium.landingPage", helium.landingPage)
      .withValue("helium.topBar", helium.webLayout.topNavigationBar)
      .build
  
}
