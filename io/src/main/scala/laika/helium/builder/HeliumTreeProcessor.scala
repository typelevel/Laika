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

import cats.data.Kleisli
import cats.effect.Sync
import laika.factory.Format
import laika.helium.Helium
import laika.helium.generate.{DownloadPageGenerator, LandingPageGenerator, TocPageGenerator}
import laika.io.model.ParsedTree
import laika.theme.Theme.TreeProcessor

/**
  * @author Jens Halm
  */
private[helium] class HeliumTreeProcessor[F[_]: Sync](helium: Helium) {

  import helium._
  
  private val noOp: TreeProcessor[F] = Kleisli.ask[F, ParsedTree[F]]
  
  private def addDownloadPage: TreeProcessor[F] = siteSettings.layout.downloadPage
    .filter(p => p.includeEPUB || p.includePDF)
    .fold(noOp)(DownloadPageGenerator.generate)

  val forHTML: TreeProcessor[F] = addDownloadPage
    .andThen(siteSettings.landingPage.fold(noOp)(LandingPageGenerator.generate))

  def forAllFormats (format: Format): TreeProcessor[F] =
    TocPageGenerator.generate(helium, format)
  
}
