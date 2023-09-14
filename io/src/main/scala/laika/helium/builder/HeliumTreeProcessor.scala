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
import cats.syntax.all.*
import laika.ast.Path.Root
import laika.config.LaikaKeys
import laika.factory.Format
import laika.helium.Helium
import laika.helium.generate.{ DownloadPageGenerator, LandingPageGenerator, TocPageGenerator }
import laika.io.errors.ConfigException
import laika.io.model.ParsedTree
import laika.theme.Theme.TreeProcessor

/** @author Jens Halm
  */
private[helium] class HeliumTreeProcessor[F[_]: Sync](helium: Helium) {

  import helium._

  private val noOp: TreeProcessor[F] = Kleisli.ask[F, ParsedTree[F]]

  private def addLandingPage: TreeProcessor[F] =
    siteSettings.content.landingPage
      .fold(noOp)(_ => LandingPageGenerator.generate)

  private def addDownloadPage: TreeProcessor[F] =
    siteSettings.content.downloadPage
      .filter(p => p.includeEPUB || p.includePDF)
      .fold(noOp)(DownloadPageGenerator.generate)

  private def removePreviewJS: TreeProcessor[F] = Kleisli[F, ParsedTree[F], ParsedTree[F]] { tree =>
    val newTree = for {
      enabled <- tree.root.config.get(LaikaKeys.preview.enabled, false)
    } yield {
      if (enabled) tree
      else tree.removeStaticDocuments(_ == Root / "helium" / "site" / "laika-preview.js")
    }

    Sync[F].fromEither(newTree.leftMap(ConfigException.apply))
  }

  val forHTML: TreeProcessor[F] = addDownloadPage
    .andThen(addLandingPage)

  def forAllFormats(format: Format): TreeProcessor[F] =
    TocPageGenerator.generate(helium, format).andThen(removePreviewJS)

}
