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
import laika.ast.Path.Root
import laika.factory.{Format, TwoPhaseRenderFormat}
import laika.format.HTML
import laika.helium.Helium
import laika.helium.generate.{DownloadPageGenerator, LandingPageGenerator, TocPageGenerator}
import laika.io.model.ParsedTree

/**
  * @author Jens Halm
  */
private[laika] class HeliumTreeProcessor[F[_]: Sync](helium: Helium) {

  import helium._
  
  type TreeProcessor = Kleisli[F, ParsedTree[F], ParsedTree[F]]

  private val noOp: TreeProcessor = Kleisli.ask[F, ParsedTree[F]]
  
  private def addDownloadPage: TreeProcessor = siteSettings.webLayout.downloadPage
    .filter(p => p.includeEPUB || p.includePDF)
    .fold(noOp)(DownloadPageGenerator.generate)

  private def filterFonts (format: Format): TreeProcessor = format match {
    case _: TwoPhaseRenderFormat[_,_] => noOp
    case _ => Kleisli { tree: ParsedTree[F] =>
      val filteredOther = tree.staticDocuments.filterNot(_.path.isSubPath(Root / "laika" / "fonts"))
      Sync[F].pure(tree.copy(staticDocuments = filteredOther))
    }
  }

  val forHTML: TreeProcessor = addDownloadPage
    .andThen(TocPageGenerator.generate(helium, HTML))
    .andThen(siteSettings.landingPage.fold(noOp)(LandingPageGenerator.generate))
    .andThen(filterFonts(HTML))

  def forFormat (format: Format): TreeProcessor =
    TocPageGenerator.generate(helium, format).andThen(filterFonts(format))
  
}
