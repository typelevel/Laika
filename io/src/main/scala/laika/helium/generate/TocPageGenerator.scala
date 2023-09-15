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
import laika.api.format.Format
import laika.ast.Path.Root
import laika.ast.{
  Document,
  InternalTarget,
  NavigationBuilderContext,
  NavigationLink,
  NavigationList,
  RootElement,
  Style,
  Styles,
  Title
}
import laika.format.{ EPUB, HTML, XSLFO }
import laika.helium.Helium
import laika.helium.config.TableOfContent
import laika.internal.render.FOFormatter.Preamble
import laika.theme.Theme.TreeProcessor

private[helium] object TocPageGenerator {

  def generate[F[_]: Sync](helium: Helium, format: Format): TreeProcessor[F] = {
    val tocConfig = (format match {
      case HTML       => helium.siteSettings.content.tableOfContent
      case EPUB.XHTML => helium.epubSettings.layout.tableOfContent
      case XSLFO      => helium.pdfSettings.layout.tableOfContent
      case _          => None
    }).filter(_.depth > 0)
    tocConfig.fold[TreeProcessor[F]](Kleisli(Sync[F].pure))(generate(_))
  }

  def generate[F[_]: Sync](tocConfig: TableOfContent): TreeProcessor[F] = Kleisli { tree =>
    val result =
      if (tocConfig.depth < 1) tree
      else {
        val navContext = NavigationBuilderContext.defaults
          .withItemStyles("toc")
          .withMaxLevels(tocConfig.depth)
          .withCurrentLevel(0)
        val navItem    = tree.root.tree.asNavigationItem(navContext)
        val navContent = navItem.content.filter { item =>
          item.link match {
            case Some(NavigationLink(in: InternalTarget, _, _)) =>
              val path = in.relativeTo(Root / "doc").absolutePath
              path != Root / "downloads.gen" && path != Root / "cover"
            case _                                              => true
          }
        }
        val navList    = NavigationList(navContent, Styles("toc"))
        val title      = Title(tocConfig.title).withOptions(Style.title)
        val root       = RootElement(
          Preamble(tocConfig.title),
          title,
          navList
        ) // TODO - Preamble should be inserted in PDF.prepareTree
        val doc = Document(Root / "table-of-content", root)
          .modifyConfig(_.withValue("helium.site.pageNavigation.enabled", false))
        tree.modifyTree(_.prependContent(doc))
      }
    Sync[F].pure(result)
  }

}
