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
import laika.ast.{Document, NavigationBuilderContext, NavigationList, RootElement, Styles, Title}
import laika.factory.Format
import laika.format.{EPUB, HTML, XSLFO}
import laika.helium.Helium
import laika.helium.config.TableOfContent
import laika.theme.Theme.TreeProcessor

private[helium] object TocPageGenerator {

  def generate[F[_]: Sync] (helium: Helium, format: Format): TreeProcessor[F] = {
    val tocConfig = (format match {
      case HTML => helium.siteSettings.layout.tableOfContent
      case EPUB.XHTML => helium.epubSettings.layout.tableOfContent
      case XSLFO => helium.pdfSettings.layout.tableOfContent
      case _ => None
    }).filter(_.depth > 0)
    tocConfig.fold[TreeProcessor[F]](Kleisli(Sync[F].pure))(generate(_))
  }
  
  def generate[F[_]: Sync](tocConfig: TableOfContent): TreeProcessor[F] = Kleisli { tree =>
    val result =
      if (tocConfig.depth < 1) tree
      else {
        val navContext = NavigationBuilderContext(
          refPath = Root,
          itemStyles = Set("toc"),
          maxLevels = tocConfig.depth,
          currentLevel = 0)
        val navItem = tree.root.tree.asNavigationItem(navContext)
        val navList = NavigationList(navItem.content, Styles("toc"))
        val title = Title(tocConfig.title)
        val root = RootElement(title, navList)
        val doc = Document(Root / "table-of-content", root, config = tree.root.config)
        val oldTree = tree.root.tree
        val newTree = tree.copy(root = tree.root.copy(tree = oldTree.copy(content = doc +: oldTree.content)))
        newTree
      }
    Sync[F].pure(result)
  }
  
}
