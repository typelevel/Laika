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

package laika.render

import laika.ast.*
import laika.ast.Path.Root
import laika.ast.sample.SampleTrees.SampleTreeBuilder
import laika.ast.sample.{ BuilderKey, SampleConfig, SampleTrees }
import laika.format.PDF

trait PDFTreeModel {

  private def content(key: BuilderKey): Seq[Block] = Seq(
    Title(Seq(Text(s"Title ${key.num} & More")), Id(s"title-${key.num}") + Style.title),
    Paragraph(s"Text ${key.num}")
  )

  private def titleDocContent(num: Int): Seq[Block] = Seq(
    Title(Seq(Text(s"Title Doc $num")), Id(s"title-$num") + Style.title),
    Paragraph(s"Text $num")
  )

  private def treeSetup(useTitleDocuments: Boolean): SampleTreeBuilder => SampleTreeBuilder = {
    import SampleTrees.sixDocuments.*

    if (!useTitleDocuments)
      _
        .treeConfig(paths.tree1, SampleConfig.title("Tree 1 & More"))
        .treeConfig(paths.tree2, SampleConfig.title("Tree 2 & More"))
    else
      _
        .docContent(paths.tree1_titleDoc, titleDocContent(2))
        .docContent(paths.tree2_titleDoc, titleDocContent(3))
  }

  private val navConfigKey = PDF.configKey.child("navigationDepth")

  def createTree(navigationDepth: Int = 23, useTitleDocuments: Boolean = false): DocumentTree =
    SampleTrees.sixDocuments
      .builder
      .treeConfig(Root, _.withValue(navConfigKey, navigationDepth))
      .docContent(content _)
      .apply(treeSetup(useTitleDocuments))
      .buildRoot
      .tree

}
