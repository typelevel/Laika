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

import laika.config.{Config, ConfigBuilder, Origin}
import laika.ast._
import laika.ast.Path.Root

trait TreeModel {
  
  def usePDFFileConfig: Boolean = false

  def useTitleDocuments: Boolean = false
  
  private def pdfFileConfig: Config = if (usePDFFileConfig) ConfigBuilder.empty
      .withValue("pdf.bookmarks.depth", 0)
      .withValue("pdf.toc.depth", 0)
      .build
    else Config.empty
  
  def doc (num: Int): Document = {
    val parent = if (num > 4) Root / "tree2" else if (num > 2) Root / "tree1" else Root
    Document(parent / s"doc$num.md", RootElement(Seq(
      Title(Seq(Text(s"Title $num & More")), Id(s"title-$num") + Styles("title")),
      Paragraph(Seq(Text(s"Text $num")))
    )))
  }
    
  def configWithTreeTitle (num: Int): Config = ConfigBuilder
    .withFallback(pdfFileConfig, Origin(Origin.TreeScope, Root))
    .withValue("title", s"Tree $num & More")
    .build

  def configWithFallback: Config = ConfigBuilder.withFallback(pdfFileConfig).build

  def subtreeDocs (nums: Int*): Seq[Document] = nums.map(doc)
  
  def subtreeTitle (subTreeNum: Int): Option[Document] = {
    val parent = Root / s"tree${subTreeNum - 1}"
    if (useTitleDocuments) Some(Document(parent / "title.md", RootElement(Seq(
      Title(Seq(Text(s"Title Doc $subTreeNum")), Id(s"title-$subTreeNum") + Styles("title")),
      Paragraph(Seq(Text(s"Text $subTreeNum")))
    )))) else None
  }
  
  lazy val tree = DocumentTree(Root, Seq(
      doc(1), 
      doc(2),
      DocumentTree(Root / "tree1", subtreeDocs(3,4), titleDocument = subtreeTitle(2), 
        config = if (useTitleDocuments) configWithFallback else configWithTreeTitle(2)),
      DocumentTree(Root / "tree2", subtreeDocs(5,6), titleDocument = subtreeTitle(3), 
        config = if (useTitleDocuments) configWithFallback else configWithTreeTitle(3))
    ),
    config = configWithTreeTitle(1)
  )
  
}
