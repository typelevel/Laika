/*
 * Copyright 2016 the original author or authors.
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

import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}
import laika.tree.Documents.{Document, DocumentTree}
import laika.tree.Elements._
import laika.tree.Paths.Root

trait TreeModel {
  
  def usePDFFileConfig: Boolean = false
  
  private def pdfFileConfig = if (usePDFFileConfig) ConfigFactory.parseString("""
    |pdf {
    |  insertTitles = false
    |  bookmarks.depth = 0
    |  toc.depth = 0
    |}  
    """.stripMargin) else ConfigFactory.empty
  
  def doc (num: Int): Document = {
    val parent = if (num > 4) Root / "tree2" else if (num > 2) Root / "tree1" else Root
    Document(parent / s"doc$num.md", RootElement(Seq(
      Title(Seq(Text(s"Title $num & More")), Id(s"title-$num") + Styles("title")),
      Paragraph(Seq(Text(s"Text $num")))
    )))
  }
    
  def configWithTreeTitle (num: Int): Config = ConfigFactory.empty
      .withValue("title", ConfigValueFactory.fromAnyRef(s"Tree $num & More"))
      .withFallback(pdfFileConfig)
  
  lazy val tree = DocumentTree(Root, Seq(
      doc(1), 
      doc(2),
      DocumentTree(Root / "tree1", Seq(doc(3), doc(4)), config = configWithTreeTitle(2)),
      DocumentTree(Root / "tree2", Seq(doc(5), doc(6)), config = configWithTreeTitle(3))
    ),
    config = configWithTreeTitle(1)
  )
  
}
