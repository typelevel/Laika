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

import laika.ast._
import laika.ast.sample.{BuilderKey, SampleConfig, SampleSixDocuments, SampleTrees}
import laika.format.PDF

trait TreeModel {
  
  def navigationDepth: Int = 23

  def useTitleDocuments: Boolean = false
  
  def content (key: BuilderKey): Seq[Block] = Seq(
    Title(Seq(Text(s"Title ${key.num} & More")), Id(s"title-${key.num}") + Style.title),
    Paragraph(s"Text ${key.num}")
  )
  def titleDocContent (num: Int): Seq[Block] = Seq(
    Title(Seq(Text(s"Title Doc $num")), Id(s"title-$num") + Style.title),
    Paragraph(s"Text $num")
  )
  
  lazy val treeSetup: SampleSixDocuments => SampleSixDocuments = 
    if (!useTitleDocuments) _
      .tree1.config(SampleConfig.title("Tree 1 & More"))
      .tree2.config(SampleConfig.title("Tree 2 & More"))
    else 
      _.tree1.titleDoc.content(titleDocContent(2))
       .tree2.titleDoc.content(titleDocContent(3))
          
  val navConfigKey = PDF.BookConfig.defaultKey.value.child("navigationDepth")
  
  lazy val tree = SampleTrees.sixDocuments
    .root.config(_.withValue(navConfigKey, navigationDepth))
    .docContent(content _)
    .apply(treeSetup)
    .build
    .tree
  
}
