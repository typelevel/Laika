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

package laika.ast.sample

import laika.ast.Path.Root
import laika.ast.{Block, Document, DocumentCursor, DocumentTree, DocumentTreeRoot, Paragraph, Path, RootElement, Title, TreeContent}
import laika.config.{Config, ConfigBuilder, Origin}

object SampleTrees {

  def empty: SampleRoot = ???
  def twoDocuments: SampleRoot = ???
  def sixDocuments: SampleSixDocuments = new SampleSixDocuments(SampleRoot(2, 6, SampleContent.text))
  def titleDocuments: SampleRoot = ???
  
}

trait SampleOps {
  
  type RootApi
  
  protected def sampleRoot: SampleRoot

  private[sample] def addTreeConfig (num: Int, f: ConfigBuilder => ConfigBuilder): RootApi = {
    val target = sampleRoot.treeBuilders(num)
    copyWith(sampleRoot.copy(treeBuilders = sampleRoot.treeBuilders.updated(num, target.copy(config = f +: target.config))))
  }
  
  private[sample] def addDocumentConfig (num: Int, f: ConfigBuilder => ConfigBuilder): RootApi = {
    val target = sampleRoot.docBuilders(num-1)
    copyWith(sampleRoot.copy(docBuilders = sampleRoot.docBuilders.updated(num-1, target.copy(config = f +: target.config))))
  }

  private[sample] def setDocumentContent (num: Int, f: Int => Seq[Block]): RootApi = {
    val target = sampleRoot.docBuilders(num-1)
    copyWith(sampleRoot.copy(docBuilders = sampleRoot.docBuilders.updated(num-1, target.copy(content = Some(f)))))
  }

  private[sample] def copyWith (root: SampleRoot): RootApi
  
}

trait NumberedSampleOps extends SampleOps {

  protected def num: Int 
  
}

class SampleSixDocuments (protected val sampleRoot: SampleRoot) extends SampleOps { self =>

  type RootApi = SampleSixDocuments
  
  trait DocOps extends SampleDocumentOps {
    type RootApi = SampleSixDocuments
    protected def sampleRoot = self.sampleRoot
    private[sample] def copyWith (root: SampleRoot): RootApi = new SampleSixDocuments(root)
  }
  trait TreeOps extends SampleTreeOps {
    type RootApi = SampleSixDocuments
    protected def sampleRoot = self.sampleRoot
    private[sample] def copyWith (root: SampleRoot): RootApi = new SampleSixDocuments(root)
  }
 
  object root  extends TreeOps { protected def num = 0 }
  object tree1 extends TreeOps { protected def num = 1 }
  object tree2 extends TreeOps { protected def num = 2 }
  
  object doc1 extends DocOps { protected def num = 1 }
  object doc2 extends DocOps { protected def num = 2 }
  object doc3 extends DocOps { protected def num = 3 }
  object doc4 extends DocOps { protected def num = 4 }
  object doc5 extends DocOps { protected def num = 5 }
  object doc6 extends DocOps { protected def num = 6 }
  
  def suffix (value: String): RootApi = new SampleSixDocuments(sampleRoot.copy(suffix = Some(value)))
  def docContent (f: Int => Seq[Block]): RootApi = new SampleSixDocuments(sampleRoot.copy(defaultContent = f))
  
  private[sample] def copyWith (root: SampleRoot): RootApi = new SampleSixDocuments(root)
  
  def build: DocumentTreeRoot = {
    def buildTree (treeNum: Int, docNum: Int, parentConfig: Config): DocumentTree = 
      sampleRoot.treeBuilders(treeNum).build(Seq(
        sampleRoot.docBuilders(docNum),
        sampleRoot.docBuilders(docNum + 1),
      ), parentConfig, sampleRoot)
    val tree = buildTree(0, 0, Config.empty)
    DocumentTreeRoot(tree.copy(
      content = tree.content ++ Seq(buildTree(1, 2, tree.config), buildTree(2, 4, tree.config))
    ))
  }
}

private[sample] case class SampleRoot (treeBuilders: IndexedSeq[SampleTree],
                                       docBuilders: IndexedSeq[SampleDocument],
                                       defaultContent: Int => Seq[Block],
                                       suffix: Option[String] = None) {
  
}

object SampleRoot {
  private[sample] def apply(numTrees: Int, numDocs: Int, defaultContent: Int => Seq[Block]): SampleRoot =
    new SampleRoot(
      (0 to numTrees).map(SampleTree(_)).toVector, 
      (1 to numDocs).map(SampleDocument(_)).toVector,
      SampleContent.text
    )
}

trait SampleDocumentOps extends NumberedSampleOps {
  
  def config (f: ConfigBuilder => ConfigBuilder): RootApi = addDocumentConfig(num, f)

  // TODO - configValue API

  def content (f: Int => Seq[Block]): RootApi = setDocumentContent(num, f)
  
  def buildCursor: DocumentCursor = ???
  
}

trait SampleTreeOps extends NumberedSampleOps {

  def config (f: ConfigBuilder => ConfigBuilder): RootApi = addTreeConfig(num, f)

}

private[sample] case class SampleDocument (num: Int, 
                                           config: Seq[ConfigBuilder => ConfigBuilder] = Nil, 
                                           content: Option[Int => Seq[Block]] = None) {

  private[sample] def build (treePath: Path, parentConfig: Config, root: SampleRoot): TreeContent = {
    println(s"doc $num with ${config.size} config builders")
    val path = treePath / ("doc-" + num + root.suffix.fold("")("." + _))
    val configBuilder = ConfigBuilder.withFallback(parentConfig, Origin(Origin.DocumentScope, path))
    Document(
      path, 
      RootElement(content.getOrElse(root.defaultContent)(num)),
      config = config.foldLeft(configBuilder){ case (builder, f) => f(builder) }.build
    )
  }
}

private[sample] case class SampleTree (num: Int,
                                       config: Seq[ConfigBuilder => ConfigBuilder] = Nil) {

  def versioned: SampleTree = ???
  def unversioned: SampleTree = ???

  private[sample] def build (content: Seq[SampleDocument], parentConfig: Config, root: SampleRoot): DocumentTree = {
    val path = if (num == 0) Root else Root / s"tree-$num"
    val configBuilder = ConfigBuilder.withFallback(parentConfig, Origin(Origin.TreeScope, path))
    val conf = config.foldLeft(configBuilder){ case (builder, f) => f(builder) }.build
    DocumentTree(
      path,
      content.map(_.build(path, conf, root)),
      config = conf
    )
  }

}

object SampleContent {

  val empty: Int => Seq[Block] = _ => Nil
  val text: Int => Seq[Block] = num => Seq(Paragraph(s"Text $num"))
  val title: Int => Seq[Block] = num => Seq(Title(s"Title $num"))
  val sections: Int => Seq[Block] = num => ???

}

object SampleConfig {
  
//  val versions: ConfigBuilder => ConfigBuilder = ???
//  val selections: ConfigBuilder => ConfigBuilder = ???
  
}
