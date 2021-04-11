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
import laika.ast._
import laika.config.{Config, ConfigBuilder, LaikaKeys, Origin, TreeConfigErrors}

object SampleTrees {

  def twoDocuments: SampleTwoDocuments = new SampleTwoDocuments(SampleRoot(0, 2, 0, SampleContent.text))
  def sixDocuments: SampleSixDocuments = new SampleSixDocuments(SampleRoot(2, 6, 2, SampleContent.text))
  
}

sealed trait BuilderKey {
  def num: Int
  def defaultTitle: String
  def prefix: String
}
object BuilderKey {
  case class Doc(num: Int) extends BuilderKey {
    val defaultTitle = "Title " + num // TODO - should be Doc
    val prefix = "doc"
  }
  case class Tree(num: Int) extends BuilderKey {
    val defaultTitle = "Tree " + num
    val prefix = "tree"
  }
  case class StaticTree(num: Int) extends BuilderKey {
    val defaultTitle = "Static " + num
    val prefix = "static"
  }
}

trait SampleOps { self =>
  
  type RootApi <: SampleOps
  
  protected def sampleRoot: SampleRoot

  private[sample] def copyWith (root: SampleRoot): RootApi

  private[sample] def addStaticConfig (key: BuilderKey, f: ConfigBuilder => ConfigBuilder): RootApi = {
    val target = sampleRoot.staticConfigs(key.num - 1)
    copyWith(sampleRoot.copy(staticConfigs =
      sampleRoot.staticConfigs.updated(key.num - 1, target.copy(config = target.config + f))
    ))
  }

  private[sample] def addDocumentConfig (key: BuilderKey, f: ConfigBuilder => ConfigBuilder): RootApi =
    updateDocument(key, target => target.copy(config = target.config + f))

  private[sample] def setDocumentContent (key: BuilderKey, f: BuilderKey => Seq[Block]): RootApi =
    updateDocument(key, target => target.copy(content = Some(f)))

  private[sample] def setDocumentSuffix (key: BuilderKey, value: String): RootApi =
    updateDocument(key, target => target.copy(suffix = Some(value)))
  
  private[sample] def updateDocument (key: BuilderKey, f: SampleDocument => SampleDocument): RootApi = {
    key match {
      case _: BuilderKey.Doc =>
        val target = sampleRoot.docBuilders(key.num - 1)
        copyWith(sampleRoot.copy(docBuilders = sampleRoot.docBuilders.updated(key.num - 1, f(target))))

      case _: BuilderKey.Tree =>
        val tree = sampleRoot.treeBuilders(key.num)
        val target = tree.titleDoc.getOrElse(SampleDocument(key, SampleConfigBuilder(Origin.DocumentScope)))
        copyWith(sampleRoot.copy(treeBuilders =
          sampleRoot.treeBuilders.updated(key.num, tree.copy(titleDoc = Some(f(target))))
        ))
      case _: BuilderKey.StaticTree => copyWith(sampleRoot)
    }
  }
  
  private[sample] def addTreeConfig (key: BuilderKey, f: ConfigBuilder => ConfigBuilder): RootApi =
    updateTree(key, target => target.copy(config = target.config + f))

  private[sample] def addTemplate (key: BuilderKey, name: String, spans: Seq[TemplateSpan]): RootApi = {
    updateTree(key, target => {
      val doc = TemplateDocument(target.path / name, TemplateRoot(spans))
      target.copy(templates = doc +: target.templates)
    })
  }
  private def updateTree (key: BuilderKey, f: SampleTree => SampleTree): RootApi = {
    val target = sampleRoot.treeBuilders(key.num)
    copyWith(sampleRoot.copy(treeBuilders = sampleRoot.treeBuilders.updated(key.num, f(target))))
  }
}

trait SampleRootOps extends SampleOps { self =>

  trait KeyedSampleOps {
    protected def key: BuilderKey
    type RootApi = self.RootApi
    protected def sampleRoot = self.sampleRoot
    private[sample] def copyWith (root: SampleRoot): RootApi = self.copyWith(root)
  }
  
  abstract class DocOps(protected val key: BuilderKey) extends KeyedSampleOps {
    
    def config (f: ConfigBuilder => ConfigBuilder): RootApi = addDocumentConfig(key, f)

    // TODO - configValue API

    def content (f: BuilderKey => Seq[Block]): RootApi  = setDocumentContent(key, f)
    def content (blocks: Seq[Block]): RootApi           = setDocumentContent(key, _ => blocks)
    def content (block: Block, blocks: Block*): RootApi = content(block +: blocks)
    def suffix (value: String): RootApi                 = setDocumentSuffix(key, value)

    def buildCursor: DocumentCursor = ???
  }
  
  abstract class TreeOps(num: Int) extends KeyedSampleOps {
    protected def key: BuilderKey = BuilderKey.Tree(num)
    def config (f: ConfigBuilder => ConfigBuilder): RootApi        = addTreeConfig(key, f)
    def template (name: String, spans: Seq[TemplateSpan]): RootApi = addTemplate(key, name, spans)
    def template (name: String, span: TemplateSpan, spans: TemplateSpan*): RootApi = template(name, span +: spans)
  }
  
  abstract class StaticOps(num: Int) extends KeyedSampleOps {
    protected def key: BuilderKey = BuilderKey.Tree(num)
    def config (f: ConfigBuilder => ConfigBuilder): RootApi = addStaticConfig(key, f)
  }
  
  def suffix (value: String): RootApi                    = copyWith(sampleRoot.copy(suffix = Some(value)))
  def docContent (f: BuilderKey => Seq[Block]): RootApi  = copyWith(sampleRoot.copy(defaultContent = f))
  def docContent (blocks: Seq[Block]): RootApi           = copyWith(sampleRoot.copy(defaultContent = _ => blocks))
  def docContent (block: Block, blocks: Block*): RootApi = docContent(block +: blocks)

  def staticDoc (path: Path): RootApi =
    copyWith(sampleRoot.copy(
      staticDocuments = sampleRoot.staticDocuments :+ StaticDocument(path)
    ))

  def staticDoc (path: Path, format: String, formats: String*): RootApi =
    copyWith(sampleRoot.copy(
      staticDocuments = sampleRoot.staticDocuments :+ StaticDocument(path, format, formats:_*)
    ))

  protected def buildTree (treeNum: Int, docNum: Int, parentConfig: Config): DocumentTree = {
    val docs =  Seq(sampleRoot.docBuilders(docNum), sampleRoot.docBuilders(docNum + 1))
    sampleRoot.treeBuilders(treeNum).build(docs, parentConfig, sampleRoot)
  }

  protected def buildStaticTree (treeNum: Int, parentConfig: Config): DocumentTree = {
    sampleRoot.staticConfigs(treeNum-1).build(Nil, parentConfig, sampleRoot)
  }

  def titleDocuments (includeRoot: Boolean = true): RootApi = {
    val keys = sampleRoot.treeBuilders
      .map(_.key)
      .drop(if (includeRoot) 0 else 1)

    keys.foldLeft(copyWith(sampleRoot)) {
      case (api, key) => api.updateDocument(key, identity).asInstanceOf[RootApi]
    }
  }

}

class SampleTwoDocuments (protected val sampleRoot: SampleRoot) extends SampleRootOps { self =>

  type RootApi = SampleTwoDocuments

  object root extends TreeOps(0)
  object doc1 extends DocOps(BuilderKey.Doc(1))
  object doc2 extends DocOps(BuilderKey.Doc(2))

  def apply (f: SampleTwoDocuments => SampleTwoDocuments): RootApi = f(this)
  private[sample] def copyWith (root: SampleRoot): RootApi = new SampleTwoDocuments(root)

  def build: DocumentTreeRoot = DocumentTreeRoot(
    tree = buildTree(0, 0, Config.empty),
    staticDocuments = sampleRoot.staticDocuments
  )
  def buildCursor: Either[TreeConfigErrors, RootCursor] = RootCursor(build)
}

class SampleSixDocuments (protected val sampleRoot: SampleRoot) extends SampleRootOps { self =>

  import BuilderKey._
  
  type RootApi = SampleSixDocuments
  
  object root extends TreeOps(0) {
    object titleDoc extends DocOps(Tree(0)) 
  }
  object tree1 extends TreeOps(1) {
    object titleDoc extends DocOps(Tree(1))
  }
  object tree2 extends TreeOps(2) { 
    object titleDoc extends DocOps(Tree(2))
  }
  
  object doc1 extends DocOps(Doc(1))
  object doc2 extends DocOps(Doc(2))
  object doc3 extends DocOps(Doc(3))
  object doc4 extends DocOps(Doc(4))
  object doc5 extends DocOps(Doc(5))
  object doc6 extends DocOps(Doc(6))
  
  object static1 extends StaticOps(1)
  object static2 extends StaticOps(2)
  
  def apply (f: SampleSixDocuments => SampleSixDocuments): RootApi = f(this)
  private[sample] def copyWith (root: SampleRoot): RootApi = new SampleSixDocuments(root)
  
  def build: DocumentTreeRoot = {
    val tree = buildTree(0, 0, Config.empty)
    DocumentTreeRoot(
      tree = tree.copy(
        content = tree.content ++ Seq(
          buildTree(1, 2, tree.config), 
          buildTree(2, 4, tree.config), 
          buildStaticTree(1, tree.config),
          buildStaticTree(2, tree.config)
        )
      ),
      staticDocuments = sampleRoot.staticDocuments
    )
  }
  
  def buildCursor: Either[TreeConfigErrors, RootCursor] = RootCursor(build)
}

private[sample] case class SampleRoot (treeBuilders: IndexedSeq[SampleTree],
                                       docBuilders: IndexedSeq[SampleDocument],
                                       defaultContent: BuilderKey => Seq[Block],
                                       suffix: Option[String] = None,
                                       staticDocuments: Seq[StaticDocument] = Nil,
                                       staticConfigs: Seq[SampleTree] = Nil) {
  
}

private[sample] object SampleRoot {
  def apply(numTrees: Int, numDocs: Int, numStatic: Int, defaultContent: BuilderKey => Seq[Block]): SampleRoot =
    new SampleRoot(
      (0 to numTrees).map(num => SampleTree(BuilderKey.Tree(num), SampleConfigBuilder(Origin.TreeScope))).toVector, 
      (1 to numDocs).map(num => SampleDocument(BuilderKey.Doc(num), SampleConfigBuilder(Origin.DocumentScope))).toVector,
      SampleContent.text,
      staticConfigs = (1 to numStatic).map(num => SampleTree(BuilderKey.StaticTree(num), SampleConfigBuilder(Origin.TreeScope)))
    )
}

private[sample] case class SampleTree (key: BuilderKey,
                                       config: SampleConfigBuilder,
                                       templates: Seq[TemplateDocument] = Nil,
                                       titleDoc: Option[SampleDocument] = None) {

  def versioned: SampleTree = ???
  def unversioned: SampleTree = ???

  val path = if (key.num == 0) Root else Root / s"${key.prefix}-${key.num}"

  def build (content: Seq[SampleDocument], parentConfig: Config, root: SampleRoot): DocumentTree = {
    val conf = config.build(parentConfig, path)
    DocumentTree(
      path,
      content.map(_.build(path, conf, root)),
      titleDocument = titleDoc.map(_.build(path, conf, root)),
      templates = templates,
      config = conf
    )
  }

}

private[sample] case class SampleDocument (key: BuilderKey, 
                                           config: SampleConfigBuilder, 
                                           content: Option[BuilderKey => Seq[Block]] = None,
                                           suffix: Option[String] = None) {

  def build (treePath: Path, parentConfig: Config, root: SampleRoot): Document = {
    val suffixString = suffix.orElse(root.suffix).fold("")("." + _)
    val localName = key match {
      case _: BuilderKey.Tree => "README"
      case _: BuilderKey.Doc  => "doc-" + key.num
      case _: BuilderKey.StaticTree => "static-" + key.num
    } 
    val path = treePath / (localName + suffixString)
    Document(
      path, 
      RootElement(content.getOrElse(root.defaultContent)(key)),
      config = config.build(parentConfig, path)
    )
  }
}

private[sample] case class SampleConfigBuilder (scope: Origin.Scope, builders: Seq[ConfigBuilder => ConfigBuilder] = Nil) {
  def + (builder: ConfigBuilder => ConfigBuilder): SampleConfigBuilder =
    copy(builders = builder +: builders)
  def build (parentConfig: Config, path: Path): Config = {
    val configBuilder = ConfigBuilder.withFallback(parentConfig, Origin(scope, path))
    builders.foldLeft(configBuilder){ case (builder, f) => f(builder) }.build
  }
}

object SampleContent {

  private def header (level: Int, pos: Int, style: String = "section") =
    Header(level, List(Text("Section "+pos)), Styles(style))
    
  private def section (level: Int, pos: Int, content: Seq[Block] = Nil): Section =
    Section(header(level, pos).withId(s"section-$pos"), content)
  
  val empty: BuilderKey => Seq[Block] = _ => Nil
  val text: BuilderKey => Seq[Block] = key => Seq(Paragraph(s"Text ${key.num}"))
  val title: BuilderKey => Seq[Block] = key => Seq(Title(key.defaultTitle))
  val fourHeaders: BuilderKey => Seq[Block] = key => Seq(
    Header(1, List(Text(key.defaultTitle)), Style.title),
      header(1,1),
      header(2,2),
      header(1,3),
      header(2,4)
    )
  val fourSections: BuilderKey => Seq[Block] = key => Seq(
    Title(List(Text(key.defaultTitle)), Id("title") + Style.title),
    section(1, 1, Seq(section(2,2))),
    section(1, 3, Seq(section(2,4)))
  )

}

object SampleConfig {
  
  def versioned (flag: Boolean): ConfigBuilder => ConfigBuilder = _.withValue(LaikaKeys.versioned, flag)
  val noLinkValidation: ConfigBuilder => ConfigBuilder = _.withValue(LaikaKeys.validateLinks, false)
  def targetFormats (formats: String*): ConfigBuilder => ConfigBuilder = _.withValue(LaikaKeys.targetFormats, formats)
  def siteBaseURL (value: String): ConfigBuilder => ConfigBuilder = _.withValue(LaikaKeys.siteBaseURL, value)
  def title (text: String): ConfigBuilder => ConfigBuilder = _.withValue(LaikaKeys.title, text)
  
//  val versions: ConfigBuilder => ConfigBuilder = ???
//  val selections: ConfigBuilder => ConfigBuilder = ???
  
}
