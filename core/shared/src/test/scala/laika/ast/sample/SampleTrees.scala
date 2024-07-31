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

import laika.api.config.{ ConfigBuilder, Origin }
import laika.ast.Path.Root
import laika.ast.{ Path, * }
import laika.api.config.Origin.{ DocumentScope, TreeScope }
import laika.config.{ LaikaKeys, LinkValidation }

object SampleTrees {

  object sixDocuments {

    object paths {

      val doc1: Path           = Root / "doc-1"
      val doc2: Path           = Root / "doc-2"
      val tree1: Path          = Root / "tree-1"
      val tree2: Path          = Root / "tree-2"
      val tree1_titleDoc: Path = tree1 / "README"
      val tree2_titleDoc: Path = tree2 / "README"
      val tree1_doc3: Path     = tree1 / "doc-3"
      val tree1_doc4: Path     = tree1 / "doc-4"
      val tree2_doc5: Path     = tree2 / "doc-5"
      val tree2_doc6: Path     = tree2 / "doc-6"

      val allDocuments: Seq[Path] = Seq(doc1, doc2, tree1_doc3, tree1_doc4, tree2_doc5, tree2_doc6)

    }

    val builder = new SampleTreeBuilder(
      paths.allDocuments.map(p => (p, DocumentBuilder(p))).toMap
    )

  }

  private trait BuilderPart {
    def applyTo(builder: DocumentTreeBuilder): DocumentTreeBuilder
  }

  private case class DocumentBuilder(
      path: Path,
      content: BuilderKey => Seq[Block] = SampleContent.text,
      configF: Seq[ConfigBuilder => ConfigBuilder] = Nil
  ) extends BuilderPart {

    def applyTo(builder: DocumentTreeBuilder): DocumentTreeBuilder = {
      val builderKey       =
        if (path.basename == "README") BuilderKey.TitleDoc(path.parent.basename.last.toString.toInt)
        else BuilderKey.Doc(path.basename.last.toString.toInt)
      val configBuilder    = ConfigBuilder.withOrigin(Origin(DocumentScope, path))
      val processedBuilder = configF.foldLeft(configBuilder) { case (acc, builderF) =>
        builderF(acc)
      }
      val doc              = Document(path, RootElement(content(builderKey))).withConfig(
        processedBuilder.build
      )
      builder.addDocument(doc)
    }

  }

  private case class TreeConfigBuilder(path: Path, builderF: ConfigBuilder => ConfigBuilder)
      extends BuilderPart {

    def applyTo(builder: DocumentTreeBuilder): DocumentTreeBuilder = {
      val configBuilder =
        ConfigBuilder.withOrigin(Origin(TreeScope, path / "directory.conf"))
      val config        = builderF(configBuilder).build
      builder.addConfig(config)
    }

  }

  class SampleTreeBuilder private[SampleTrees] (
      docBuilders: Map[Path, DocumentBuilder],
      treeConfigs: Map[Path, TreeConfigBuilder] = Map.empty
  ) {

    private def modifyDocBuilder(
        path: Path
    )(f: DocumentBuilder => DocumentBuilder): SampleTreeBuilder = {
      val builder    = docBuilders.getOrElse(path, DocumentBuilder(path))
      val newBuilder = f(builder)
      val newMap     = docBuilders + ((path, newBuilder))
      new SampleTreeBuilder(newMap, treeConfigs)
    }

    private def modifyAllDocBuilders(f: DocumentBuilder => DocumentBuilder): SampleTreeBuilder = {
      import laika.internal.collection.TransitionalCollectionOps.*
      val newMap = docBuilders.mapValuesStrict(f)
      new SampleTreeBuilder(newMap, treeConfigs)
    }

    def docContent(blocks: Seq[Block]): SampleTreeBuilder = docContent(_ => blocks)

    def docContent(f: BuilderKey => Seq[Block]): SampleTreeBuilder =
      modifyAllDocBuilders { builder =>
        builder.copy(content = f)
      }

    def docContent(path: Path, content: Seq[Block]): SampleTreeBuilder =
      modifyDocBuilder(path) { builder =>
        builder.copy(content = _ => content)
      }

    def docConfig(path: Path, f: ConfigBuilder => ConfigBuilder): SampleTreeBuilder =
      modifyDocBuilder(path) { builder =>
        builder.copy(configF = builder.configF :+ f)
      }

    def titleDocuments(treePaths: Path*): SampleTreeBuilder = treePaths.foldLeft(this) {
      case (acc, path) =>
        acc.docContent(
          path / "README",
          Seq(Paragraph(s"Text " + path.basename.last.toString.toInt))
        )
    }

    def treeConfig(path: Path, f: ConfigBuilder => ConfigBuilder): SampleTreeBuilder = {
      val builder = treeConfigs.get(path) match {
        case Some(builder) => builder.copy(builderF = builder.builderF.andThen(f))
        case None          => TreeConfigBuilder(path, f)
      }
      val newMap  = treeConfigs + ((builder.path, builder))
      new SampleTreeBuilder(docBuilders, newMap)
    }

    def suffix(value: String): SampleTreeBuilder =
      modifyAllDocBuilders { builder =>
        builder.copy(path = builder.path.withSuffix(value))
      }

    def suffix(path: Path, value: String): SampleTreeBuilder =
      modifyDocBuilder(path) { builder =>
        builder.copy(path = builder.path.withSuffix(value))
      }

    def apply(f: SampleTreeBuilder => SampleTreeBuilder): SampleTreeBuilder = f(this)

    def builder: DocumentTreeBuilder = {

      val allParts = docBuilders.values.toList.sortBy(_.path.toString) ++
        treeConfigs.values.toList.sortBy(_.path.toString)

      allParts.foldLeft(DocumentTree.builder) { case (acc, part) => part.applyTo(acc) }

    }

    def buildRoot: DocumentTreeRoot = builder.buildRoot

  }

}

sealed trait BuilderKey {
  def num: Int
}

object BuilderKey {

  case class Doc(num: Int)      extends BuilderKey
  case class TitleDoc(num: Int) extends BuilderKey

}

object SampleContent {

  def titleFor(key: BuilderKey): String = key match {
    case BuilderKey.Doc(num)      => "Doc " + num
    case BuilderKey.TitleDoc(num) => "Tree " + num
  }

  private def header(level: Int, pos: Int, style: String = "section") =
    Header(level, List(Text("Section " + pos)), Styles(style))

  private def section(level: Int, pos: Int, content: Seq[Block] = Nil): Section =
    Section(header(level, pos).withId(s"section-$pos"), content)

  val empty: BuilderKey => Seq[Block] = _ => Nil
  val text: BuilderKey => Seq[Block]  = key => Seq(Paragraph(s"Text ${key.num}"))

  val fourHeaders: BuilderKey => Seq[Block] = key =>
    Seq(
      Header(1, List(Text(titleFor(key))), Style.title),
      header(1, 1),
      header(2, 2),
      header(1, 3),
      header(2, 4)
    )

  val fourSections: BuilderKey => Seq[Block] = key =>
    Seq(
      Title(List(Text(titleFor(key))), Id("title") + Style.title),
      section(1, 1, Seq(section(2, 2))),
      section(1, 3, Seq(section(2, 4)))
    )

}

object SampleConfig {

  def versioned(flag: Boolean): ConfigBuilder => ConfigBuilder =
    _.withValue(LaikaKeys.versioned, flag)

  val globalLinkValidation: ConfigBuilder => ConfigBuilder = _.withValue(LinkValidation.Global())

  def targetFormats(formats: String*): ConfigBuilder => ConfigBuilder =
    _.withValue(LaikaKeys.targetFormats, formats)

  def siteBaseURL(value: String): ConfigBuilder => ConfigBuilder =
    _.withValue(LaikaKeys.siteBaseURL, value)

  def title(text: String): ConfigBuilder => ConfigBuilder = _.withValue(LaikaKeys.title, text)

}
