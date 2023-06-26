/*
 * Copyright 2012-2023 the original author or authors.
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

package laika.ast

import laika.config.{ Config, ConfigError, ConfigParser, Origin }
import laika.ast.Path.Root
import cats.implicits._
import laika.ast.DocumentTreeBuilder.BuilderPart
import laika.config.Config.IncludeMap
import laika.config.Origin.{ DocumentScope, TreeScope }
import laika.rewrite.nav.TitleDocumentConfig

/** API for a safe and concise way of constructing a `DocumentTree`.
  *
  * The hierarchy of the tree will be constructed based on the provided `Path` instances
  * while also ensuring that document configuration is wired up correctly (inheriting
  * the configuration from directories).
  *
  * @author Jens Halm
  */
class DocumentTreeBuilder private[laika] (parts: Map[Path, BuilderPart] = Map.empty) {

  import laika.collection.TransitionalCollectionOps._
  import DocumentTreeBuilder._

  private def extract(
      content: Seq[TreeContent],
      docName: String
  ): (Option[Document], Seq[TreeContent]) = {
    val extracted = content.collectFirst { case d: Document if d.path.basename == docName => d }
    val filteredContent =
      if (extracted.nonEmpty) content.filterNot(_.path.basename == docName) else content
    (extracted, filteredContent)
  }

  private def buildNode(path: Path, content: Seq[BuilderPart]): TreePart = {

    val treeContent = content.collect {
      case tree: TreePart     => tree
      case markup: MarkupPart => markup
      case doc: DocumentPart  => doc
    }
    val templates   = content.collect { case TemplatePart(doc) => doc }

    val hoconConfig = content.collect { case c: HoconPart => c }
    val treeConfig  = content.collect { case c: ConfigPart => c }

    TreePart(path, treeContent, templates, hoconConfig, treeConfig)
  }

  private def resolveAndBuildDocument(
      doc: UnresolvedDocument,
      baseConfig: Config,
      includes: IncludeMap
  ): Either[ConfigError, Document] =
    doc.config.resolve(Origin(DocumentScope, doc.document.path), baseConfig, includes).map(config =>
      doc.document.copy(config = config)
    )

  private def applyBaseConfig(doc: Document, baseConfig: Config): Document =
    doc.copy(config =
      doc.config.withFallback(baseConfig).withOrigin(Origin(DocumentScope, doc.path))
    )

  private def resolveAndBuildTree(
      result: TreePart,
      baseConfig: Config,
      includes: IncludeMap
  ): Either[ConfigError, DocumentTree] = {

    val resolvedConfig =
      result.hocon.foldLeft[Either[ConfigError, Config]](Right(result.mergeConfigs(baseConfig))) {
        case (acc, unresolved) =>
          acc.flatMap(base =>
            unresolved.config.resolve(Origin(TreeScope, unresolved.path), base, includes)
          )
      }

    for {
      treeConfig      <- resolvedConfig
      titleName       <- TitleDocumentConfig.inputName(treeConfig)
      resolvedContent <- result.content.toVector.traverse {
        case tree: TreePart     => resolveAndBuildTree(tree, treeConfig, includes)
        case markup: MarkupPart => resolveAndBuildDocument(markup.doc, treeConfig, includes)
        case doc: DocumentPart  => Right(applyBaseConfig(doc.doc, treeConfig))
      }
    } yield {
      val (title, content) = extract(resolvedContent, titleName)
      DocumentTree(result.path, content, title, result.templates, treeConfig)
    }
  }

  private def buildTree(
      result: TreePart,
      baseConfig: Config,
      defaultTitleName: String
  ): DocumentTree = {

    val treeConfig       = result.mergeConfigs(baseConfig)
    val titleName        = TitleDocumentConfig.inputName(treeConfig).getOrElse(defaultTitleName)
    val allContent       = result.content.flatMap {
      case tree: TreePart    => Some(buildTree(tree, treeConfig, titleName))
      case doc: DocumentPart => Some(applyBaseConfig(doc.doc, treeConfig))
      case _: MarkupPart     => None
    }
    val (title, content) = extract(allContent, titleName)
    DocumentTree(result.path, content, title, result.templates, treeConfig)
  }

  private def collectStyles(parts: Seq[BuilderPart]): Map[String, StyleDeclarationSet] = parts
    .collect { case StylePart(styleSet, format) => (format, styleSet) }
    .groupBy(_._1)
    .mapValuesStrict(
      _
        .map(_._2)
        .sortBy(set => (set.precedence, set.paths.headOption.fold("")(_.toString)))
        .reduce(_ ++ _)
    )
    .withDefaultValue(StyleDeclarationSet.empty)

  private[laika] def resolveAndBuildRoot(
      baseConfig: Config,
      includes: IncludeMap
  ): Either[ConfigError, DocumentTreeRoot] = {
    val allParts = parts.values.toList
    val tree     = TreeBuilder.build(allParts, buildNode)
    val styles   = collectStyles(allParts)
    for {
      resolvedTree <- resolveAndBuildTree(tree, baseConfig, includes)
    } yield {
      val (cover, content) = extract(resolvedTree.content, "cover")
      val rootTree         = resolvedTree.copy(content = content)
      DocumentTreeRoot(rootTree, cover, styles, includes = includes)
    }
  }

  private def addParts(newParts: List[BuilderPart]) = {
    val newMap = newParts.foldLeft(parts) { case (map, part) =>
      map + ((part.path, part))
    }
    new DocumentTreeBuilder(newMap)
  }

  def addDocuments(docs: List[Document]): DocumentTreeBuilder =
    addParts(docs.map(DocumentPart(_)))

  def addDocument(doc: Document): DocumentTreeBuilder =
    new DocumentTreeBuilder(parts + ((doc.path, DocumentPart(doc))))

//  def addConfig(mountPath: Path, config: Config): DocumentTreeBuilder =
//    new DocumentTreeBuilder(parts + ((mountPath, ConfigPart(mountPath, config))))

  def addConfig(config: Config): DocumentTreeBuilder =
    new DocumentTreeBuilder(parts + ((config.origin.path, ConfigPart(config.origin.path, config))))

  def addTemplates(docs: List[TemplateDocument]): DocumentTreeBuilder =
    addParts(docs.map(TemplatePart(_)))

  def addTemplate(doc: TemplateDocument): DocumentTreeBuilder =
    new DocumentTreeBuilder(parts + ((doc.path, TemplatePart(doc))))

  def buildRoot: DocumentTreeRoot = buildRoot(Config.empty)

  def buildRoot(baseConfig: Config): DocumentTreeRoot = {
    val allParts         = parts.values.toList
    val tree             = TreeBuilder.build(allParts, buildNode)
    val styles           = collectStyles(allParts)
    val resolvedTree     = buildTree(tree, baseConfig, TitleDocumentConfig.defaultInputName)
    val (cover, content) = extract(resolvedTree.content, "cover")
    val rootTree         = resolvedTree.copy(content = content)
    DocumentTreeRoot(rootTree, cover, styles)
  }

  def build: DocumentTree = build(Config.empty)

  def build(baseConfig: Config): DocumentTree = {
    val tree = TreeBuilder.build(parts.values.toList, buildNode)
    buildTree(tree, baseConfig, TitleDocumentConfig.defaultInputName)
  }

}

private[laika] object DocumentTreeBuilder {

  sealed trait BuilderPart extends Navigatable

  sealed trait TreeContentPart extends BuilderPart

  case class DocumentPart(doc: Document) extends TreeContentPart {
    val path: Path = doc.path
  }

  case class MarkupPart(doc: UnresolvedDocument) extends TreeContentPart {
    val path: Path = doc.document.path
  }

  case class TemplatePart(doc: TemplateDocument) extends BuilderPart {
    val path: Path = doc.path
  }

  case class StylePart(doc: StyleDeclarationSet, format: String) extends BuilderPart {
    val path: Path = doc.paths.head
  }

  case class HoconPart(path: Path, config: ConfigParser) extends BuilderPart

  case class ConfigPart(path: Path, config: Config) extends BuilderPart

  case class TreePart(
      path: Path,
      content: Seq[TreeContentPart],
      templates: Seq[TemplateDocument],
      hocon: Seq[HoconPart],
      config: Seq[ConfigPart]
  ) extends TreeContentPart {

    def mergeConfigs(baseConfig: Config): Config =
      config.foldLeft(baseConfig) { case (acc, conf) =>
        conf.config.withFallback(acc).withOrigin(Origin(TreeScope, conf.path))
      }

  }

}
