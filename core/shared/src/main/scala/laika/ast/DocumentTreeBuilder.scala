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
import cats.implicits.*
import laika.config.Config.IncludeMap
import laika.config.Origin.{ DocumentScope, TreeScope }
import laika.rewrite.nav.TitleDocumentConfig

import scala.collection.mutable

/** API for a safe and concise way of constructing a `DocumentTree`.
  *
  * The hierarchy of the tree will be constructed based on the provided `Path` instances
  * while also ensuring that document configuration is wired up correctly (inheriting
  * the configuration from directories).
  *
  * @author Jens Halm
  */
class DocumentTreeBuilder private[laika] (parts: List[DocumentTreeBuilder.BuilderPart] = Nil) {

  import laika.collection.TransitionalCollectionOps.*
  import DocumentTreeBuilder.*

  private[laika] lazy val distinctParts: List[BuilderPart] = {
    /* distinctBy does not exist in 2.12
       insertion happens at head, so later additions are dropped and the final result is reversed
       this is cheaper than using `ListMap` which has O(n) insertion.
     */
    val paths = mutable.Set[Path]()
    parts.filter(p => paths.add(p.path)).reverse
  }

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
      new DocumentTree(result.path, content, title, result.templates, treeConfig)
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
    new DocumentTree(result.path, content, title, result.templates, treeConfig)
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

  /** Internal entry point for the parser runtime in laika-io.
    * Placed here as it shares a lot of functionality with the public builder API,
    * but using additional sub-types of `BuilderPart` for parsed, but yet unresolved
    * documents and configurations.
    */
  private[laika] def resolveAndBuildRoot(
      baseConfig: Config,
      includes: IncludeMap
  ): Either[ConfigError, DocumentTreeRoot] = {
    val tree   = TreeBuilder.build(distinctParts, buildNode)
    val styles = collectStyles(distinctParts)
    for {
      resolvedTree <- resolveAndBuildTree(tree, baseConfig, includes)
    } yield {
      val (cover, content) = extract(resolvedTree.content, "cover")
      val rootTree         = resolvedTree.replaceContent(content)
      DocumentTreeRoot(rootTree, cover, styles, includes = includes)
    }
  }

  private[laika] def addPart(newPart: BuilderPart) =
    new DocumentTreeBuilder(newPart +: parts)

  private[laika] def addParts(newParts: List[BuilderPart]) =
    new DocumentTreeBuilder(newParts.reverse ++ parts)

  /** Add the specified documents to the builder.
    * Existing instances with identical paths will be overridden.
    */
  def addDocuments(docs: List[Document]): DocumentTreeBuilder =
    addParts(docs.map(DocumentPart(_)))

  /** Add the specified document to the builder.
    * Existing instances with identical paths will be overridden.
    */
  def addDocument(doc: Document): DocumentTreeBuilder =
    new DocumentTreeBuilder(DocumentPart(doc) +: parts)

  /** Add the specified tree configuration to the builder.
    * The path it will be assigned to will be taken from the `origin`
    * property of the `Config` instance.
    * Existing instances with identical paths will be overridden.
    *
    * For assigning a configuration to a specific document and not
    * an entire tree or subtree, set the `config` property of
    * a `Document` instance directly before adding it to the builder.
    */
  def addConfig(config: Config): DocumentTreeBuilder =
    new DocumentTreeBuilder(ConfigPart(config.origin.path, config) +: parts)

  /** Add the specified templates to the builder.
    * Existing instances with identical paths will be overridden.
    */
  def addTemplates(docs: List[TemplateDocument]): DocumentTreeBuilder =
    addParts(docs.map(TemplatePart(_)))

  /** Add the specified template to the builder.
    * Existing instances with identical paths will be overridden.
    */
  def addTemplate(doc: TemplateDocument): DocumentTreeBuilder =
    new DocumentTreeBuilder(TemplatePart(doc) +: parts)

  /** Builds a `DocumentTreeRoot` from the provided instances and wires the
    * configuration of documents to that of parent trees for proper inheritance.
    */
  def buildRoot: DocumentTreeRoot = buildRoot(Config.empty)

  /** Builds a `DocumentTreeRoot` from the provided instances, using the specified
    * `Config` instance as a base for the configuration of all trees and documents.
    * Also wires configuration of documents to that of parent trees for proper inheritance.
    */
  def buildRoot(baseConfig: Config): DocumentTreeRoot = {
    val tree             = build(baseConfig)
    val (cover, content) = extract(tree.content, "cover")
    DocumentTreeRoot(tree.replaceContent(content), cover)
  }

  /** Builds a `DocumentTree` from the provided instances and wires the
    * configuration of documents to that of parent trees for proper inheritance.
    */
  def build: DocumentTree = build(Config.empty)

  /** Builds a `DocumentTree` from the provided instances, using the specified
    * `Config` instance as a base for the configuration of all trees and documents.
    * Also wires configuration of documents to that of parent trees for proper inheritance.
    */
  def build(baseConfig: Config): DocumentTree = {
    val tree = TreeBuilder.build(distinctParts, buildNode)
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
