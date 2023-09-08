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

package laika.io.runtime

import laika.config.{ Config, ConfigError, ConfigParser, Origin }
import laika.ast.Path.Root
import laika.ast.{
  Document,
  DocumentTree,
  DocumentTreeRoot,
  Navigatable,
  Path,
  StyleDeclarationSet,
  TemplateDocument,
  TreeBuilder,
  TreeContent,
  UnresolvedDocument
}
import cats.implicits._
import laika.config.Config.IncludeMap
import laika.config.Origin.{ DocumentScope, TreeScope }
import laika.io.model.FilePath
import laika.rewrite.nav.TitleDocumentConfig

import scala.annotation.nowarn

@nowarn("cat=deprecation")
object TreeResultBuilder {

  import laika.collection.TransitionalCollectionOps._

  sealed trait ParserResult extends Navigatable {
    def sourceFile: Option[FilePath]
  }

  sealed trait TreeContentResult extends ParserResult

  case class DocumentResult(doc: Document) extends TreeContentResult {
    val path: Path                   = doc.path
    val sourceFile: Option[FilePath] = None
  }

  case class MarkupResult(doc: UnresolvedDocument, sourceFile: Option[FilePath] = None)
      extends TreeContentResult {
    val path: Path = doc.document.path
  }

  case class TemplateResult(doc: TemplateDocument, sourceFile: Option[FilePath] = None)
      extends ParserResult {
    val path: Path = doc.path
  }

  case class StyleResult(
      doc: StyleDeclarationSet,
      format: String,
      sourceFile: Option[FilePath] = None
  ) extends ParserResult {
    val path: Path = doc.paths.head
  }

  case class HoconResult(path: Path, config: ConfigParser, sourceFile: Option[FilePath] = None)
      extends ParserResult

  case class ConfigResult(path: Path, config: Config) extends ParserResult {
    val sourceFile: Option[FilePath] = None
  }

  type UnresolvedContent = Either[UnresolvedDocument, TreeResult]

  case class TreeResult(
      path: Path,
      content: Seq[TreeContentResult],
      titleDoc: Option[UnresolvedDocument],
      templates: Seq[TemplateDocument],
      hocon: Seq[HoconResult],
      config: Seq[ConfigResult]
  ) extends TreeContentResult {
    val sourceFile: Option[FilePath] = None
  }

  @deprecated("0.19.3", "internal API in version 1.x")
  def buildNode(path: Path, content: Seq[ParserResult]): TreeResult = {

    val treeContent = content.collect {
      case tree: TreeResult     => tree
      case markup: MarkupResult => markup
      case doc: DocumentResult  => doc
    }
    val templates   = content.collect { case TemplateResult(doc, _) => doc }

    val hoconConfig = content.collect { case c: HoconResult => c }
    val treeConfig  = content.collect { case c: ConfigResult => c }

    TreeResult(path, treeContent, None, templates, hoconConfig, treeConfig)
  }

  @deprecated("0.19.3", "internal API in version 1.x")
  def resolveConfig(
      doc: UnresolvedDocument,
      baseConfig: Config,
      includes: IncludeMap
  ): Either[ConfigError, Document] =
    doc.config.resolve(Origin(DocumentScope, doc.document.path), baseConfig, includes).map(config =>
      doc.document.copy(config = config)
    )

  @deprecated("0.19.3", "internal API in version 1.x")
  def resolveConfig(doc: Document, baseConfig: Config): Either[ConfigError, Document] =
    Right(
      doc.copy(config =
        doc.config.withFallback(baseConfig).withOrigin(Origin(DocumentScope, doc.path))
      )
    )

  @deprecated("0.19.3", "internal API in version 1.x")
  def resolveConfig(
      result: TreeResult,
      baseConfig: Config,
      includes: IncludeMap,
      titleDocName: Option[String] = None
  ): Either[ConfigError, DocumentTree] = {

    val mergedGeneratedConfig = result.config.foldLeft(baseConfig) { case (acc, conf) =>
      conf.config.withFallback(acc).withOrigin(Origin(TreeScope, conf.path))
    }

    val resolvedConfig =
      result.hocon.foldLeft[Either[ConfigError, Config]](Right(mergedGeneratedConfig)) {
        case (acc, unresolved) =>
          acc.flatMap(base =>
            unresolved.config.resolve(Origin(TreeScope, unresolved.path), base, includes)
          )
      }

    def isTitleDoc(titleName: String)(doc: TreeContent): Boolean = doc.path.basename == titleName

    for {
      treeConfig      <- resolvedConfig
      titleName       <- titleDocName.fold(TitleDocumentConfig.inputName(treeConfig))(Right.apply)
      resolvedContent <- result.content.toVector.traverse {
        case tree: TreeResult     => resolveConfig(tree, treeConfig, includes, Some(titleName))
        case markup: MarkupResult => resolveConfig(markup.doc, treeConfig, includes)
        case doc: DocumentResult  => resolveConfig(doc.doc, treeConfig)
      }
      title = resolvedContent.collectFirst { case d: Document if isTitleDoc(titleName)(d) => d }
    } yield {
      new DocumentTree(
        result.path,
        resolvedContent.filterNot(isTitleDoc(titleName)),
        title,
        result.templates,
        treeConfig
      )
    }
  }

  @deprecated("0.19.3", "internal API in version 1.x")
  def buildTree(
      results: Seq[ParserResult],
      baseConfig: Config,
      includes: IncludeMap
  ): Either[ConfigError, DocumentTreeRoot] = {

    val coverDoc = results.collectFirst {
      case MarkupResult(doc, _)
          if doc.document.path.parent == Root && doc.document.path.basename == "cover" =>
        doc
    }
    val tree     = TreeBuilder.build(
      results.filterNot(res => coverDoc.exists(_.document.path == res.path)),
      buildNode
    )

    val styles = results
      .collect { case StyleResult(styleSet, format, _) => (format, styleSet) }
      .groupBy(_._1)
      .mapValuesStrict(
        _
          .map(_._2)
          .sortBy(set => (set.precedence, set.paths.headOption.fold("")(_.toString)))
          .reduce(_ ++ _)
      )
      .withDefaultValue(StyleDeclarationSet.empty)

    for {
      resolvedTree  <- resolveConfig(tree, baseConfig, includes)
      resolvedCover <- coverDoc.traverse(resolveConfig(_, resolvedTree.config, includes)): Either[
        ConfigError,
        Option[Document]
      ]
    } yield DocumentTreeRoot(resolvedTree, resolvedCover, styles, includes = includes)
  }

}
