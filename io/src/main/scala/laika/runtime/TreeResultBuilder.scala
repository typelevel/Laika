/*
 * Copyright 2012-2019 the original author or authors.
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

package laika.runtime

import laika.config.{Config, ConfigError, Origin}
import laika.ast.Path.Root
import laika.ast.{Document, DocumentTree, DocumentTreeRoot, Navigatable, Path, StyleDeclarationSet, TemplateDocument, TreeBuilder, UnresolvedDocument}
import laika.bundle.UnresolvedConfig
import cats.implicits._
import laika.config.Origin.{DocumentScope, TreeScope}

/**
  * @author Jens Halm
  */
object TreeResultBuilder {

  import laika.collection.TransitionalCollectionOps._

  sealed trait ParserResult extends Navigatable

  case class MarkupResult (doc: UnresolvedDocument) extends ParserResult {
    val path: Path = doc.document.path
  }
  case class TemplateResult (doc: TemplateDocument)  extends ParserResult {
    val path: Path = doc.path
  }
  case class StyleResult (doc: StyleDeclarationSet, format: String) extends ParserResult {
    val path: Path = doc.paths.head
  }
  case class ConfigResult (path: Path, config: UnresolvedConfig) extends ParserResult

  type UnresolvedContent = Either[UnresolvedDocument, TreeResult]
  
  case class TreeResult (path: Path, 
                         content: Seq[Either[UnresolvedDocument, TreeResult]], 
                         titleDoc: Option[UnresolvedDocument],
                         templates: Seq[TemplateDocument],
                         configs: Seq[ConfigResult]) extends ParserResult

  def buildNode (path: Path, content: Seq[ParserResult]): TreeResult = {
    
    def isTitleDoc (doc: Document): Boolean = doc.path.basename == "title"
    
    val titleDoc = content.collectFirst { case MarkupResult(doc) if isTitleDoc(doc.document) => doc }
    val subTrees = content.collect { case tree: TreeResult => Right(tree) }.sortBy(_.right.get.path.name)
    val treeContent = content.collect { case MarkupResult(doc) if !isTitleDoc(doc.document) => Left(doc) } ++ subTrees
    val templates = content.collect { case TemplateResult(doc) => doc }

    val treeConfig = content.collect { case c: ConfigResult => c }

    TreeResult(path, treeContent, titleDoc, templates, treeConfig)
  }

  def resolveConfig (doc: UnresolvedDocument, baseConfig: Config): Either[ConfigError, Document] =
    doc.config.resolve(Origin(DocumentScope, doc.document.path), baseConfig).map(config => doc.document.copy(config = config))
  
  def resolveConfig (result: TreeResult, baseConfig: Config): Either[ConfigError, DocumentTree] = {
    
    val resolvedConfig = result.configs.foldLeft[Either[ConfigError, Config]](Right(baseConfig)) {
      case (acc, unresolved) => acc.flatMap(base => unresolved.config.resolve(Origin(TreeScope, unresolved.path), base))
    }
    
    resolvedConfig.flatMap { treeConfig =>
      val resolvedContent = result.content.toVector.traverse(
        _.fold(resolveConfig(_, treeConfig), resolveConfig(_, treeConfig))
        //case Right(tree) => resolvedConfig(tree, treeConfig)
        //case Left(doc)   => resolveConfig(doc, treeConfig)
      )
      
      for {
        content <- resolvedContent
        title   <- result.titleDoc.map(resolveConfig(_, baseConfig)).sequence
      } yield DocumentTree(result.path, content, title, result.templates, treeConfig)
    }
  }

  def buildTree (results: Seq[ParserResult], baseConfig: Config): Either[ConfigError, DocumentTreeRoot] = {
    
    val coverDoc = results.collectFirst {
      case MarkupResult(doc) if doc.document.path.parent == Root && doc.document.path.basename == "cover" => doc
    }
    val tree = TreeBuilder.build(results.filterNot(res => coverDoc.exists(_.document.path == res.path)), buildNode)

    val styles = results
      .collect { case StyleResult(styleSet, format) => (format, styleSet) }
      .groupBy(_._1)
      .mapValuesStrict(_.map(_._2).reduce(_ ++ _))
      .withDefaultValue(StyleDeclarationSet.empty)
    
    for {
      resolvedTree  <- resolveConfig(tree, baseConfig)
      resolvedCover <- coverDoc.map(resolveConfig(_, resolvedTree.config)).sequence: Either[ConfigError, Option[Document]]
    } yield
      DocumentTreeRoot(resolvedTree, resolvedCover, styles)
  }
  
}
