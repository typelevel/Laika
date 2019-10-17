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

import laika.api.config.Config
import laika.ast.Path.Root
import laika.ast.{Document, DocumentTree, DocumentTreeRoot, Navigatable, Path, StyleDeclarationSet, TemplateDocument, TreeBuilder}

/**
  * @author Jens Halm
  */
object TreeResultBuilder {

  import laika.collection.TransitionalCollectionOps._

  sealed trait ParserResult extends Navigatable

  case class MarkupResult (doc: Document) extends ParserResult {
    val path: Path = doc.path
  }
  case class TemplateResult (doc: TemplateDocument)  extends ParserResult {
    val path: Path = doc.path
  }
  case class StyleResult (doc: StyleDeclarationSet, format: String) extends ParserResult {
    val path: Path = doc.paths.head
  }
  case class ConfigResult (path: Path, config: Config) extends ParserResult

  case class TreeResult (tree: DocumentTree) extends ParserResult {
    val path: Path = tree.path
  }

  def buildNode (baseConfig: Config)(path: Path, content: Seq[ParserResult]): TreeResult = {
    def isTitleDoc (doc: Document): Boolean = doc.path.basename == "title"
    val titleDoc = content.collectFirst { case MarkupResult(doc) if isTitleDoc(doc) => doc }
    val subTrees = content.collect { case TreeResult(doc) => doc }.sortBy(_.path.name)
    val treeContent = content.collect { case MarkupResult(doc) if !isTitleDoc(doc) => doc } ++ subTrees
    val templates = content.collect { case TemplateResult(doc) => doc }

    val treeConfig = content.collect { case ConfigResult(_, config) => config }
    val rootConfig = if (path == Root) Seq(baseConfig) else Nil
    val fullConfig = (treeConfig.toList ++ rootConfig) reduceLeftOption (_ withFallback _) getOrElse Config.empty

    TreeResult(DocumentTree(path, treeContent, titleDoc, templates, fullConfig))
  }

  def buildTree (results: Seq[ParserResult], baseConfig: Config): DocumentTreeRoot = {
    val coverDoc = results.collectFirst {
      case MarkupResult(doc) if doc.path.parent == Root && doc.path.basename == "cover" => doc
    }
    val tree = TreeBuilder.build(results.filterNot(res => coverDoc.exists(_.path == res.path)), buildNode(baseConfig)).tree

    val styles = results
      .collect { case StyleResult(styleSet, format) => (format, styleSet) }
      .groupBy(_._1)
      .mapValuesStrict(_.map(_._2).reduce(_ ++ _))
      .withDefaultValue(StyleDeclarationSet.empty)

    DocumentTreeRoot(tree, coverDoc, styles)
  }
  
}
