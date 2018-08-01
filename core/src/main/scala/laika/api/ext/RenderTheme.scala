/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.api.ext

import laika.api.config.OperationConfig
import laika.io.DocumentType.Static
import laika.io.{DocumentType, InputTree, InputTreeOps}
import laika.parse.css.Styles.StyleDeclarationSet
import laika.tree.Documents.{Document, DocumentTree, StaticDocument, TreeContent}
import laika.tree.Elements.RenderFunction
import laika.tree.Paths.{Path, Root}
import laika.tree.Templates.TemplateRoot

/**
  * @author Jens Halm
  */
trait RenderTheme {

  type Writer

  def customRenderer: Writer => RenderFunction

  def defaultTemplate: Option[TemplateRoot]

  def defaultStyles: StyleDeclarationSet

  def staticDocuments: StaticDocuments

  def defaultTemplateOrFallback: TemplateRoot = defaultTemplate.getOrElse(TemplateRoot.fallback)

}

case class StaticDocuments (tree: DocumentTree) {

  def merge (base: DocumentTree): DocumentTree = {

    def mergeContent (content: Seq[TreeContent]): Seq[TreeContent] = {
      val trees = content.collect{ case t: DocumentTree => t }.groupBy(_.path).mapValues(_.reduceLeft(mergeTrees)).values.toList
      (content.filter(_.isInstanceOf[Document]) ++ trees).sortBy(_.position)
    }

    def mergeTrees (left: DocumentTree, right: DocumentTree): DocumentTree = {
      right.copy(
        content = mergeContent(left.content ++ right.content),
        additionalContent = left.additionalContent ++ right.additionalContent
      )
    }

    mergeTrees(tree, base)

  }

}

object StaticDocuments extends InputTreeOps {

  val empty = StaticDocuments(DocumentTree(Root, Nil))

  override type InputTreeResult = StaticDocuments

  override def config: OperationConfig = OperationConfig(Seq(new ExtensionBundle {
    override def docTypeMatcher: PartialFunction[Path, DocumentType] = { case _ => Static }
  }))

  override def fromInputTree (inputTree: InputTree): StaticDocuments = {
    def collectDocuments (currentTree: InputTree): DocumentTree = {
      val trees = currentTree.subtrees map collectDocuments
      val static = currentTree.staticDocuments map StaticDocument
      DocumentTree(currentTree.path, trees, additionalContent = static, sourcePaths = currentTree.sourcePaths)
    }
    StaticDocuments(collectDocuments(inputTree))
  }

}
