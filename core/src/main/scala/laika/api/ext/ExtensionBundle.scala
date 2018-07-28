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

import com.typesafe.config.{Config, ConfigFactory}
import laika.api.config.OperationConfig
import laika.factory.RendererFactory
import laika.io.DocumentType.Static
import laika.io.{DefaultDocumentTypeMatcher, DocumentType, InputTree, InputTreeOps}
import laika.parse.css.CSSParsers
import laika.parse.css.Styles.StyleDeclarationSet
import laika.rewrite.{DocumentCursor, LinkResolver, SectionBuilder}
import laika.tree.Documents.{Document, DocumentTree, StaticDocument, TreeContent}
import laika.tree.Elements._
import laika.tree.Paths.{Path, Root}
import laika.tree.Templates.TemplateRoot

import scala.annotation.tailrec

/**
  * @author Jens Halm
  */
trait ExtensionBundle { self =>

  /** Base configuration that serves as a fallback for
    * configuration files in the source directories
    * and/or config headers in markup and template documents.
    */
  def baseConfig: Config = ConfigFactory.empty

  /** Specifies the function to use for determining the document type
    * of the input based on its path. Any path for which this function
    * is not defined will be processed by the remaining installed bundles.
    * The documents for paths for which none of the extensions provides
    * a `DocumentType` will be ignored.
    */
  def docTypeMatcher: PartialFunction[Path, DocumentType] = PartialFunction.empty

  /** Specifies extensions and/or replacements for parsers that deal with
    * text markup, templates, CSS or configuration headers.
    */
  def parsers: ParserConfig = ParserConfig()

  def rewriteRules: Seq[DocumentCursor => RewriteRule] = Seq.empty

  def themes: Seq[RenderTheme] = Seq.empty


  // for providing APIs like registering Directives
  def processExtension: PartialFunction[ExtensionBundle, ExtensionBundle] = PartialFunction.empty

  def withBase (base: ExtensionBundle): ExtensionBundle = new ExtensionBundle {

    override val useInStrictMode = self.useInStrictMode && base.useInStrictMode

    override lazy val baseConfig = self.baseConfig.withFallback(base.baseConfig)

    override lazy val docTypeMatcher = self.docTypeMatcher.orElse(base.docTypeMatcher)

    override lazy val parsers: ParserConfig = self.parsers withBase base.parsers

    /* flipped on purpose, base rules need to be applied first, so that app rules do not need to deal with potentially
       unknown node types */
    override lazy val rewriteRules = base.rewriteRules ++ self.rewriteRules

    override lazy val themes = self.themes ++ base.themes

    override def processExtension: PartialFunction[ExtensionBundle, ExtensionBundle] =
      self.processExtension.orElse(base.processExtension)
  }

  def useInStrictMode: Boolean = false

  def acceptRawContent: Boolean = false

}

object ExtensionBundle {

  // TODO - move this to OperationSetup.mergedBundle
  def mergeBundles (bundles: Seq[ExtensionBundle]): ExtensionBundle = {

    @tailrec
    def processBundles (past: Seq[ExtensionBundle], pending: Seq[ExtensionBundle]): Seq[ExtensionBundle] = pending match {
      case Nil => past
      case next :: rest =>
        val newPast = past.map(ex => next.processExtension.lift(ex).getOrElse(ex)) :+ next
        val newPending = rest.map(ex => next.processExtension.lift(ex).getOrElse(ex))
        processBundles(newPast, newPending)
    }

    processBundles(Nil, bundles).reverse.reduceLeftOption(_ withBase _).getOrElse(ExtensionBundle.Empty)

  }

  object Empty extends ExtensionBundle

  object LaikaDefaults extends ExtensionBundle {

    override val useInStrictMode = true

    override val docTypeMatcher: PartialFunction[Path, DocumentType] = DefaultDocumentTypeMatcher.get

    override val parsers: ParserConfig = ParserConfig(
      styleSheetParser = Some(CSSParsers.styleDeclarationSet)
    )

  }

}

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
