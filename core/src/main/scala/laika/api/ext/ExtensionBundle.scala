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
import laika.factory.RendererFactory
import laika.io.{DefaultDocumentTypeMatcher, DocumentType}
import laika.parse.core.markup.RecursiveParsers
import laika.parse.css.CSSParsers
import laika.parse.css.Styles.StyleDeclarationSet
import laika.rewrite.{DocumentCursor, LinkResolver, SectionBuilder}
import laika.tree.Elements._
import laika.tree.Paths.Path
import laika.tree.Templates.TemplateRoot

import scala.annotation.tailrec

/**
  * @author Jens Halm
  */
trait ExtensionBundle { self =>

  def baseConfig: Config = ConfigFactory.empty

  /** Specifies the function to use for determining the document type
    * of the input based on its path. Any path for which this function
    * is not defined will be processed by the remaining installed bundles.
    * The documents for paths for which none of the extensions provides
    * a `DocumentType` will be ignored.
    */
  def docTypeMatcher: PartialFunction[Path, DocumentType] = PartialFunction.empty

  def parserDefinitions: ParserDefinitionBuilders = ParserDefinitionBuilders()

  def rewriteRules: Seq[DocumentCursor => RewriteRule] = Seq.empty

  def themeFor[Writer] (rendererFactory: RendererFactory[Writer]): Theme[Writer] = Theme[Writer]()


  // for providing APIs like registering Directives
  def processExtension: PartialFunction[ExtensionBundle, ExtensionBundle] = PartialFunction.empty

  def withBase (bundle: ExtensionBundle): ExtensionBundle = new ExtensionBundle {

    override def baseConfig = self.baseConfig.withFallback(bundle.baseConfig)

    override def docTypeMatcher = self.docTypeMatcher.orElse(bundle.docTypeMatcher)

    override def parserDefinitions: ParserDefinitionBuilders = self.parserDefinitions withBase bundle.parserDefinitions

    override def rewriteRules = self.rewriteRules ++ bundle.rewriteRules

    override def themeFor[Writer] (rendererFactory: RendererFactory[Writer]) =
      self.themeFor(rendererFactory).withBase(bundle.themeFor(rendererFactory))

    override def processExtension: PartialFunction[ExtensionBundle, ExtensionBundle] =
      self.processExtension.orElse(bundle.processExtension)
  }

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

    override def docTypeMatcher: PartialFunction[Path, DocumentType] = DefaultDocumentTypeMatcher.get

    override def parserDefinitions: ParserDefinitionBuilders = ParserDefinitionBuilders(
      styleSheetParser = Some(CSSParsers.styleDeclarationSet)
    )

    override def rewriteRules: Seq[DocumentCursor => RewriteRule] = Seq(LinkResolver, SectionBuilder)

  }

}

trait ExtensionFactory {

  def create (recursiveParsers: RecursiveParsers): ExtensionBundle

}

case class Theme[Writer] (customRenderers: Seq[Writer => RenderFunction] = Nil,
                          defaultTemplate: Option[TemplateRoot] = None,
                          defaultStyles: StyleDeclarationSet = StyleDeclarationSet.empty
                          /*, staticDocuments: InputProvider = InputProvider.empty TODO - implement */) {

  def withBase(other: Theme[Writer]): Theme[Writer] = Theme(
    customRenderers ++ other.customRenderers,
    defaultTemplate.orElse(other.defaultTemplate),
    other.defaultStyles ++ defaultStyles
    /* staticDocuments.merge(other.staticDocuments TODO - implement + simplify InputProvider and related types */
  )

}
