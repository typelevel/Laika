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

package laika.api.config

import com.typesafe.config.{Config, ConfigFactory}
import laika.api.ext.{ExtensionBundle, ParserDefinitionBuilders}
import laika.api.ext.ExtensionBundle.LaikaDefaults
import laika.directive.{DirectiveSupport, StandardDirectives}
import laika.factory.{ParserFactory, RendererFactory}
import laika.io.{DefaultDocumentTypeMatcher, DocumentType}
import laika.io.DocumentType.Ignored
import laika.parse.core.Parser
import laika.parse.core.combinator.Parsers
import laika.parse.core.markup.DocumentParser.InvalidElement
import laika.rewrite.{DocumentCursor, RewriteRules}
import laika.tree.Documents.Document
import laika.tree.Elements.{Fatal, MessageLevel, RewriteRule}
import laika.tree.Paths.Path

/**
  * @author Jens Halm
  */
case class OperationConfig (bundles: Seq[ExtensionBundle] = Nil,
                            bundleFilter: BundleFilter = BundleFilter(),
                            minMessageLevel: MessageLevel = Fatal,
                            parallel: Boolean = false) {

  private lazy val mergedBundle: ExtensionBundle = ExtensionBundle.mergeBundles(bundles.filter(bundleFilter))


  lazy val baseConfig: Config = mergedBundle.baseConfig

  lazy val docTypeMatcher: Path => DocumentType = mergedBundle.docTypeMatcher.lift.andThen(_.getOrElse(Ignored))

  lazy val parserDefinitions: ParserDefinitionBuilders = mergedBundle.parserDefinitions

  lazy val configHeaderParser: Path => Parser[Either[InvalidElement, Config]] = {
    val allParsers = mergedBundle.parserDefinitions.configHeaderParsers :+
      { _:Path => Parsers.success(Right(ConfigFactory.empty)) }
    { path: Path => allParsers.map(_(path)).reduce(_ | _) }
  }

  lazy val rewriteRule: DocumentCursor => RewriteRule = RewriteRules.chainFactories(mergedBundle.rewriteRules)

  def rewriteRuleFor (doc: Document): RewriteRule = rewriteRule(DocumentCursor(doc))

  // TODO - withBase rendererFactory.defaultTheme - withBase Theme.fallback (== Theme[Nothing])
  def themeFor[Writer] (rendererFactory: RendererFactory[Writer]) = mergedBundle.themeFor(rendererFactory)


  def withBundles (bundles: Seq[ExtensionBundle]): OperationConfig = copy(bundles = this.bundles ++ bundles)

  def withBundlesFor (factory: ParserFactory): OperationConfig = {
    val docTypeMatcher = new ExtensionBundle {
      override val docTypeMatcher: PartialFunction[Path, DocumentType] =
        DefaultDocumentTypeMatcher.forMarkup(factory.fileSuffixes)
      override val useInStrictMode: Boolean = true
    }
    copy(bundles = this.bundles ++ factory.extensions :+ docTypeMatcher)
  }

  def forStrictMode: OperationConfig = copy(bundleFilter = bundleFilter.copy(strict = true))

  def forRawContent: OperationConfig = copy(bundleFilter = bundleFilter.copy(acceptRawContent = true))

}

object OperationConfig {

  val default: OperationConfig = OperationConfig(
    bundles = Seq(LaikaDefaults, DirectiveSupport, StandardDirectives)
  )

}

case class BundleFilter (strict: Boolean = false, acceptRawContent: Boolean = false) extends (ExtensionBundle => Boolean) {
  override def apply (bundle: ExtensionBundle): Boolean =
    (!strict || bundle.useInStrictMode) && (acceptRawContent || !bundle.acceptRawContent)
}
