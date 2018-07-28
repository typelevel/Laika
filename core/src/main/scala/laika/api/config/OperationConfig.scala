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

import com.typesafe.config.Config
import laika.api.ext.ExtensionBundle.LaikaDefaults
import laika.api.ext.{ExtensionBundle, MarkupExtensions, RewriteRules}
import laika.directive.{ConfigHeaderParser, DirectiveSupport, StandardDirectives}
import laika.factory.MarkupParser
import laika.io.DocumentType.Ignored
import laika.io.{DefaultDocumentTypeMatcher, DocumentType}
import laika.parse.core.Parser
import laika.parse.core.combinator.Parsers.success
import laika.parse.core.markup.DocumentParser.InvalidElement
import laika.parse.css.Styles.StyleDeclaration
import laika.rewrite.DocumentCursor
import laika.tree.Documents.Document
import laika.tree.Elements.{Fatal, MessageLevel, RewriteRule}
import laika.tree.Paths.Path
import laika.tree.Templates.TemplateRoot

/**
  * @author Jens Halm
  */
case class OperationConfig (bundles: Seq[ExtensionBundle] = Nil,
                            bundleFilter: BundleFilter = BundleFilter(),
                            minMessageLevel: MessageLevel = Fatal,
                            renderFormatted: Boolean = true,
                            parallel: Boolean = false) extends RenderConfig {

  private lazy val mergedBundle: ExtensionBundle = ExtensionBundle.mergeBundles(bundles.filter(bundleFilter))


  lazy val baseConfig: Config = mergedBundle.baseConfig

  lazy val docTypeMatcher: Path => DocumentType = mergedBundle.docTypeMatcher.lift.andThen(_.getOrElse(Ignored))

  lazy val markupExtensions: MarkupExtensions = mergedBundle.parsers.markupExtensions

  lazy val configHeaderParser: Path => Parser[Either[InvalidElement, Config]] =
    ConfigHeaderParser.merged(mergedBundle.parsers.configHeaderParsers :+ ConfigHeaderParser.fallback)

  lazy val styleSheetParser: Parser[Set[StyleDeclaration]] =
    mergedBundle.parsers.styleSheetParser.getOrElse(success(Set.empty[StyleDeclaration]))

  lazy val templateParser: Option[Parser[TemplateRoot]] = mergedBundle.parsers.templateParser

  lazy val rewriteRule: DocumentCursor => RewriteRule =
    RewriteRules.chainFactories(mergedBundle.rewriteRules ++ RewriteRules.defaults)

  def rewriteRuleFor (doc: Document): RewriteRule = rewriteRule(DocumentCursor(doc))

  // TODO - withBase rendererFactory.defaultTheme - withBase Theme.fallback
  lazy val themes = mergedBundle.themes


  def withBundles (bundles: Seq[ExtensionBundle]): OperationConfig = copy(bundles = this.bundles ++ bundles)

  def withBundlesFor (factory: MarkupParser): OperationConfig = {
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

trait RenderConfig {
  def minMessageLevel: MessageLevel
  def renderFormatted: Boolean
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
