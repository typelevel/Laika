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
import laika.factory.{MarkupParser, RendererFactory}
import laika.io.DocumentType.Ignored
import laika.io.{DefaultDocumentTypeMatcher, DocumentType}
import laika.parse.core.Parser
import laika.parse.core.combinator.Parsers.success
import laika.parse.css.Styles.StyleDeclaration
import laika.rewrite.DocumentCursor
import laika.tree.Documents.Document
import laika.tree.Elements.{Fatal, InvalidElement, MessageLevel, RewriteRule}
import laika.tree.Paths.Path
import laika.tree.Templates.TemplateRoot

import scala.annotation.tailrec

/** Represents the configuration for an operation based on extension bundles and several flags.
  *
  * Provides convenient access to the result of merging the defined bundles and adding fallback
  * options wherever necessary.
  *
  * @param bundles all extension bundles defined by this operation
  * @param bundleFilter a filter that might deactivate some of the bundles based on user configuration
  * @param minMessageLevel specifies the minimum required level for a system message to get included into the output by a renderer
  * @param renderFormatted indicates whether rendering should include any formatting (line breaks or indentation)
  * @param parallel indicates whether parsers and renderers should run in parallel
  */
case class OperationConfig (bundles: Seq[ExtensionBundle] = Nil,
                            bundleFilter: BundleFilter = BundleFilter(),
                            minMessageLevel: MessageLevel = Fatal,
                            renderFormatted: Boolean = true,
                            parallel: Boolean = false) extends RenderConfig {

  private lazy val mergedBundle: ExtensionBundle = OperationConfig.mergeBundles(bundles.filter(bundleFilter))

  /** Base configuration merged from all defined extension bundles
    * that serves as a fallback for configuration files in the source
    * directories and/or config headers in markup and template documents.
    */
  lazy val baseConfig: Config = mergedBundle.baseConfig

  /** Specifies the function to use for determining the document type
    * of the input based on its path. The function represents the result
    * of merging the partial functions from all defined bundles and adding
    * a fallback (the `Ignored` document type) for all unhandled `Path` instances.
    */
  lazy val docTypeMatcher: Path => DocumentType = mergedBundle.docTypeMatcher.lift.andThen(_.getOrElse(Ignored))

  /** Provides all extensions for the text markup parser extracted from
    * all defined bundles.
    */
  lazy val markupExtensions: MarkupExtensions = mergedBundle.parsers.markupExtensions

  /** Provides the parser for configuration headers in text markup and template documents,
    * obtained from merging the parsers defined in all extension bundles and adding a fallback
    * that produces an empty configuration if all other parsers fail (or none are defined).
    */
  lazy val configHeaderParser: Path => Parser[Either[InvalidElement, Config]] =
    ConfigHeaderParser.merged(mergedBundle.parsers.configHeaderParsers :+ ConfigHeaderParser.fallback)

  /** Provides the parser for CSS documents, obtained by merging the parsers defined in all extension bundles
    * and adding a fallback that produces an empty style declaration set if all other parsers fail (or none are defined).
    */
  lazy val styleSheetParser: Parser[Set[StyleDeclaration]] =
    mergedBundle.parsers.styleSheetParser.getOrElse(success(Set.empty[StyleDeclaration]))

  /** Provides the parser for template documents, obtained by merging the parsers defined in all extension bundles
    * (or none if no bundle defines a parser). This method does not provide a fallback parser as the lack of any
    * defined parser indicates that templates are not supported for this operation. The parse operation should
    * therefore ignore all template documents in the input tree and use the default template from the merged theme
    * for the renderer for its output.
    */
  lazy val templateParser: Option[Parser[TemplateRoot]] = mergedBundle.parsers.templateParser

  /** The combined rewrite rule, obtained by merging the rewrite rules defined in all bundles.
    * This combined rule gets applied to the document between parse and render operations.
    */
  lazy val rewriteRule: DocumentCursor => RewriteRule =
    RewriteRules.chainFactories(mergedBundle.rewriteRules ++ RewriteRules.defaults)

  /** The combined rewrite rule for the specified document, obtained by merging the rewrite rules defined in all bundles.
    * This combined rule gets applied to the document between parse and render operations.
    */
  def rewriteRuleFor (doc: Document): RewriteRule = rewriteRule(DocumentCursor(doc))

  /** Provides the theme for the specified render format, obtained by merging all themes defined
    * for this format and adding the default theme for the format and a fallback theme.
    */
  def themeFor[W] (factory: RendererFactory[W]): factory.Theme = (mergedBundle.themes.collect {
    case t: factory.Theme => t
  } :+ factory.defaultTheme :+ factory.Theme()).reduceLeft(_ withBase _)

  /** Returns a new instance with the specified extension bundles added to the
    * bundles defined in this instance. The new bundles are treated with higher
    * precedence that the already defined bundles and may thus overwrite features.
    */
  def withBundles (bundles: Seq[ExtensionBundle]): OperationConfig = copy(bundles = this.bundles ++ bundles)

  /** Returns a new instance with the extension bundles provided by the specified markup
    * parser added to the bundles defined in this instance.
    */
  def withBundlesFor (factory: MarkupParser): OperationConfig = {
    val docTypeMatcher = new ExtensionBundle {
      override val docTypeMatcher: PartialFunction[Path, DocumentType] =
        DefaultDocumentTypeMatcher.forMarkup(factory.fileSuffixes)
      override val useInStrictMode: Boolean = true
    }
    copy(bundles = this.bundles ++ factory.extensions :+ docTypeMatcher)
  }

  /** Creates a new instance that is configured to interpret text markup as defined by its specification,
    * without any extensions. Technically this will exclude all bundles that do not have the `useInStrictMode`
    * flag set.
    */
  def forStrictMode: OperationConfig = copy(bundleFilter = bundleFilter.copy(strict = true))

  /**  Creates a new instance that is configured to allow raw content embedded in the host
    *  markup language.
    *
    *  These are disabled by default as Laika is designed to render to multiple
    *  output formats from a single input document. With raw content embedded
    *  the markup document is tied to a specific output format.
    *
    *  Technically it activates all bundle instances which have
    *  the `acceptRawContent` flag set to true.
    */
  def forRawContent: OperationConfig = copy(bundleFilter = bundleFilter.copy(acceptRawContent = true))

}

/** Represents the subset of OperationConfig relevant for renderers.
  */
trait RenderConfig {

  /** Specifies the minimum required level for a system message to get included into the output by a renderer.
    */
  def minMessageLevel: MessageLevel

  /** Indicates whether rendering should include any formatting (line breaks or indentation).
    */
  def renderFormatted: Boolean
}

/** Provides OperationConfig instances and a utility for merging bundles.
  */
object OperationConfig {

  /** Merges a sequence of bundles, including the invocation of their `processExtension` methods that allows
    * bundles to modify other bundles. The sequence is treated with decreasing precedence for features where
    * a bundle may overwrite other bundles.
    */
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

  /** A configuration instance with all the libraries default extension bundles.
    */
  val default: OperationConfig = OperationConfig(
    bundles = Seq(LaikaDefaults, DirectiveSupport, StandardDirectives)
  )

  /** An empty configuration instance.
    */
  val empty: OperationConfig = OperationConfig()

}

/** A filter that might deactivate or activate some of the bundles based on user configuration.
  *
  * @param strict indicates that text markup should be interpreted as defined by its specification, without any extensions
  * @param acceptRawContent indicates that the users accepts the inclusion of raw content in text markup
  */
case class BundleFilter (strict: Boolean = false, acceptRawContent: Boolean = false) extends (ExtensionBundle => Boolean) {
  override def apply (bundle: ExtensionBundle): Boolean =
    (!strict || bundle.useInStrictMode) && (acceptRawContent || !bundle.acceptRawContent)
}
