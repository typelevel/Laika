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

package laika.api.builder

import laika.ast.RewriteRules.RewriteRulesBuilder
import laika.config.{ Config, ConfigBuilder, ConfigEncoder, DefaultKey, Key, ValidationError }
import laika.ast._
import laika.bundle.ExtensionBundle.PathTranslatorExtensionContext
import laika.bundle.{
  BundleOrigin,
  ConfigProvider,
  DocumentTypeMatcher,
  ExtensionBundle,
  MarkupExtensions
}
import laika.config.Config.ConfigResult
import laika.directive.DirectiveSupport
import laika.directive.std.StandardDirectives
import laika.factory.{ MarkupFormat, RenderFormat }
import laika.parse.Parser
import laika.parse.combinator.Parsers
import laika.rewrite.OutputContext
import laika.rewrite.RecursiveResolverRules
import laika.rewrite.link.SlugBuilder
import laika.rewrite.nav.PathTranslator

import scala.annotation.tailrec

/** Represents the configuration for an operation based on extension bundles and several flags.
  *
  * Provides convenient access to the result of merging the defined bundles and adding fallback
  * options wherever necessary.
  *
  * @param bundles all extension bundles defined by this operation
  * @param bundleFilter a filter that might deactivate some of the bundles based on user configuration
  * @param configBuilder a builder for assembling values for the base configuration as
  * @param failOnMessages the filter to apply to runtime messages that should cause the transformation to fail
  * @param renderMessages the filter to apply to runtime messages that should be rendered in the output
  * @param renderFormatted indicates whether rendering should include any formatting (line breaks or indentation)
  */
case class OperationConfig(
    bundles: Seq[ExtensionBundle] = Nil,
    bundleFilter: BundleFilter = BundleFilter(),
    configBuilder: ConfigBuilder = ConfigBuilder.empty,
    failOnMessages: MessageFilter = MessageFilter.Error,
    renderMessages: MessageFilter = MessageFilter.None,
    renderFormatted: Boolean = true
) extends RenderConfig {

  private lazy val mergedBundle: ExtensionBundle =
    OperationConfig.mergeBundles(bundleFilter(bundles))

  /** Base configuration merged from all defined extension bundles
    * that serves as a fallback for configuration files in the source
    * directories and/or config headers in markup and template documents.
    */
  lazy val baseConfig: Config = configBuilder.build(mergedBundle.baseConfig)

  /** Returns a new instance with the specified configuration value added.
    *
    * The specified value with have higher precedence than any value with the same key registered by extension bundles,
    * but lower precedence than any value with the same key specified in a configuration file for a directory
    * or a configuration header in a markup document.
    */
  def withConfigValue[T: ConfigEncoder: DefaultKey](value: T): OperationConfig =
    copy(configBuilder = configBuilder.withValue(value))

  /** Returns a new instance with the specified configuration value added.
    *
    * The specified value with have higher precedence than any value with the same key registered by extension bundles,
    * but lower precedence than any value with the same key specified in a configuration file for a directory
    * or a configuration header in a markup document.
    */
  def withConfigValue[T](key: String, value: T)(implicit
      encoder: ConfigEncoder[T]
  ): OperationConfig =
    copy(configBuilder = configBuilder.withValue(key, value))

  /** Returns a new instance with the specified configuration value added.
    *
    * The specified value with have higher precedence than any value with the same key registered by extension bundles,
    * but lower precedence than any value with the same key specified in a configuration file for a directory
    * or a configuration header in a markup document.
    */
  def withConfigValue[T](key: Key, value: T)(implicit encoder: ConfigEncoder[T]): OperationConfig =
    copy(configBuilder = configBuilder.withValue(key, value))

  /** Applies the filter based on the `strict` and `withRawContent` flags set by the user
    * to the bundles configured for this instance.
    */
  lazy val filteredBundles: Seq[ExtensionBundle] = bundleFilter(bundles)

  /** Provides all extensions for the text markup parser extracted from
    * all defined bundles.
    */
  lazy val markupExtensions: MarkupExtensions = mergedBundle.parsers.markupExtensions

  /** Provides the parser for configuration documents and configuration headers in text markup
    * and template documents.
    * Always produces an empty configuration if no provider was installed.
    */
  lazy val configProvider: ConfigProvider = {
    val default = mergedBundle.parsers.configProvider.getOrElse(ConfigProvider.empty)
    if (bundleFilter.strict) default.forStrictMode else default
  }

  /** Provides the parser for CSS documents, obtained by merging the parsers defined in all extension bundles
    * and adding a fallback that produces an empty style declaration set if all other parsers fail (or none are defined).
    */
  lazy val styleSheetParser: Parser[Set[StyleDeclaration]] =
    mergedBundle.parsers.styleSheetParser.getOrElse(Parsers.success(Set.empty[StyleDeclaration]))

  /** Provides the parser for template documents, obtained by merging the parsers defined in all extension bundles
    * (or none if no bundle defines a parser). This method does not provide a fallback parser as the lack of any
    * defined parser indicates that templates are not supported for this operation. The parse operation should
    * therefore ignore all template documents in the input tree and use a fallback template.
    */
  lazy val templateParser: Option[Parser[TemplateRoot]] = mergedBundle.parsers.templateParser

  /** Specifies the function to use for determining the document type
    * of the input based on its path. The function represents the result
    * of merging the partial functions from all defined bundles and adding
    * a fallback (the `Ignored` document type) for all unhandled `Path` instances.
    */
  lazy val docTypeMatcher: Path => DocumentType =
    mergedBundle.docTypeMatcher.lift.andThen(_.getOrElse(DocumentType.Ignored))

  /** Function that receives the text of a headline, the name of a document
    * or directory or a manually assigned identifier, and builds a slug from it
    * that becomes part of the final URL or identifier (depending on output format).
    *
    * The result of the function must be:
    *
    * - a valid identifier in HTML and XML
    * - a valid path segment in a URL
    * - a valid file name
    */
  lazy val slugBuilder: String => String = mergedBundle.slugBuilder.getOrElse(SlugBuilder.default)

  /** The path translator for translating all virtual paths before rendering, obtained by combining
    * the path translators defined in all bundles (if present) with the internal base translator.
    *
    * The internal path translator deals with aspects like applying the suffix for the output format
    * or modifying the path for versioned documents and more.
    *
    * Theme or user extensions may wrap additional functionality around that internal translator,
    * or in rare cases even replace it with an entirely custom implementation.
    *
    * A new instance of `PathTranslator` will be created for each render operation.
    * The output format is described in the `outputContext` parameter.
    */
  def pathTranslatorFor(
      root: DocumentTreeRoot,
      outputContext: OutputContext
  ): ConfigResult[PathTranslator] = for {
    cursor         <- RootCursor(root, Some(outputContext))
    baseTranslator <- cursor.pathTranslator.toRight(
      ValidationError("Internal error: path translator should not be empty")
    )
  } yield {
    val context = new PathTranslatorExtensionContext(baseTranslator, outputContext, cursor.config)
    mergedBundle.extendPathTranslator.applyOrElse[PathTranslatorExtensionContext, PathTranslator](
      context,
      _.baseTranslator
    )
  }

  /** The combined rewrite rule, obtained by merging the rewrite rules defined in all bundles.
    * This combined rule gets applied to the document between parse and render operations.
    */
  def rewriteRulesFor(root: DocumentTreeRoot, phase: RewritePhase): RewriteRulesBuilder = {
    val baseRules = RewriteRules.chainFactories(
      mergedBundle.rewriteRules.lift(phase).getOrElse(Nil) ++
        RewriteRules.defaultsFor(root, phase, slugBuilder)
    )
    cursor => baseRules(cursor).map(RecursiveResolverRules.applyTo(cursor, _, phase))
  }

  /** The combined rewrite rule for the specified document, obtained by merging the rewrite rules defined in all bundles.
    * This combined rule gets applied to the document between parse and render operations.
    */
  def rewriteRulesFor(doc: Document, phase: RewritePhase): ConfigResult[RewriteRules] =
    DocumentCursor(doc).flatMap { cursor =>
      rewriteRulesFor(cursor.root.target, phase).apply(cursor)
    }

  /** Provides the overrides for the specified render format.
    */
  def renderOverridesFor[FMT](format: RenderFormat[FMT]): format.Overrides =
    (mergedBundle.renderOverrides.collect { case t: format.Overrides =>
      t
    } :+ format.Overrides()).reduceLeft(_ withBase _)

  /** Returns a new instance with the specified extension bundles added to the
    * bundles defined in this instance. The new bundles are treated with higher
    * precedence that the already defined bundles and may thus overwrite features.
    */
  def withBundles(bundles: Seq[ExtensionBundle]): OperationConfig =
    copy(bundles = this.bundles ++ bundles)

  /** Returns a new instance with the extension bundles provided by the specified markup
    * parser added to the bundles defined in this instance.
    */
  def withBundlesFor(parser: MarkupFormat): OperationConfig = {
    val docTypeMatcher = new ExtensionBundle {
      val description: String           = s"Document Type Matcher for ${parser.description}"
      override val origin: BundleOrigin = BundleOrigin.Parser
      override val docTypeMatcher: PartialFunction[Path, DocumentType] =
        DocumentTypeMatcher.forMarkup(parser.fileSuffixes)
    }
    copy(bundles = this.bundles ++ parser.extensions :+ docTypeMatcher)
  }

  /** Creates a new instance that is configured to interpret text markup as defined by its specification,
    * without any extensions.
    */
  def forStrictMode: OperationConfig = copy(bundleFilter = bundleFilter.copy(strict = true))

  /**  Creates a new instance that is configured to allow raw content embedded in the host markup language.
    *
    *  These are disabled by default as Laika is designed to render to multiple output formats
    *  from a single input document.
    *  With raw content embedded the markup document is tied to a specific output format.
    */
  def forRawContent: OperationConfig =
    copy(bundleFilter = bundleFilter.copy(acceptRawContent = true))

  /**  Merges the extension bundles and setting of this configuration with the specified other configuration.
    *  Alternative settings defined in this instance will have precedence.
    */
  def merge(other: OperationConfig): OperationConfig = copy(
    bundles = OperationConfig.sortBundles(this.bundles ++ other.bundles),
    bundleFilter = this.bundleFilter.merge(other.bundleFilter)
  )

}

/** Represents the subset of OperationConfig relevant for renderers.
  */
trait RenderConfig {

  /** The filter to apply to runtime messages that should get rendered to the output.
    */
  def renderMessages: MessageFilter

  /** Indicates whether rendering should include any formatting (line breaks or indentation).
    */
  def renderFormatted: Boolean
}

/** A filter that might deactivate or activate some of the bundles based on user configuration.
  *
  * @param strict indicates that text markup should be interpreted as defined by its specification, without any extensions
  * @param acceptRawContent indicates that the users accepts the inclusion of raw content in text markup
  */
case class BundleFilter(strict: Boolean = false, acceptRawContent: Boolean = false) {

  def apply(bundles: Seq[ExtensionBundle]): Seq[ExtensionBundle] = {
    val strictApplied = if (!strict) bundles else bundles.flatMap(_.forStrictMode)
    if (acceptRawContent) strictApplied else strictApplied.flatMap(_.rawContentDisabled)
  }

  def merge(other: BundleFilter): BundleFilter =
    BundleFilter(strict || other.strict, acceptRawContent || other.acceptRawContent)

}

/** Provides OperationConfig instances and a utility for merging bundles.
  */
object OperationConfig {

  private val originOrder: List[BundleOrigin] = List(
    BundleOrigin.Library,
    BundleOrigin.Parser,
    BundleOrigin.Theme,
    BundleOrigin.Mixed,
    BundleOrigin.User
  )

  def sortBundles(bundles: Seq[ExtensionBundle]): Seq[ExtensionBundle] =
    bundles.distinct.sortBy(b => originOrder.indexOf(b.origin))

  /** Merges a sequence of bundles, including the invocation of their `processExtension` methods that allows
    * bundles to modify other bundles. The sequence is treated with decreasing precedence for features where
    * a bundle may overwrite other bundles.
    */
  def mergeBundles(bundles: Seq[ExtensionBundle]): ExtensionBundle = {

    @tailrec
    def processBundles(
        past: Seq[ExtensionBundle],
        pending: Seq[ExtensionBundle]
    ): Seq[ExtensionBundle] = pending match {
      case Nil          => past
      case next :: rest =>
        val newPast    = past.map(ex => next.processExtension.lift(ex).getOrElse(ex)) :+ next
        val newPending = rest.map(ex => next.processExtension.lift(ex).getOrElse(ex))
        processBundles(newPast, newPending)
    }

    processBundles(Nil, sortBundles(bundles)).reverse.reduceLeftOption(_ withBase _).getOrElse(
      ExtensionBundle.Empty
    )
  }

  /** A configuration instance with all the libraries default extension bundles.
    */
  val default: OperationConfig = OperationConfig(
    bundles = Seq(ExtensionBundle.LaikaDefaults, DirectiveSupport, StandardDirectives)
  )

  /** An empty configuration instance.
    */
  val empty: OperationConfig = OperationConfig()

}
