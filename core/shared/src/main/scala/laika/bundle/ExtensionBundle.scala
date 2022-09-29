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

package laika.bundle

import laika.ast.RewriteRules.RewritePhaseBuilder
import laika.ast._
import laika.bundle.ExtensionBundle.PathTranslatorExtensionContext
import laika.config.{Config, ConfigBuilder}
import laika.parse.css.CSSParsers
import laika.rewrite.OutputContext
import laika.rewrite.link.SlugBuilder
import laika.rewrite.nav.PathTranslator

/** An extension bundle is a collection of parser extensions, rewrite rules, render overrides
  * and other features to be applied to parse, render and transform operations. It serves
  * as a central registry for all of Laika's extension and customization hooks.
  *
  * The base trait contains empty implementations for all these features, therefore any bundle
  * implementation only needs to override the relevant members.
  *
  * If the bundle implementation is not parameterized, the most convenient choice for users would
  * be to simply implement it as an object, like all built-in extensions do:
  *
  * {{{
  * object MyExtensions extends ExtensionBundle {
  *
  *   // override one or more members
  *
  * }
  * }}}
  *
  * This way a user can easily pass it to the operation builders:
  *
  * {{{
  * val transformer = Transformer
  *   .from(Markdown)
  *   .to(HTML)
  *   .using(MyExtensions)
  *   .build
  * }}}
  *
  * @author Jens Halm
  */
trait ExtensionBundle { self =>

  /** Short string describing the extension for tooling and logging.
    */
  def description: String
  
  /** Indicates whether the bundle is a built-in default provided by the library,
    * a collection of extensions installed by a markup format or user-defined.
    * 
    * This is relevant for determining the precedence of installed bundles when merging
    * them, as user-supplied functionality always overrides library defaults.
    */
  def origin: BundleOrigin = BundleOrigin.User

  /** Base configuration that serves as a fallback for
    * configuration files in the source directories
    * and/or config headers in markup and template documents.
    */
  def baseConfig: Config = Config.empty

  /** Specifies the function to use for determining the document type
    * of the input based on its path.
    *
    * Any path for which this function is not defined will be processed by the remaining
    * defined bundles. The documents for paths for which none of the extensions provides
    * a `DocumentType` will be treated as static files to be copied over to the target
    * directory in transformations by default.
    */
  def docTypeMatcher: PartialFunction[Path, DocumentType] = PartialFunction.empty

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
  def slugBuilder: Option[String => String] = None

  /** Specifies extensions and/or replacements for parsers that deal with
    * text markup, templates, CSS or configuration headers.
    */
  def parsers: ParserBundle = ParserBundle()

  /** Specifies rewrite rules to be applied to the document tree model between the
    * parse and render operations.
    *
    * The specified functions will be invoked for each document, allowing to capture
    * information from the entire document tree before returning the actual rule,
    * which is a partial function from `Element` to `Option[Element]` that allows
    * to remove or replace elements from the tree.
    */
  def rewriteRules: RewritePhaseBuilder = PartialFunction.empty

  /** The overrides for renderers defined by this bundle.
    *
    * An override is always specific to a particular output format like HTML or PDF.
    * A bundle can contain multiple overrides for the same output format which will be merged before use.
    */
  def renderOverrides: Seq[RenderOverrides] = Seq.empty

  /** Extends the built-in path translator with additional functionality.
    * 
    * The internal path translator deals with aspects like applying the suffix for the output format
    * or modifying the path for versioned documents and more.
    * 
    * The `PathTranslatorExtensionContext` provides access to this internal path translator, to the output
    * format it is going to be used for and the complete user configuration.
    * 
    * In most cases, extensions can simply be created by using either `PathTranslator.preTranslate`
    * or `PathTranslator.postTranslate` to apply additional translation steps either before or after 
    * applying the internal translator.
    * 
    * Alternatively a completely custom implementation of the `PathTranslator` trait can be provided,
    * but this will usually not be necessary.
    * 
    * `PathTranslator` implementations usually do not deal with the fragment part of the path.
    * Use the `slugBuilder` extension point for this purpose.
    */
  def extendPathTranslator: PartialFunction[PathTranslatorExtensionContext, PathTranslator] = PartialFunction.empty
  
  /** Internal API usually only called by other extension bundles.
    *
    * In some cases a bundle might be an extension of another bundle and needs the opportunity
    * to process and modify that bundle without requiring a direct reference to it. An example
    * is a registry for directives which needs to pass all its registered directives to the
    * bundle which deals with finally creating all the directive parsers.
    *
    * The partial function should match only on the types of bundles it intends to process
    * and is then allowed to return a new, modified instance of that bundle.
    */
  def processExtension: PartialFunction[ExtensionBundle, ExtensionBundle] = PartialFunction.empty

  /** Returns a new extension bundle by merging the content of this bundle with the
    * content of the base bundle.
    *
    * The other bundle is treated as the base of this bundle, which means that:
    *
    * - in case of optional features a feature defined in this bundle will overwrite a feature defined in the base
    *
    * - in case of features applied in sequence, the features in this bundle will be applied before the features
    *   in the base bundle
    *
    * - in case of feature collections, the features of this bundle will be merged with those of the base bundle
    */
  def withBase (base: ExtensionBundle): ExtensionBundle = new ExtensionBundle {

    val description: String = "Merged Bundle"

    override lazy val origin: BundleOrigin = if (self.origin == base.origin) self.origin else BundleOrigin.Mixed
    
    override lazy val baseConfig = self.baseConfig.withFallback(base.baseConfig)

    override lazy val docTypeMatcher = self.docTypeMatcher.orElse(base.docTypeMatcher)
    
    override lazy val slugBuilder = self.slugBuilder.orElse(base.slugBuilder)

    override lazy val parsers: ParserBundle = self.parsers withBase base.parsers

    /* flipped on purpose, base rules need to be applied first, so that app rules do not need to deal with potentially
       unknown node types */
    override lazy val rewriteRules = {
      case phase => base.rewriteRules.lift(phase).getOrElse(Nil) ++ self.rewriteRules.lift(phase).getOrElse(Nil)
    }

    override lazy val renderOverrides = self.renderOverrides ++ base.renderOverrides

    override def extendPathTranslator: PartialFunction[PathTranslatorExtensionContext, PathTranslator] =
      new PartialFunction[PathTranslatorExtensionContext, PathTranslator] {

        def isDefinedAt (ctx: PathTranslatorExtensionContext): Boolean =
          self.extendPathTranslator.isDefinedAt(ctx) || base.extendPathTranslator.isDefinedAt(ctx)

        def apply (ctx: PathTranslatorExtensionContext): PathTranslator = {
          val newPathTranslator = base.extendPathTranslator
            .applyOrElse[PathTranslatorExtensionContext, PathTranslator](ctx, _.baseTranslator)
          val newCtx = new PathTranslatorExtensionContext(newPathTranslator, ctx.outputContext, ctx.config)
          self.extendPathTranslator
            .applyOrElse[PathTranslatorExtensionContext, PathTranslator](newCtx, _.baseTranslator)
        }
      }

    override def processExtension: PartialFunction[ExtensionBundle, ExtensionBundle] =
      self.processExtension.orElse(base.processExtension)

    override def forStrictMode = merged(self.forStrictMode, base.forStrictMode)
    override def rawContentDisabled = merged(self.rawContentDisabled, base.rawContentDisabled)
    
    private def merged (thisBundle: Option[ExtensionBundle], baseBundle: Option[ExtensionBundle]): Option[ExtensionBundle] =
      Seq(thisBundle, baseBundle).flatten.reduceOption((a,b) => a.withBase(b))
  }

  /** Provides a version of this bundle that can be used in strict mode or `None` if the entire bundle
    * should be removed in strict mode.
    * 
    * When strict mode does not affect a bundle it can return `Some(this)`.
    *
    * Any bundle to be used in strict mode should be free from any parser extensions that 
    * adds features to markup syntax beyond their respective specifications.
    */
  def forStrictMode: Option[ExtensionBundle] = Some(this)

  /** Provides a version of this bundle that can be used in the default run mode where raw content in markup
    * documents (such as embedded HTML) is disabled.
    *
    * When a bundle does not add parsers for raw content it can return `Some(this)`.
    *
    * Any bundle to be used in the default run mode should be free from any parser extensions that 
    * allow raw content in markup.
    * When the user switches the `acceptRawContent` flag to `true` then this method will not be invoked
    * and the initial instance of the bundle is used.
    */
  def rawContentDisabled: Option[ExtensionBundle] = Some(this)

}

/** Provides default ExtensionBundle instances.
  */
object ExtensionBundle {

  /** The context that is provided to builders for path translator extensions.
    * 
    * @param baseTranslator the internal path translator that can be used for delegating most translation steps to
    * @param outputContext  the context for the output format the translator is used for 
    *                       (since translators are different per render format)
    * @param config         the complete user configuration for the current transformation
    */
  class PathTranslatorExtensionContext (val baseTranslator: PathTranslator, val outputContext: OutputContext, val config: Config)

  /** An empty bundle */
  object Empty extends ExtensionBundle {
    val description: String = "Empty extension bundle"
    override val origin: BundleOrigin = BundleOrigin.Library
  }

  /** Bundle containing Laika defaults which is included automatically
    * in all operations.
    */
  object LaikaDefaults extends ExtensionBundle {

    val description: String = "Laika's Default Extensions"

    override val origin: BundleOrigin = BundleOrigin.Library
    
    override val docTypeMatcher: PartialFunction[Path, DocumentType] = DocumentTypeMatcher.base
    
    override val slugBuilder: Option[String => String] = Some(SlugBuilder.default)
    
    override val baseConfig: Config = ConfigBuilder.empty.withValue("laika.version", "0.19.0").build

    override val parsers: ParserBundle = ParserBundle(
      styleSheetParser = Some(CSSParsers.styleDeclarationSet)
    )

  }

}
