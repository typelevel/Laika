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

import laika.config.{Config, ConfigBuilder}
import laika.ast._
import laika.parse.css.CSSParsers

/** An extension bundle is a collection of parser extensions, rewrite rules, render themes
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

  /** Short string describing the output format for tooling and logging
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
  def rewriteRules: Seq[DocumentCursor => RewriteRules] = Seq.empty

  /** The themes defined by this bundle, which are a collection of templates, styles
    * and custom render functions.
    *
    * A theme is always specific to a particular output format like HTML or PDF.
    * A bundle can contain multiple themes for the same output format which will be
    * merged before use.
    */
  def themes: Seq[RenderTheme] = Seq.empty

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

    override lazy val origin: BundleOrigin = if (this.origin == base.origin) origin else BundleOrigin.Mixed
    
    override val useInStrictMode = self.useInStrictMode && base.useInStrictMode

    override lazy val baseConfig = self.baseConfig.withFallback(base.baseConfig)

    override lazy val docTypeMatcher = self.docTypeMatcher.orElse(base.docTypeMatcher)

    override lazy val parsers: ParserBundle = self.parsers withBase base.parsers

    /* flipped on purpose, base rules need to be applied first, so that app rules do not need to deal with potentially
       unknown node types */
    override lazy val rewriteRules = base.rewriteRules ++ self.rewriteRules

    override lazy val themes = self.themes ++ base.themes

    override def processExtension: PartialFunction[ExtensionBundle, ExtensionBundle] =
      self.processExtension.orElse(base.processExtension)
  }

  /** Indicates that this bundle should still be used if the user runs a transformation in strict mode.
    *
    * This setting is appropriate if a bundle contains features which are native elements of a
    * text markup language as defined in its specification, but implemented as an extension for technical reasons.
    */
  def useInStrictMode: Boolean = false

  /** Indicates that this bundle deals with raw content embedded in text markup, like HTML.
    *
    * These kind of bundles are disabled by default as Laika is designed to render to multiple
    * output formats from a single input document. With raw content embedded the markup document
    * is tied to a specific output format.
    *
    * Bundles which have this flag set to true need to be enabled explicitly by the user by calling
    * `withRawContent` on the `Parse` or `Transform` API:
    *
    * {{{
    *   val transformer = Transformer.from(Markdown).to(HTML).withRawContent.build
    * }}}
    */
  def acceptRawContent: Boolean = false

}

/** Provides default ExtensionBundle instances.
  */
object ExtensionBundle {

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
    
    override val useInStrictMode = true

    override val docTypeMatcher: PartialFunction[Path, DocumentType] = DocumentTypeMatcher.base

    override val parsers: ParserBundle = ParserBundle(
      styleSheetParser = Some(CSSParsers.styleDeclarationSet)
    )

  }

}
