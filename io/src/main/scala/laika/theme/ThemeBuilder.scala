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

package laika.theme

import cats.implicits._
import cats.Monad
import cats.data.Kleisli
import cats.effect.{Resource, Sync}
import laika.ast.{DocumentCursor, RewriteRules}
import laika.bundle.{BundleOrigin, ExtensionBundle, RenderOverrides}
import laika.config.Config
import laika.factory.Format
import laika.io.model.{InputTree, InputTreeBuilder, ParsedTree}
import laika.theme.Theme.TreeProcessor
import laika.theme.ThemeBuilder.BundleBuilder

/** Builder API for constructing `Theme` instances, providing several shortcuts for defining the contents
  * of a theme. 
  * A theme can consist of inputs like templates, CSS or JavaScript files and optionally of one or more
  * extension bundles for customizing Laika with features like directives or parser extensions.
  * 
  * The API is entirely optional and can be used to avoid boilerplate when implementing a [[laika.theme.ThemeProvider]].
  * 
  * @author Jens Halm
  */
class ThemeBuilder[F[_]: Monad] private[laika] (themeName: String,
                                                inputs: F[InputTreeBuilder[F]],
                                                extensions: Seq[ExtensionBundle],
                                                bundleBuilder: BundleBuilder,
                                                treeProcessors: Seq[Format => TreeProcessor[F]]) { self =>

  private val noOp: TreeProcessor[F] = Kleisli.ask[F, ParsedTree[F]]

  /** Adds the specified inputs to the theme.
    * When the method is invoked multiple times the respective input trees will be merged.
    */
  def addInputs (inputs: InputTreeBuilder[F]): ThemeBuilder[F] = addInputs(Monad[F].pure(inputs))

  /** Adds the specified inputs, suspended in the effect `F`, to the theme.
    * When the method is invoked multiple times the respective input trees will be merged.
    */
  def addInputs (inputBuilders: F[InputTreeBuilder[F]]): ThemeBuilder[F] = {
    val mergedInputs = for { i1 <- inputs; i2 <- inputBuilders } yield i1.merge(i2)
    new ThemeBuilder(themeName, mergedInputs, extensions, bundleBuilder, treeProcessors)
  }

  /** Adds one or more extension bundles to the theme.
    * For the most common bundle features like pre-populated configuration, rewrite rules or render overrides,
    * the respective shortcuts of this class can be used to avoid the boilerplate of extending `ExtensionBundle`
    * yourself.
    * The shortcuts are `addBaseConfig`, `addRewriteRules` and `addRenderOverrides`.
    */
  def addExtensions (bundles: ExtensionBundle*): ThemeBuilder[F] = 
    new ThemeBuilder(themeName, inputs, extensions ++ bundles, bundleBuilder, treeProcessors)

  /** Add pre-populated configuration to the theme that will be merged with the user-supplied configuration.
    * User configuration has always higher precedence than theme configuration.
    * It is recommended to put all theme-specific configuration into a namespace to avoid clashes.
    * 
    * Most common use case for pre-populating configuration is to provide AST nodes and strings to be
    * used in the theme templates.
    */
  def addBaseConfig (config: Config): ThemeBuilder[F] =
    new ThemeBuilder(themeName, inputs, extensions, bundleBuilder.addConfig(config), treeProcessors)

  /** Adds overrides for the renderer which can be used for theme-specific UI elements.
    * One example is to provide custom icons for some of Laika's decorated blocks, like those produced
    * by the `@:callout` directive.
    */
  def addRenderOverrides (overrides: RenderOverrides): ThemeBuilder[F] =
    new ThemeBuilder(themeName, inputs, extensions, bundleBuilder.addRenderOverrides(overrides), treeProcessors)

  /** Adds a custom rewrite rule that can swap or remove individual nodes from the document AST.
    * In contrast to the `processTree` hook which looks at the entire tree of documents,
    * a rewrite rule looks at the individual AST nodes within a document.
    * 
    * This is an overload that allows to construct the rules based on looking at the corresponding
    * `DocumentCursor` first.
    */
  def addRewriteRules (rules: DocumentCursor => RewriteRules): ThemeBuilder[F] =
    new ThemeBuilder(themeName, inputs, extensions, bundleBuilder.addRewriteRules(rules), treeProcessors)

  /** Adds a custom rewrite rule that can swap or remove individual nodes from the document AST.
    * In contrast to the `processTree` hook which looks at the entire tree of documents,
    * a rewrite rule looks at the individual AST nodes within a document.
    */
  def addRewriteRules (rules: RewriteRules): ThemeBuilder[F] =
    new ThemeBuilder(themeName, inputs, extensions, bundleBuilder.addRewriteRules(_ => rules), treeProcessors)

  /** Adds a function that processes the document tree between parsing and rendering.
    * In contrast to the `addRewriteRule` hook which looks at AST nodes within a document,
    * this function can look at the entire tree and add, swap or remove documents for example.
    * 
    * The [[laika.theme.TreeProcessorBuilder]] provides several shortcuts for constructing
    * a `TreeProcessor` (which is just a type alias for a plain `Kleisli`).
    */
  def processTree (f: Format => TreeProcessor[F]): ThemeBuilder[F] =
    new ThemeBuilder[F](themeName, inputs, extensions, bundleBuilder, treeProcessors :+ f)

  /** Adds a function that processes the document tree between parsing and rendering,
    * to be executed only for the specified output format.
    * 
    * In contrast to the `addRewriteRule` hook which looks at AST nodes within a document,
    * this function can look at the entire tree and add, swap or remove documents for example.
    *
    * The [[laika.theme.TreeProcessorBuilder]] provides several shortcuts for constructing
    * a `TreeProcessor` (which is just a type alias for a plain `Kleisli`).
    */
  def processTree (f: TreeProcessor[F], format: Format): ThemeBuilder[F] =
    new ThemeBuilder[F](themeName, inputs, extensions, bundleBuilder, treeProcessors :+ { fmt: Format => 
      if (fmt == format) f else noOp
    })

  /** Builds a theme resource based on the elements passed to this builder instance.
    */
  def build: Resource[F, Theme[F]] = Resource.liftF(inputs.flatMap(_.build.map(in => new Theme[F] {
    def inputs: InputTree[F] = in
    def extensions: Seq[ExtensionBundle] = self.extensions ++ bundleBuilder.build.toSeq
    def treeProcessor: Format => TreeProcessor[F] = { format =>
      treeProcessors.map(_(format)).reduceLeftOption(_ andThen _).getOrElse(noOp)
    }
  })))
}

/** ThemeBuilder companion that acts as an entry point for its API.
  */
object ThemeBuilder {

  private[theme] case class BundleBuilder (themeName: String,
                                           baseConfig: Config = Config.empty,
                                           renderOverrides: Seq[RenderOverrides] = Nil,
                                           rewriteRules: Seq[DocumentCursor => RewriteRules] = Nil) { self =>
    def addConfig (config: Config): BundleBuilder = copy(baseConfig = config.withFallback(baseConfig))
    def addRenderOverrides (overrides: RenderOverrides): BundleBuilder =
      copy(renderOverrides = renderOverrides :+ overrides)
    def addRewriteRules (rules: DocumentCursor => RewriteRules): BundleBuilder =
      copy(rewriteRules = rewriteRules :+ rules)
    def build: Option[ExtensionBundle] = 
      if (baseConfig == Config.empty && renderOverrides.isEmpty && rewriteRules.isEmpty) None else Some(new ExtensionBundle {
        val description = s"Extensions for theme '$themeName'"
        override def origin = BundleOrigin.Theme
        override def baseConfig = self.baseConfig
        override def renderOverrides = self.renderOverrides
        override def rewriteRules = self.rewriteRules
      })
  }

  /** Creates a new, empty theme builder instance for the specified name.
    * The theme name is used in logging or the data returned by the `describe` method of the parser,
    * renderer and transformer APIs.
    */
  def apply[F[_] : Sync] (themeName: String): ThemeBuilder[F] = 
    new ThemeBuilder[F](themeName, Sync[F].pure(InputTree[F]), Nil, BundleBuilder(themeName), Nil)

}
