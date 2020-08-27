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

/**
  * @author Jens Halm
  */
class ThemeBuilder[F[_]: Monad] private[laika] (themeName: String,
                                                inputs: F[InputTreeBuilder[F]],
                                                extensions: Seq[ExtensionBundle],
                                                bundleBuilder: BundleBuilder,
                                                treeProcessors: Seq[Format => TreeProcessor[F]]) { self =>

  private val noOp: TreeProcessor[F] = Kleisli.ask[F, ParsedTree[F]]
  
  def addInputs (inputs: InputTreeBuilder[F]): ThemeBuilder[F] = 
    new ThemeBuilder(themeName, Monad[F].pure(inputs), extensions, bundleBuilder, treeProcessors)
  
  def addInputs (inputs: F[InputTreeBuilder[F]]): ThemeBuilder[F] = 
    new ThemeBuilder(themeName, inputs, extensions, bundleBuilder, treeProcessors)
  
  def addExtensions (bundles: ExtensionBundle*): ThemeBuilder[F] = 
    new ThemeBuilder(themeName, inputs, extensions ++ bundles, bundleBuilder, treeProcessors)
  
  def addBaseConfig (config: Config): ThemeBuilder[F] =
    new ThemeBuilder(themeName, inputs, extensions, bundleBuilder.addConfig(config), treeProcessors)

  def addRenderOverrides (overrides: RenderOverrides): ThemeBuilder[F] =
    new ThemeBuilder(themeName, inputs, extensions, bundleBuilder.addRenderOverrides(overrides), treeProcessors)

  def addRewriteRules (rules: DocumentCursor => RewriteRules): ThemeBuilder[F] =
    new ThemeBuilder(themeName, inputs, extensions, bundleBuilder.addRewriteRules(rules), treeProcessors)

  def addRewriteRules (rules: RewriteRules): ThemeBuilder[F] =
    new ThemeBuilder(themeName, inputs, extensions, bundleBuilder.addRewriteRules(_ => rules), treeProcessors)
  
  def processTree (f: Format => TreeProcessor[F]): ThemeBuilder[F] =
    new ThemeBuilder[F](themeName, inputs, extensions, bundleBuilder, treeProcessors :+ f)

  def processTree (f: TreeProcessor[F], format: Format): ThemeBuilder[F] =
    new ThemeBuilder[F](themeName, inputs, extensions, bundleBuilder, treeProcessors :+ { fmt: Format => 
      if (fmt == format) f else noOp
    })

  def build: Resource[F, Theme[F]] = Resource.liftF(inputs.flatMap(_.build.map(in => new Theme[F] {
    def inputs: InputTree[F] = in
    def extensions: Seq[ExtensionBundle] = self.extensions ++ bundleBuilder.build.toSeq
    def treeProcessor: Format => TreeProcessor[F] = { format =>
      treeProcessors.map(_(format)).reduceLeftOption(_ andThen _).getOrElse(noOp)
    }
  })))
}

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
  
  def apply[F[_] : Sync] (themeName: String): ThemeBuilder[F] = 
    new ThemeBuilder[F](themeName, Sync[F].pure(InputTree[F]), Nil, BundleBuilder(themeName), Nil)

}
