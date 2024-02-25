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

import cats.effect.{ Async, Resource }
import laika.api.bundle.ExtensionBundle
import laika.ast.OutputContext
import laika.io.descriptor.ThemeDescriptor
import laika.io.model.InputTree

/** Responsible for building a theme resource with the user-provided effect type and runtime configuration.
  *
  * Implementations of this trait can be passed to the `withTheme` method of the parser, renderer and transformer
  * APIs of the `laika-io` module.
  * Theme authors would usually offer a theme-specific configuration API with a final `build` method
  * that provides a `ThemeProvider` for the user.
  *
  * @author Jens Halm
  */
trait ThemeProvider { self =>

  /** Builds the theme resource with the user-provided effect type and runtime configuration.
    *
    * For convenience, implementations of this method usually utilize a [[laika.theme.ThemeBuilder]] to construct
    * `Theme` instances, but this is not mandatory.
    */
  def build[F[_]: Async]: Resource[F, Theme[F]]

  /** Creates a new theme using this instance as a base and the provided instance as the extension.
    *
    * The exact mechanics of extending a theme vary depending on the type of functionality supported by themes.
    * They are roughly as follows:
    *
    * - For functionality that is an accumulation of features, for example input files, parser extensions,
    *   renderer overrides or AST rewrite rules, the effect is accumulative,
    *   this theme and the extensions will be merged to a single set of features.
    *
    * - For functionality that is provided by unique instances, for example the template engine or the default template,
    *   the effect is replacement, where the instance in the extension replaces the corresponding instance in the base,
    *   if present.
    */
  def extendWith(extensions: ThemeProvider): ThemeProvider = new ThemeProvider {

    def build[F[_]: Async] = for {
      base <- self.build
      ext  <- extensions.build
    } yield {
      new Theme[F] {
        override def descriptor: ThemeDescriptor      = base.descriptor.extendWith(ext.descriptor)
        override def inputs: InputTree[F]             = base.inputs.overrideWith(ext.inputs)
        override def extensions: Seq[ExtensionBundle] = base.extensions ++ ext.extensions

        override def treeProcessor: OutputContext => Theme.TreeProcessor[F] = fmt =>
          base.treeProcessor(fmt).andThen(ext.treeProcessor(fmt))

      }
    }

  }

}
