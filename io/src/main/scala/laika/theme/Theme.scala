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

import cats.data.Kleisli
import cats.effect.Async
import laika.bundle.ExtensionBundle
import laika.api.format.Format
import laika.io.descriptor.ThemeDescriptor
import laika.io.model.{ InputTree, ParsedTree }
import laika.theme.Theme.TreeProcessor

/** A theme is a way of pre-populating the input tree with a set of templates, styles and configurations
  * to achieve a particular look & feel without the need for the user to craft their own templates,
  * CSS or JavaScript.
  *
  * Themes also allow the registration of extension bundles in case it offers custom directives, parser extensions
  * or other customizations on top of its templates and styles.
  *
  * Implementations of themes must provide an instance of the [[laika.theme.ThemeProvider]] trait to produce
  * instances of this trait.
  * The indirection is necessary as the concrete implementation of the `F[_]` effect and the thread pools for
  * execution are meant to be chosen by the user.
  *
  * Themes would also most likely come with a custom configuration API for tweaking the style and functionality
  * of the theme.
  * This is expected to be a type-safe Scala API as Laika avoids any kind of file-based, stringly configuration
  * for global settings and only uses HOCON for local configuration (per-directory, per-document, per-directive).
  *
  * @author Jens Halm
  */
trait Theme[F[_]] {

  /** A descriptor for this theme and its extensions for tooling or logging.
    */
  def descriptor: ThemeDescriptor

  /** The inputs to pre-populate when using parsers or transformers with this theme.
    *
    * For convenience, theme initialization code usually uses the [[laika.io.model.InputTreeBuilder]] API
    * for assembling the inputs.
    * The builder allows to collect inputs from the resource folder of the jar, from in-memory strings or streams
    * or from document AST constructed on-the-fly, completely by-passing the parsing step.
    *
    * It is not recommended to use the file-based options of the builder for themes,
    * as this would limit the flexibility of the end-user.
    */
  def inputs: InputTree[F]

  /** Installs one or more extension bundles into all parsers, renderers and transformers using this theme.
    *
    * The extensions should be essential for the theme to function.
    * If their features are rather opt-in in nature, it is best to offer the bundles separately and let the user
    * choose if and where to use them.
    */
  def extensions: Seq[ExtensionBundle]

  /** Hook for transforming the document AST between parsing and rendering.
    *
    * The provided function accepts a `Format` instance as parameter which can be used to provide
    * different processor per output format.
    */
  def treeProcessor: Format => TreeProcessor[F]

}

/** Theme companion for constructing empty theme instances.
  */
object Theme {

  /** Type alias for a tree processor Kleisli for convenience.
    */
  type TreeProcessor[F[_]] = Kleisli[F, ParsedTree[F], ParsedTree[F]]

  /** An empty theme that can be installed in place of the default Helium theme
    * if you want to control the look & feel by simply placing all necessary templates and styles
    * right into the input directories.
    */
  def empty: ThemeProvider = new ThemeProvider {
    def build[F[_]: Async] = ThemeBuilder("Empty Theme").build
  }

}
