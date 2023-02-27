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

/** Indicates whether an extension bundle is a built-in default provided by the library,
  * a collection of extensions installed by a markup format or user-defined.
  *
  * This is relevant for determining the precedence of installed bundles when merging
  * them, as user-supplied functionality always overrides library defaults.
  *
  * @author Jens Halm
  */
sealed trait BundleOrigin

object BundleOrigin {

  /** Indicates that the extension bundle is a built-in extension provided by Laika.
    *
    * Internally Laika uses its own extension hooks to provide non-standard
    * functionality like its directive syntax. Keeping it in extensions
    * means these features can be disabled when run in strict mode.
    */
  case object Library extends BundleOrigin

  /** Indicates that the extension bundle is provided by a parser for markup format.
    *
    * A parser may either provide a feature as a bundle to allow the user to disable
    * it, e.g. support for raw content in the output format which might be security risk
    * when entered by the user.
    *
    * Another scenario is a parser that needs to supply custom renderers as its
    * parsers produce AST nodes which are not known by the built-in renderers.
    */
  case object Parser extends BundleOrigin

  /** Indicates that the extension bundle is provided by a theme.
    *
    * A theme usually focuses on pre-populating the input tree with templates,
    * styles and configuration, but may in addition also decide to offer lower-level extensions
    * such as custom directives or extensions to markup syntax.
    */
  case object Theme extends BundleOrigin

  /** Indicates that the extension bundle has been provided by the user.
    */
  case object User extends BundleOrigin

  /** Indicates that a bundle has been merged from multiple individual bundles
    * with different origins. This would normally not occur before the final
    * step in a parse or transform operation when the final bundle is merged
    * internally from all the individual bundles.
    */
  case object Mixed extends BundleOrigin

}
