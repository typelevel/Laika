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

import cats.effect.{Resource, Sync}

/** Responsible for building a theme resource with the user-provided effect type and runtime configuration.
  * 
  * Implementations of this trait can be passed to the `withTheme` method of the parser, renderer and transformer
  * APIs of the `laika-io` module.
  * Theme authors would usually offer a theme-specific configuration API with a final `build` method
  * that provides a `ThemeProvider` for the user.
  * 
  * @author Jens Halm
  */
trait ThemeProvider {

  /** Builds the theme resource with the user-provided effect type and runtime configuration.
    * 
    * For convenience, implementations of this method usually utilize a [[laika.theme.ThemeBuilder]] to construct
    * `Theme` instances, but this is not mandatory.
    */
  def build[F[_]: Sync]: Resource[F, Theme[F]]
  
}
