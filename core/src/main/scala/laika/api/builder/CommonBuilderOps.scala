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

import laika.bundle.ExtensionBundle

/** Base API for specifying configuration options that apply to all
  * kinds of operations (Parser, Renderer and Transformer).
  *
  * @author Jens Halm
  */
trait CommonBuilderOps {

  /** The type of the operation being configured by this instance
    */
  type ThisType

  /** Returns a new instance with the specified configuration.
    *
    * This method discards any previously specified
    * options. It is usually meant to be used when copying over
    * the configuration from a fully configured object to an
    * unconfigured one.
    */
  def withConfig (newConfig: OperationConfig): ThisType

  /** The current configuration for this instance.
    */
  protected def config: OperationConfig

  /** Returns a new instance with the specified extension bundles installed.
    * Features in the new bundles may override features in already installed bundles.
    *
    * Bundles are usually provided by libraries (by Laika itself or a 3rd-party extension library)
    * or as re-usable building blocks by application code.
    */
  def using (bundles: ExtensionBundle*): ThisType = withConfig(config.withBundles(bundles))

}
