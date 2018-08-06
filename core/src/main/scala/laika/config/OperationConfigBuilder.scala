/*
 * Copyright 2013-2018 the original author or authors.
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

package laika.config

import laika.bundle.ExtensionBundle

/** Base API for specifying configuration options that apply to all
  * kinds of operations (Parse, Render and Transform).
  *
  * @author Jens Halm
  */
trait OperationConfigBuilder {

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
  def config: OperationConfig

  /** Returns a new instance with the specified extension bundles installed.
    * Features in the new bundles may override features in already installed bundles.
    *
    * Bundles are usually provided by libraries (by Laika itself or a 3rd-party extension library)
    * or as re-usable building blocks by application code.
    */
  def using (bundles: ExtensionBundle*): ThisType = withConfig(config.withBundles(bundles))

  /**  Instructs the parser and/or renderer to process all inputs and outputs in parallel.
    *  The recursive structure of document trees will be flattened before parsing and rendering
    *  and then get reassembled afterwards, therefore the parallel processing
    *  includes all subtrees of the document tree.
    *
    *  The actual transformation is a three phase process, the first (parsing) and
    *  third (rendering) can run in parallel. The second phase in the middle cannot,
    *  as this is the document tree model rewrite step where things like cross references or
    *  table of contents get processed that need access to more than just the current
    *  document.
    */
  def inParallel: ThisType = withConfig(config.copy(parallel = true))

}
