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

package laika.api.config

import laika.api.ext.ExtensionBundle
import laika.rewrite.{DocumentCursor, RewriteRules}
import laika.tree.Documents.Document
import laika.tree.Elements.RewriteRule

/**
  * @author Jens Halm
  */
case class OperationConfig (bundles: Seq[ExtensionBundle] = Nil, parallel: Boolean = false) {

  private lazy val mergedBundle: ExtensionBundle = ExtensionBundle.mergeBundles(bundles)

  lazy val rewriteRule: DocumentCursor => RewriteRule = RewriteRules.chainFactories(mergedBundle.rewriteRules)

  def rewriteRuleFor (doc: Document): RewriteRule = rewriteRule(DocumentCursor(doc))

  def withBundles (bundles: Seq[ExtensionBundle]): OperationConfig = copy(bundles = this.bundles ++ bundles)

}

trait OperationConfigBuilder {

  type ThisType

  protected def withConfig(newConfig: OperationConfig): ThisType

  protected def config: OperationConfig

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
