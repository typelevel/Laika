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
import laika.api.ext.ExtensionBundle.LaikaDefaults
import laika.directive.{DirectiveSupport, StandardDirectives}
import laika.io.DocumentType
import laika.io.DocumentType.Ignored
import laika.rewrite.{DocumentCursor, RewriteRules}
import laika.tree.Documents.Document
import laika.tree.Elements.RewriteRule
import laika.tree.Paths.Path

/**
  * @author Jens Halm
  */
case class OperationConfig (bundles: Seq[ExtensionBundle] = Nil, parallel: Boolean = false) {

  private lazy val mergedBundle: ExtensionBundle = ExtensionBundle.mergeBundles(bundles)

  lazy val docTypeMatcher: Path => DocumentType = mergedBundle.docTypeMatcher.lift.andThen(_.getOrElse(Ignored))

  lazy val rewriteRule: DocumentCursor => RewriteRule = RewriteRules.chainFactories(mergedBundle.rewriteRules)

  def rewriteRuleFor (doc: Document): RewriteRule = rewriteRule(DocumentCursor(doc))

  def withBundles (bundles: Seq[ExtensionBundle]): OperationConfig = copy(bundles = this.bundles ++ bundles)

}

object OperationConfig {

  val default: OperationConfig = OperationConfig(
    bundles = Seq(LaikaDefaults, DirectiveSupport, StandardDirectives)
  )

}