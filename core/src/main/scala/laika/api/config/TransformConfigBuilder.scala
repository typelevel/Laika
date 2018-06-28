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
import laika.rewrite.DocumentCursor
import laika.tree.Elements.RewriteRule

/**
  * @author Jens Halm
  */
trait TransformConfigBuilder[Writer] extends ParseConfigBuilder with RenderConfigBuilder[Writer] {

  /** Specifies a rewrite rule to be applied to the document tree model between the
    *  parse and render operations. This is identical to calling `Document.rewrite`
    *  directly, but if there is no need to otherwise access the document instance
    *  and just chain parse and render operations this hook is more convenient.
    *
    *  The rule is a partial function that takes an `Element` and returns an `Option[Element]`.
    *
    *  If the function is not defined for a specific element the old element remains
    *  in the tree unchanged. If it returns `None` then the node gets removed from the tree,
    *  if it returns an element it will replace the old one. Of course the function may
    *  also return the old element.
    *
    *  The rewriting is performed in a way that only branches of the tree that contain
    *  new or removed elements will be replaced. It is processed bottom-up, therefore
    *  any element container passed to the rule only contains children which have already
    *  been processed.
    *
    *  In case multiple rewrite rules need to be applied it may be more efficient to
    *  first combine them with `orElse`.
    */
  def usingRule (newRule: RewriteRule): ThisType = creatingRule(_ => newRule)

  /** Specifies a rewrite rule to be applied to the document tree model between the
    *  parse and render operations. This is identical to calling `Document.rewrite`
    *  directly, but if there is no need to otherwise access the document instance
    *  and just chain parse and render operations this hook is more convenient.
    *
    *  The difference of this method to the `usingRule` method is that it expects a function
    *  that expects a Document instance and returns the rewrite rule. This way the full document
    *  can be queried before any rule is applied. This is necessary in cases where the rule
    *  (which gets applied node-by-node) depends on information from other nodes. An example
    *  from the built-in rewrite rules is the rule that resolves link references. To replace
    *  all link reference elements with actual link elements, the rewrite rule needs to know
    *  all LinkDefinitions the document tree contains.
    *
    *  The rule itself is a partial function that takes an `Element` and returns an `Option[Element]`.
    *
    *  If the function is not defined for a specific element the old element remains
    *  in the tree unchanged. If it returns `None` then the node gets removed from the tree,
    *  if it returns an element it will replace the old one. Of course the function may
    *  also return the old element.
    *
    *  The rewriting is performed in a way that only branches of the tree that contain
    *  new or removed elements will be replaced. It is processed bottom-up, therefore
    *  any element container passed to the rule only contains children which have already
    *  been processed.
    *
    *  In case multiple rewrite rules need to be applied it may be more efficient to
    *  first combine them with `orElse`.
    */
  def creatingRule (newRule: DocumentCursor => RewriteRule): ThisType = using(new ExtensionBundle {
    override val useInStrictMode: Boolean = true
    override def rewriteRules: Seq[DocumentCursor => RewriteRule] = Seq(newRule)
  })

}
