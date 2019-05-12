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

import laika.ast._
import laika.bundle.ExtensionBundle

/** API for specifying configuration options that apply to all
  * kinds of operations that contain both, a parsing and a rendering step (only Transform API).
  *
  * @author Jens Halm
  */
trait TransformConfigBuilder[FMT] extends ParseConfigBuilder with RenderConfigBuilder[FMT] {

  /**  Specifies rewrite rules to be applied to the document tree model between the
    *  parse and render operations. This is identical to calling `Document.rewrite`
    *  directly, but if there is no need to otherwise access the document instance
    *  and just chain parse and render operations this hook is more convenient.
    *
    *  The rules are partial functions of type `PartialFunction[T, RewriteRule[T]]` where `T` is
    *  either `Span`, `Block` or `TemplateSpan`, the 3 main categories of element types that support
    *  rewriting.
    *
    *  If the partial function is not defined for a specific element or the rule returns
    *  a `Retain` action as a result the old element remains in the tree unchanged. 
    *
    *  If it returns `Remove` then the element gets removed from the ast,
    *  if it returns `Replace` with a new element it will replace the old one. 
    *
    *  The rewriting is performed bottom-up (depth-first), therefore
    *  any element container passed to the rule only contains children which have already
    *  been processed.
    */
  def usingRules (newRules: RewriteRules): ThisType = creatingRule(_ => newRules)

  /**  Specifies a single block rewrite rule to be applied to the document tree model between the
    *  parse and render operations. This is identical to calling `Document.rewrite`
    *  directly, but if there is no need to otherwise access the document instance
    *  and just chain parse and render operations this hook is more convenient.
    *
    *  The rule is a type alias for a partial function of type `PartialFunction[Block, RewriteRule[Block]]`.
    *
    *  If the partial function is not defined for a specific block or the rule returns
    *  a `Retain` action as a result the old block remains in the tree unchanged. 
    *
    *  If it returns `Remove` then the block gets removed from the ast,
    *  if it returns `Replace` with a new block it will replace the old one. 
    *
    *  The rewriting is performed bottom-up (depth-first), therefore
    *  any element container passed to the rule only contains children which have already
    *  been processed.
    */
  def usingBlockRule (rule: RewriteRule[Block]): ThisType = usingRules(RewriteRules.forBlocks(rule))

  /**  Specifies a single span rewrite rule to be applied to the document tree model between the
    *  parse and render operations. This is identical to calling `Document.rewrite`
    *  directly, but if there is no need to otherwise access the document instance
    *  and just chain parse and render operations this hook is more convenient.
    *
    *  The rule is a type alias for a partial function of type `PartialFunction[Span, RewriteRule[Span]`.
    *
    *  If the partial function is not defined for a specific span or the rule returns
    *  a `Retain` action as a result the old span remains in the tree unchanged. 
    *
    *  If it returns `Remove` then the span gets removed from the ast,
    *  if it returns `Replace` with a new span it will replace the old one. 
    *
    *  The rewriting is performed bottom-up (depth-first), therefore
    *  any element container passed to the rule only contains children which have already
    *  been processed.
    */
  def usingSpanRule (rule: RewriteRule[Span]): ThisType = usingRules(RewriteRules.forSpans(rule))

  /**  Specifies a single rewrite rule for template spans to be applied to the template tree model between the
    *  parse and render operations. This is identical to calling `TemplateDocument.rewrite`
    *  directly, but if there is no need to otherwise access the document instance
    *  and just chain parse and render operations this hook is more convenient.
    *
    *  The rule is a type alias for a partial function of type `PartialFunction[TemplateSpan, RewriteRule[TemplateSpan]`.
    *
    *  If the partial function is not defined for a specific span or the rule returns
    *  a `Retain` action as a result the old span remains in the tree unchanged. 
    *
    *  If it returns `Remove` then the span gets removed from the ast,
    *  if it returns `Replace` with a new span it will replace the old one. 
    *
    *  The rewriting is performed bottom-up (depth-first), therefore
    *  any element container passed to the rule only contains children which have already
    *  been processed.
    */
  def usingTemplateRule (rule: RewriteRule[TemplateSpan]): ThisType = usingRules(RewriteRules.forTemplates(rule))

  /**  Specifies a rewrite rule to be applied to the document tree model between the
    *  parse and render operations. This is identical to calling `Document.rewrite`
    *  directly, but if there is no need to otherwise access the document instance
    *  and just chain parse and render operations this hook is more convenient.
    *
    *  The difference of this method to the `usingRules` method is that it expects a function
    *  that takes a Document instance and returns the rewrite rules. This way the full document
    *  can be queried before any rule is applied. This is necessary in cases where the rule
    *  (which gets applied node-by-node) depends on information from other nodes. An example
    *  from the built-in rewrite rules is the rule that resolves link references. To replace
    *  all link reference elements with actual link elements, the rewrite rule needs to know
    *  all LinkDefinitions the document tree contains.
    *
    *  The rules themselves are partial functions of type `PartialFunction[T, RewriteRule[T]]` where `T` is
    *  either `Span`, `Block` or `TemplateSpan`, the 3 main categories of element types that support
    *  rewriting.
    *
    *  If the partial function is not defined for a specific element or the rule returns
    *  a `Retain` action as a result the old block remains in the tree unchanged. 
    * 
    *  If it returns `Remove` then the element gets removed from the ast,
    *  if it returns `Replace` with a new element it will replace the old one. 
    * 
    *  The rewriting is performed bottom-up (depth-first), therefore
    *  any element container passed to the rule only contains children which have already
    *  been processed.
    */
  def creatingRule (newRules: DocumentCursor => RewriteRules): ThisType = using(new ExtensionBundle {
    override val useInStrictMode: Boolean = true
    override def rewriteRules: Seq[DocumentCursor => RewriteRules] = Seq(newRules)
  })

}
