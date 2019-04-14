/*
 * Copyright 2012-2019 the original author or authors.
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

package laika.ast

import laika.bundle.RewriteRules.ChainedRewriteRules


case class RewriteRules (spanRules: Seq[RewriteRule[Span]] = Nil,
                         blockRules: Seq[RewriteRule[Block]] = Nil,
                         templateRules: Seq[RewriteRule[TemplateSpan]] = Nil) {

  lazy val chainedSpanRules: Span => RewriteAction[Span] = ChainedRewriteRules(spanRules)
  lazy val chainedBlockRules: Block => RewriteAction[Block] = ChainedRewriteRules(blockRules)
  lazy val chainedTemplateRules: TemplateSpan => RewriteAction[TemplateSpan] = ChainedRewriteRules(templateRules)
  
  def ++ (other: RewriteRules): RewriteRules =
    RewriteRules(spanRules ++ other.spanRules, blockRules ++ other.blockRules, templateRules ++ other.templateRules)

  def rewriteSpan (span: Span): Span = rewriteSpans(Seq(span)).fold(span)(_.headOption.getOrElse(SpanSequence(Nil)))
  def rewriteBlock (block: Block): Block = rewriteBlocks(Seq(block)).fold(block)(_.headOption.getOrElse(BlockSequence(Nil)))
  def rewriteTemplateSpan (span: TemplateSpan): TemplateSpan = rewriteTemplateSpans(Seq(span)).fold(span)(_.headOption.getOrElse(TemplateSpanSequence(Nil)))
  
  def rewriteSpans (spans: Seq[Span]): Option[Seq[Span]] = rewrite(chainedSpanRules, spans)
  def rewriteBlocks (blocks: Seq[Block]): Option[Seq[Block]] = rewrite(chainedBlockRules, blocks)
  def rewriteTemplateSpans (spans: Seq[TemplateSpan]): Option[Seq[TemplateSpan]] = rewrite(chainedTemplateRules, spans)

  /** Returns a new, rewritten tree model based on the specified rule.
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
  private def rewrite[T <: AnyRef] (childRules: T => RewriteAction[T], children: Seq[T]): Option[Seq[T]] = {

    val actions = children map {
      case rw: RewritableContainer[_] =>
        val newChild = rw.rewriteChildren(this).asInstanceOf[T]
        val action = childRules(newChild)
        if (action == Retain && newChild.ne(rw)) Replace(newChild)
        else action
      case child => childRules(child)
    }

    if (actions.forall(_ == Retain)) None
    else {
      val newContent = children.zip(actions) flatMap {
        case (element, Retain) => Some(element)
        case (_, Remove) => None
        case (_, Replace(e)) => Some(e)
      }
      Some(newContent)
    }
  }
  
}

object RewriteRules {
  def forSpans (rule: RewriteRule[Span]): RewriteRules = RewriteRules(spanRules = Seq(rule))
  def forBlocks (rule: RewriteRule[Block]): RewriteRules = RewriteRules(blockRules = Seq(rule))
  def forTemplates (rule: RewriteRule[TemplateSpan]): RewriteRules = RewriteRules(templateRules = Seq(rule))
}

sealed trait RewriteAction[+T]
case class Replace[T] (newValue: T) extends RewriteAction[T]
case object Retain extends RewriteAction[Nothing]
case object Remove extends RewriteAction[Nothing]

trait RewritableContainer[Self <: RewritableContainer[Self]] { this: Self =>
  
  def rewriteChildren (rules: RewriteRules): Self
  
  def rewriteSpans (rules: RewriteRule[Span]): Self = rewriteChildren(RewriteRules(spanRules = Seq(rules)))

}
