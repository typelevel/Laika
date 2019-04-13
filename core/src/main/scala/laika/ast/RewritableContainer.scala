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


case class RewriteRules (spanRules: PartialFunction[Span, RewriteAction[Span]] = PartialFunction.empty,
                         blockRules: PartialFunction[Block, RewriteAction[Block]] = PartialFunction.empty,
                         templateRules: PartialFunction[TemplateSpan, RewriteAction[TemplateSpan]] = PartialFunction.empty) {

  def rewriteSpans (spans: Seq[Span]): Option[Seq[Span]] = rewrite(spanRules, spans)
  def rewriteBlocks (blocks: Seq[Block]): Option[Seq[Block]] = rewrite(blockRules, blocks)
  def rewriteTemplateSpans (spans: Seq[TemplateSpan]): Option[Seq[TemplateSpan]] = rewrite(templateRules, spans)
  
  private def rewrite[T <: AnyRef] (childRules: PartialFunction[T, RewriteAction[T]], children: Seq[T]): Option[Seq[T]] = {

    val actions = children map {
      case rw: Rewritable[_] =>
        val newChild = rw.rewrite2(this).asInstanceOf[T]
        val action = childRules.applyOrElse[T, RewriteAction[T]](newChild, _ => Retain)
        if (action == Retain && newChild.ne(rw)) Replace(newChild)
        else action
      case child => childRules.applyOrElse[T, RewriteAction[T]](child, _ => Retain)
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

sealed trait RewriteAction[+T]
case class Replace[T] (newValue: T) extends RewriteAction[T]
case object Retain extends RewriteAction[Nothing]
case object Remove extends RewriteAction[Nothing]

trait Rewritable[Self <: Rewritable[Self]] { this: Self =>
  
  type RewriteRule[T] = PartialFunction[T, RewriteAction[T]] // TODO - 0.12 - promote to package object
  
  def rewrite2 (rules: RewriteRules): Self // TODO - 0.12 - rename after old API is gone
  
  def rewriteSpans (rules: RewriteRule[Span]): Self = rewrite2(RewriteRules(spanRules = rules))

}
