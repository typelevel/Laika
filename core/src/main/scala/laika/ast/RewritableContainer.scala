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


trait RewriteRules {
  def spanRules: PartialFunction[Span, RewriteAction[Span]]
  def blockRules: PartialFunction[Block, RewriteAction[Block]]
}

sealed trait RewriteAction[+T]
case class Replace[T] (newValue: T) extends RewriteAction[T]
case object Retain extends RewriteAction[Nothing]
case object Remove extends RewriteAction[Nothing]

trait Rewritable[Self <: Rewritable[Self]] {
  
  type RewriteRule[T] = PartialFunction[T, RewriteAction[T]]
  
  def rewrite2 (rules: RewriteRules): Self // TODO - 0.12 - rename after old API is gone
  
  def rewriteSpans (rules: RewriteRule[Span]): Self = rewrite2(new RewriteRules {
    override def spanRules: PartialFunction[Span, RewriteAction[Span]] = rules
    override def blockRules: PartialFunction[Block, RewriteAction[Block]] = PartialFunction.empty
  })
  
}

trait RewritableContainer[E <: AnyRef, Self <: RewritableContainer[E, Self]] extends Rewritable[Self] {

  def content: Seq[E] // TODO - 0.12 - should extend a Container trait
  
  def rewrite2 (rules: RewriteRules): Self = {
    val contentRules = rulesForContent(rules)
    
    val actions = content map {
      case rw: RewritableContainer[_, _] => 
        val newChild = rw.rewrite2(rules).asInstanceOf[E]
        val action = contentRules.applyOrElse[E, RewriteAction[E]](newChild, _ => Retain)
        if (action == Retain && newChild.ne(rw)) Replace(newChild)
        else action
      case child => contentRules.applyOrElse[E, RewriteAction[E]](child, _ => Retain)
    }
    
    if (actions.forall(_ == Retain)) this.asInstanceOf[Self]
    else {
      val newContent = content.zip(actions) flatMap {
        case (element, Retain) => Some(element)
        case (_, Remove) => None
        case (_, Replace(e)) => Some(e)
      }
      withContent(newContent)
    }
  }
  
  protected def rulesForContent (rules: RewriteRules): RewriteRule[E]
  
  protected def withContent (newContent: Seq[E]): Self
  
}
