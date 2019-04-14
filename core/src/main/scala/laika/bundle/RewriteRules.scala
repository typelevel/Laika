/*
 * Copyright 2013-2016 the original author or authors.
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

package laika.bundle

import laika.ast._
import laika.rewrite.link.LinkResolver
import laika.rewrite.nav.SectionBuilder

import scala.annotation.tailrec

/** Utilities for dealing with rewrite rules.
 * 
 *  @author Jens Halm
 */
object RewriteRules {

  case class ChainedRewriteRules[T] (rules: Seq[RewriteRule[T]]) extends (T => RewriteAction[T]) {

    def apply (element: T): RewriteAction[T] = {
      
      @tailrec
      def applyNextRule (currentAction: RewriteAction[T], remainingRules: Seq[RewriteRule[T]]): RewriteAction[T] =
        if (currentAction == Remove || remainingRules.isEmpty) currentAction
        else {
          val input = currentAction match {
            case Replace(elem) => elem
            case _ => element
          }
          val action = remainingRules.head.applyOrElse[T, RewriteAction[T]](input, _ => currentAction)
          applyNextRule(action, remainingRules.tail)
        }
      
      applyNextRule(Retain, rules)
    }
    
  }

  /** Chains the specified rule factory functions into a single factory function.
    */
  def chainFactories (rules: Seq[DocumentCursor => RewriteRules]): DocumentCursor => RewriteRules =
    cursor => rules.map(_(cursor)).reduce(_ ++ _) // TODO - 0.12 - verify rules is always non-empty or use Nel

  /** The default built-in rewrite rules, dealing with section building and link resolution.
    * These are not installed as part of any default extension bundle as they have specific
    * ordering requirements not compatible with the standard bundle ordering in `OperationConfig`.
    */
  val defaults = Seq(LinkResolver, SectionBuilder)

}
