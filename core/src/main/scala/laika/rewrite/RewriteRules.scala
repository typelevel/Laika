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

package laika.rewrite

import laika.factory.ParserFactory
import laika.tree.Elements.Element
import laika.tree.Elements.RewriteRule

/** Utilities for dealing with rewrite rules.
 * 
 *  @author Jens Halm
 */
object RewriteRules {

  
  /** Chains the specified rules into one PartialFunction composition.
   *  The resulting function is always defined, but is returned
   *  as a PartialFunction as required by the `ElementTraversal` API.
   */
  def chain (rules: Seq[RewriteRule]): RewriteRule = {
    
    def extractRules (rule: RewriteRule): Seq[RewriteRule] = rule match {
      case ChainedRewriteRules(rules) => rules
      case other => Seq(other)
    }
    
    ChainedRewriteRules(rules.flatMap(extractRules))
  }
  
  private case class ChainedRewriteRules (rules: Seq[RewriteRule]) extends RewriteRule {
    
    def apply (element: Element): Option[Element] = {
      
      val fallback: RewriteRule = { case e => Some(e) }
    
      val f = 
        if (rules.isEmpty) fallback  
        else (rules map { _ orElse fallback }) reduceRight { (ruleA,ruleB) => ruleA andThen (_ flatMap ruleB) }
      
      f(element)
    }
      
    def isDefinedAt (element: Element): Boolean = true
    
  }
  
  
  private val defaultsFactories = Seq(LinkResolver, SectionBuilder)

  /** Chains the specified rule factory functions into a single factory function.
    */
  def chainFactories (rules: Seq[DocumentCursor => RewriteRule]): DocumentCursor => RewriteRule = 
    cursor => chain(rules map (_(cursor)))

  /** The default Laika rewrite rules common to all supported markup languages,
    * dealing with resolving links and building the section structure of a document.
    */
  def defaults: DocumentCursor => RewriteRule = chainFactories(defaultsFactories)

  /** Combines the default Laika rewrite rules with the ones provided by the
    * specified parser factory implementations into a single rule.
    */
  def defaultsFor (parsers: ParserFactory*): DocumentCursor => RewriteRule = 
    chainFactories(parsers.flatMap(_.rewriteRules) ++ defaultsFactories)
   
    
}
