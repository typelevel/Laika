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

package laika.tree


import laika.tree.Elements.Element 
import laika.tree.Elements.RewriteRule 
import laika.tree.Documents.DocumentContext 

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
    
    val fallback: RewriteRule = { case e => Some(e) }
    
    if (rules.isEmpty) fallback  
    else (rules map { _ orElse fallback }) reduceRight { (ruleA,ruleB) => ruleA andThen (_ flatMap ruleB) }
  }
  
   
}
