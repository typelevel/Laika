/*
 * Copyright 2013 the original author or authors.
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
import laika.tree.Documents.Document 

/** The default rewrite rules that get applied to the raw document tree after parsing
 *  unless explicitly disabled. The rules are responsible for resolving references 
 *  to images, footnotes, citations and other inline targets and building a structure 
 *  of sections based on the headers in the document and their level.
 *  
 *  To disable these rules the `Parse` API needs to be used (the `Transform` API always
 *  applies them):
 *  
 *  {{{
 *  val doc = Parse as Markdown asRawDocument fromFile "hello.md"
 *  }}}
 * 
 *  @author Jens Halm
 */
object RewriteRules extends (Document => PartialFunction[Element,Option[Element]]) {

  
  /** Provides the default rewrite rules for the specified document.
   */
  def apply (document: Document) = {
    LinkResolver(document) orElse SectionBuilder()
  }
  
  
  /** Chains the specified rules into one PartialFunction composition.
   *  The resulting function is always defined, but is returned
   *  as a PartialFunction as required by the `ElementTraversal` API.
   */
  def chain (rules: Seq[PartialFunction[Element,Option[Element]]]) = {
    
    val fallback: PartialFunction[Element, Option[Element]] = { case e => Some(e) }
      
    (rules map { _ orElse fallback }) reduceRight { (ruleA,ruleB) => ruleA andThen (_ flatMap ruleB) }
  }
  
   
}