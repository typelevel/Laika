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

package laika.parse.rst

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import laika.tree.Elements._
import laika.parse.rst.Elements._
import scala.annotation.tailrec
import scala.collection.immutable.TreeSet

/** 
 *  The default rewrite rules that get applied to the raw document tree after parsing
 *  reStructuredTextMarkup. The rules are responsible for resolving internal references
 *  to link targets, footnotes, citations, substitution definitions and text roles.
 * 
 *  These rules are specific to `reStructuredText`, but some of them might get promoted
 *  to the general rules implemented in [[laika.tree.RewriteRules]] in a later release.
 *  
 *  @author Jens Halm
 */
object RewriteRules {

  
  private def invalidSpan (message: String, fallback: String) =
      InvalidSpan(SystemMessage(laika.tree.Elements.Error, message), Text(fallback))
      
  private def selectSubstitutions (document: Document) = document.collect { 
      case SubstitutionDefinition(id,content,_) => (id,content) 
    } toMap
    
  private def selectTextRoles (document: Document) = document.collect { 
      case CustomizedTextRole(id,f,_) => (id,f)                                   
    } toMap  
  
  /** Function providing the default rewrite rules when passed a document instance.
   */
  val defaults: Document => PartialFunction[Element, Option[Element]] = { document =>
    
    val substitutions = selectSubstitutions(document)
    
    val textRoles = selectTextRoles(document)
    
    val levelMap = scala.collection.mutable.Map.empty[(Char,Boolean),Int]
    val levelIt = Stream.from(1).iterator
    
    val pf: PartialFunction[Element, Option[Element]] = {
      case SectionHeader(char, overline, content, _) => 
        Some(Header(levelMap.getOrElseUpdate((char,overline), levelIt.next), content))
        
      case SubstitutionReference(id,_) =>
        substitutions.get(id).orElse(Some(invalidSpan("unknown substitution id: " + id, "|"+id+"|")))
        
      case InterpretedText(role,text,_,_) =>
        textRoles.get(role).orElse(Some({s: String => invalidSpan("unknown text role: " + role, "`"+s+"`")})).map(_(text))
        
      case SubstitutionDefinition(_,_,_) => None // TODO - should be covered by default rules
      case CustomizedTextRole(_,_,_) => None
    }
    pf
  }

  /** Applies the default rewrite rules to the specified document tree,
   *  returning a new rewritten tree instance.
   */
  def applyDefaults (doc: Document) = doc.rewrite(defaults(doc)) 
   
  
}