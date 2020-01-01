/*
 * Copyright 2012-2020 the original author or authors.
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

package laika.rst.bundle

import laika.ast._
import laika.rst.ast.{CustomizedTextRole, InterpretedText, SubstitutionDefinition, SubstitutionReference}
import laika.rst.ext.TextRoles.TextRole

/** 
 *  The default rewrite rules that get applied to the raw document tree after parsing
 *  reStructuredText markup. These rules are responsible for resolving  substitution 
 *  references and interpreted text which are specific to reStructuredText and get usually
 *  executed alongside the generic rules. .
 * 
 *  @author Jens Halm
 */
class RewriteRules (textRoles: Seq[TextRole]) extends (DocumentCursor => laika.ast.RewriteRules) {

  val baseRoleElements: Map[String, String => Span] = textRoles map { role => (role.name, role.default) } toMap

  class DefaultRules (cursor: DocumentCursor) {

    val substitutions: Map[String, Span] = cursor.target.content.collect { 
      case SubstitutionDefinition(id,content,_) => (id,content) 
    } toMap
    
    val textRoles: Map[String, String => Span] = (cursor.target.content.collect {
      case CustomizedTextRole(id,f,_) => (id,f)                                   
    } toMap) ++ baseRoleElements

    def invalidSpan (message: String, fallback: String): InvalidSpan = InvalidElement(message, fallback).asSpan
      
    /** Function providing the default rewrite rules when passed a document instance.
     */
    val rewrite: laika.ast.RewriteRules = laika.ast.RewriteRules.forSpans {
          
      case SubstitutionReference(id, _) =>
        Replace(substitutions.getOrElse(id, invalidSpan(s"unknown substitution id: $id", s"|$id|")))
        
      case InterpretedText(role, text, _, _) =>
        Replace(textRoles.get(role).fold[Span](invalidSpan(s"unknown text role: $role", s"`$text`"))(_(text)))
        
    }
  }

  /** Provides the default rewrite rules specific to reStructuredText 
   *  for the specified document. These rules usually get executed
   *  alongside the generic default rules.
   */
  def apply (cursor: DocumentCursor): laika.ast.RewriteRules = new DefaultRules(cursor).rewrite
  
  
}
