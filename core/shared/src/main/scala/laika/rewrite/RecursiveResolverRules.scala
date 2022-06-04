/*
 * Copyright 2012-2022 the original author or authors.
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

import laika.ast._

object RecursiveResolverRules {
  
  private def asTemplateSpan (span: Span): TemplateSpan = span match {
    case t: TemplateSpan => t
    case s => TemplateElement(s)
  }
  
  def applyTo (cursor: DocumentCursor, baseRules: RewriteRules): RewriteRules = {

    def rulesForScope (scope: ElementScope[_]): RewriteRules = applyTo(cursor.withReferenceContext(scope.context), baseRules)
    
    lazy val rules: RewriteRules = RewriteRules.forBlocks {
      case ph: BlockResolver => Replace(rules.rewriteBlock(ph.resolve(cursor)))
      case scope: BlockScope => Replace(rulesForScope(scope).rewriteBlock(scope.content))
    } ++ RewriteRules.forSpans {
      case ph: SpanResolver => Replace(rules.rewriteSpan(ph.resolve(cursor)))
      case scope: SpanScope => Replace(rulesForScope(scope).rewriteSpan(scope.content))
    } ++ RewriteRules.forTemplates {
      case ph: SpanResolver     => Replace(rules.rewriteTemplateSpan(asTemplateSpan(ph.resolve(cursor))))
      case scope: TemplateScope => Replace(rulesForScope(scope).rewriteTemplateSpan(scope.content))
    } ++ baseRules

    rules
  }

}
