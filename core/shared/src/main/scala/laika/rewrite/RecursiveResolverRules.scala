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

import laika.ast.{BlockResolver, DocumentCursor, NavigationList, Replace, RewriteRules, Span, SpanResolver, TemplateElement, TemplateSpan}

object RecursiveResolverRules {
  
  private def asTemplateSpan (span: Span) = span match {
    case t: TemplateSpan => t
    case s => TemplateElement(s)
  }
  
  def applyTo (cursor: DocumentCursor, baseRules: RewriteRules): RewriteRules = {
    lazy val rules: RewriteRules = RewriteRules.forBlocks {
      case ph: BlockResolver => Replace(rules.rewriteBlock(ph.resolve(cursor)))
    } ++ RewriteRules.forSpans {
      case ph: SpanResolver => Replace(rules.rewriteSpan(ph.resolve(cursor)))
    } ++ RewriteRules.forTemplates {
      case ph: SpanResolver => Replace(rules.rewriteTemplateSpan(asTemplateSpan(ph.resolve(cursor))))
    } ++ baseRules

    rules
  }

}
