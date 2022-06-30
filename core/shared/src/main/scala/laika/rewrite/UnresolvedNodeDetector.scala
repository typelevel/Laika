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

import laika.ast.{DocumentCursor, InvalidBlock, InvalidSpan, Replace, RewriteRules, TemplateElement, Unresolved}
import laika.ast.RewriteRules.RewriteRulesBuilder
import laika.config.Config.ConfigResult

private[laika] object UnresolvedNodeDetector extends RewriteRulesBuilder {
  
  def apply (cursor: DocumentCursor): ConfigResult[RewriteRules] = Right {
    RewriteRules.forBlocks {
      case unresolved: Unresolved => Replace(InvalidBlock(unresolved.unresolvedMessage, unresolved.source))
    } ++ RewriteRules.forSpans {
      case unresolved: Unresolved => Replace(InvalidSpan(unresolved.unresolvedMessage, unresolved.source))
    } ++ RewriteRules.forTemplates {
      case unresolved: Unresolved => Replace(TemplateElement(InvalidSpan(unresolved.unresolvedMessage, unresolved.source)))
    }
  }
  
}
