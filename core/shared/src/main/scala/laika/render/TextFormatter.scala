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

package laika.render

import laika.ast.*
import laika.factory.RenderContext

/** Default factory for ASTFormatters, based on a provided RenderContext.
  */
private[laika] object ASTFormatter extends (RenderContext[Formatter] => Formatter) {

  def apply(ctx: RenderContext[Formatter]): Formatter =
    new Formatter {
      type Rep = Formatter
      protected def self: Rep                         = this
      protected def context: RenderContext[Formatter] = ctx
      protected def withChild(element: Element): Rep  = apply(ctx.forChildElement(element))

      protected def withIndentation(newIndentation: Indentation): Rep = apply(
        ctx.withIndentation(newIndentation)
      )

    }

  private[laika] def forAST(ctx: RenderContext[Formatter]): Formatter =
    apply(ctx.withIndentation(Indentation.dotted))

}
