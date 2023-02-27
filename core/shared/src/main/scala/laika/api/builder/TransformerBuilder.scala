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

package laika.api.builder

import laika.api.{ MarkupParser, Transformer }
import laika.ast.DocumentType.Markup
import laika.ast._
import laika.factory.{ MarkupFormat, RenderFormat }

/** Builder API for Transformer instances.
  *
  * Allows to add ExtensionBundles, to register AST rewrite rules,
  * to override the renderer for specific elements and other options.
  *
  * @tparam FMT the formatter API to use which varies depending on the renderer
  *
  * @author Jens Halm
  */
class TransformerBuilder[FMT](
    markupFormat: MarkupFormat,
    protected val renderFormat: RenderFormat[FMT],
    protected val config: OperationConfig
) extends TransformerBuilderOps[FMT] {

  type ThisType = TransformerBuilder[FMT]

  val docType: TextDocumentType = Markup

  def withConfig(newConfig: OperationConfig): ThisType =
    new TransformerBuilder(markupFormat, renderFormat, newConfig)

  /** Applies all configuration specified with this builder
    * and returns a new Transformer instance.
    */
  def build: Transformer = {
    val parser   = new MarkupParser(markupFormat, config)
    val renderer = new RendererBuilder[FMT](renderFormat, config).build
    new Transformer(parser, renderer)
  }

}
